{-# LANGUAGE TemplateHaskell #-}

module TypeCheck
  where

import Control.Monad.Trans.State.Lazy
import Control.Monad.Except
import Control.Monad.Trans.Class (lift)
import Control.Lens
import Data.Maybe (isJust)
import Control.Monad (when)

import qualified Data.Map as Map
import qualified Data.Set as Set

import AST
import Parser (parseLang)

data SymbolTable = SymbolTable {
  _depSorts      :: Set.Set SortName
, _simpleSorts   :: Set.Set SortName
, _funSyms       :: Map.Map Name FunctionalSymbol
, _axioms        :: Map.Map Name Axiom
, _reductions    :: Map.Map Name Reduction
, _iSymAxiomMap  :: Map.Map Name Name -- intro axioms of funSyms
-- , _reductions    :: Map.Map Name
} deriving (Eq, Show)

makeLenses ''SymbolTable

type TypeCheckM = StateT SymbolTable (Either TypeError)
type TypeError = String
type MetaCtx = [(MetaVar, Sort)]
type Ctx = [VarName]

varsInit :: SymbolTable
varsInit = SymbolTable Set.empty Set.empty Map.empty Map.empty Map.empty Map.empty

typecheck :: LangSpec -> TypeCheckM ()
typecheck lsp = do
    typecheckSorts lsp
    typecheckFunSyms (AST.funSyms lsp)
    typecheckAxioms (AST.axioms lsp)
    --typecheckReductions

runTypecheck :: Either String LangSpec -> Either TypeError SymbolTable
runTypecheck langSp = do
  lsp' <- langSp
  execStateT (typecheck lsp') varsInit

--------------------------------------------------------------------------------
-- Sorts

-- SortName or VarName
-- Throws Error when there are duplicatese in a list of names
checkForDups :: String -> [Name] -> Either TypeError (Set.Set Name)
checkForDups msg lst = do
  let deps = Set.fromList lst
  when (length lst /= Set.size deps) $ throwError msg
  return deps

-- Checks for duplicates, intersections and sets the sorts
typecheckSorts :: LangSpec -> TypeCheckM ()
typecheckSorts lsp = do
  deps <- lift . checkForDups "Duplicates in sorts" $ AST.depSortNames lsp
  sims <- lift . checkForDups "Duplicates in sorts" $ AST.simpleSortNames lsp
  when (Set.size (Set.intersection sims deps) /= 0) $ throwError "Dependent and simple sorts can't intersect"
  modify $ set depSorts deps
  modify $ set simpleSorts sims
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- FunSyms

-- typecheck and populate the state with funsyms
-- (sorts of func return types may need modification - we do it here)
typecheckFunSyms :: [FunctionalSymbol] -> TypeCheckM ()
typecheckFunSyms [] = return ()
typecheckFunSyms (fs : fss) = do
  fs' <- checkFun fs
  modify $ over TypeCheck.funSyms (Map.insert (nameFun fs') fs')
  typecheckFunSyms fss

-- Checks func redefinition, checks depsorts and simplesorts
-- Adds the return sort
checkFun :: FunctionalSymbol -> TypeCheckM FunctionalSymbol
checkFun fs@(FunSym name args res) = do
  st <- get

  when (isJust $ Map.lookup name (st^.TypeCheck.funSyms)) $
    throwError $ "Function redefinition " ++ name

  -- Adding the type knowledge of the result here
  let fs' = if Set.member (getSortName res) (st^.simpleSorts)
              then fs
              else FunSym name args (DepSort (getSortName res) 0)

  -- filters args by f, checks if they are all in the set
  let isIn f set = Set.size (Set.difference (Set.fromList (map getSortName (filter f args))) set) == 0

  unless (isIn isDepSort (st^.depSorts)) $
    throwError $ show name ++ " functional symbol's dependent sorts are not completely defined"
  unless (isIn (not . isDepSort) (st^.simpleSorts)) $
    throwError $ show name ++ " functional symbol's simple sorts are not completely defined"

  return fs'
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Axioms

typecheckAxioms :: [Axiom] -> TypeCheckM ()
typecheckAxioms [] = return ()
typecheckAxioms (ax : axs) = do
  ax' <- checkAx ax
  modify $ over TypeCheck.axioms (Map.insert (nameAx ax') ax')

  -- check there is only one funSym intro axiom
  -- can't have equalities in the conclusion
  funSym <- getAxFunSym ax'
  st <- get
  when (isJust $ Map.lookup funSym (st^.TypeCheck.iSymAxiomMap)) $
    throwError $ "There is already an intro axiom for " ++ funSym
  modify $ over TypeCheck.iSymAxiomMap (Map.insert funSym (nameAx ax'))
  typecheckAxioms axs

-- could be less monadic, but it's easier to throw errors this way
-- statements are always funSym intros
getAxFunSym :: Axiom -> TypeCheckM Name
getAxFunSym (Axiom _ _ _ (Statement _ (FunApp name tms) _)) = do
  checkArgsAreMetaVars tms
  return name
  where
    checkArgsAreMetaVars :: [Term] -> TypeCheckM ()
    checkArgsAreMetaVars [] = return ()
    checkArgsAreMetaVars (Var _ : xs) = checkArgsAreMetaVars xs
    checkArgsAreMetaVars (TermInCtx _ (Var _) : xs) = checkArgsAreMetaVars xs
    checkArgsAreMetaVars _ = throwError $ "Not all terms in " ++ name ++ " are metavars"

getAxFunSym (Axiom _ _ _ Statement {}) =
  throwError "Implementation bug, should have FunApp here(?)"
getAxFunSym _ = throwError "Implementation bug, cannot have equality judgement in conclusion"

-- need to check forall var types and change them if need be
-- check redefinition, fix forallvars, check types inside each judgement
checkAx :: Axiom -> TypeCheckM Axiom
checkAx ax@(Axiom name forall prem concl) = do
  st <- get

  when (isJust $ Map.lookup name (st^.TypeCheck.axioms)) $
    throwError $ "Axiom redefinition: " ++ name

  when (isEqJudgement concl) $
    throwError $ "Equality is not allowed in the conclusion of typing rules: " ++ name ++ "\nUse reductions"

  -- unless (isFunSym tm) $
  --   throwError $ "Statements must define fun syms\n" ++ show st

  forall' <- checkForallVars forall
  mapM_ (checkJudgem forall') prem
  checkJudgem forall' concl

  return (Axiom name forall' prem concl)

-- This function looks up a sortName in state
-- ContextDepth is needed for forming the sort (not lookup)
checkSortByName :: ContextDepth -> SortName -> TypeCheckM Sort
checkSortByName depth name = do
  st <- get
  if Set.member name (st^.simpleSorts)
    then
      if depth == 0
        then return (SimpleSort name)
      else throwError $ "Independent sort " ++ name ++ " can't have non-empty context"
    else
      if Set.member name (st^.depSorts)
        then return (DepSort name depth)
      else
        throwError $ "Sort " ++ name ++ " is not defined"

-- checks and modifies one vars and checks for dups
checkForallVars :: MetaCtx -> TypeCheckM MetaCtx
checkForallVars forall = do
  -- changes the sort to appropriate depth (if it's dependent at all)
  forall' <- mapM (\ (a , b) -> do
    b' <- checkSortByName (length $ mContext a) (getSortName b)
    return (a , b') ) forall
  -- check for dups in captures and x.x situations
  mapM_ (\ (a , _) -> lift . checkForDups "Duplicates in captures" $ mName a : mContext a) forall'
  lift . checkForDups "Duplicates in metas" $ map (mName . fst) forall'

  return forall'


-- given meta vars (forall) and a judgement - typechecks it
-- first checks context
-- !!!then does all the different judgement specific ops
checkJudgem :: MetaCtx -> Judgement -> TypeCheckM ()
checkJudgem meta st = do
  let ctx = jContext st
  vars <- checkCtx meta ctx
  checkJSpecific meta vars st

-- for now only Statements are checked
-- Statement - check "tm : ty"
checkJSpecific :: MetaCtx -> Ctx -> Judgement -> TypeCheckM ()
checkJSpecific meta ctx (Statement _ tm (Just ty)) = do
  tmSort <- checkTerm meta ctx tm
  tySort <- checkTerm meta ctx ty
  checkTmSort tmSort tm
  checkTySort tySort ty
checkJSpecific meta ctx (Statement _ tm Nothing) = do
  checkTerm meta ctx tm
  return ()
checkJSpecific meta ctx a@Equality{} = __checkEqAndRed meta ctx a
checkJSpecific meta ctx r@Reduct{} = do
  -- reduct specific stuff
  __checkEqAndRed meta ctx r

__checkEqAndRed :: MetaCtx -> Ctx -> Judgement -> TypeCheckM ()
__checkEqAndRed meta ctx judg = do
  lSort <- checkTerm meta ctx (jLeft judg)
  rSort <- checkTerm meta ctx (jRight judg)
  checkEqSorts (getSortName lSort) (getSortName rSort) $
    "Sorts are unequal in" ++ show judg
  case jType judg of
    Nothing -> return ()
    Just ty -> do
      checkTmSort lSort (jLeft judg)
      tySort <- checkTerm meta ctx ty
      checkTySort tySort ty

--------------------------------------------------------------------
-- given a sort checks if it's equal to universal tm sort
checkTmSort :: Sort -> Term -> TypeCheckM ()
checkTmSort tmSort tm =
  let sName = getSortName tmSort in
  checkEqSorts sName tmName $ "Left of : is not a term, but " ++ show sName ++
                               "\n in " ++ show tm

checkTySort :: Sort -> Term -> TypeCheckM ()
checkTySort tySort tm =
  let sName = getSortName tySort in
  checkEqSorts sName tyName $ "Right of : is not a type, but " ++ show sName ++
                                "\n in " ++ show tm

checkEqSorts :: SortName -> SortName -> String -> TypeCheckM ()
checkEqSorts l r msg =  when (l /= r) $ throwError msg

--------------------------------------------------------------------
checkCtx :: MetaCtx -> [(VarName, Term)] -> TypeCheckM Ctx
checkCtx mCtx = checkCtxVarsHelper mCtx []
  where
    checkCtxVarsHelper :: MetaCtx -> Ctx -> [(VarName, Term)] -> TypeCheckM Ctx
    checkCtxVarsHelper _ ctx [] = return ctx
    checkCtxVarsHelper mCtx ctx ((vname, tm):xs) = do
      tySort <- checkTerm mCtx ctx tm
      checkTySort tySort tm

      -- check if it's in metas we have it fixed
      -- !!(this is here and not just
      --         case on "checkTerm mCtx ctx (Var vname)" -- same lookup is inside there!
      --         cause I forgot how destructure it)
      case lookupName (AST.mName . fst) vname mCtx of
        Right _ -> do
          tmSort <- checkTerm mCtx ctx (Var vname)
          checkTmSort tmSort (Var vname)
          checkCtxVarsHelper mCtx ctx xs
      -- ELSE it's a variable
        Left _ -> do
          lift $
            lookupName' (\x name -> name `elem` AST.mContext (fst x))
                        vname
                        mCtx
          checkCtxVarsHelper mCtx (vname : ctx) xs

-- Given a context + forall. (The sort of the term was checked)
-- ??Not all high level terms have to be sort checked (only statements)
checkTerm :: MetaCtx -> Ctx -> Term -> TypeCheckM Sort
-- проверить все аппы на корректность сортов
checkTerm meta ctx (Var name) = do
      -- if name `elem` ctx
      --   then return varSort -- is this bullshit?
      --   else do
    -- so we're a metavar: check we have all we need in ctx and return our sort
    (mVar, sort) <- lift (lookupName (AST.mName . fst) name meta)
    unless (subset (mContext mVar) ctx) $
      throwError $ "Not all vars of a metavar are in context! Have:\n\t" ++
        show ctx ++ "\nNeed:\n\t" ++ show (mContext mVar)
    return sort
checkTerm meta ctx (TermInCtx vars tm) = do
  unless (allUnique $ vars ++ ctx) $
    throwError $ "Added vars that shadow other vars in ctx:\n" ++ show ctx ++ show vars
  checkTerm meta (vars ++ ctx) tm

checkTerm meta ctx (FunApp f args) = do
  st <- get
  case Map.lookup f (st^.TypeCheck.funSyms) of
    Nothing -> throwError $ "Undefined funSym" ++ f
    Just (FunSym _ needS res) -> do
      haveS <- mapM (checkTerm meta ctx) args
      unless (all (uncurry (==)) (zip needS haveS)) $
        throwError $ "Arg sorts don't match, need:\n\t" ++ show needS ++
          "\nbut have:\n\t" ++ show haveS
      return res

checkTerm meta ctx (Subst v@(Var name) varName what) = do -- where must! be a metavar
  -- we get: checking of compatibility of varName and v for free,
  -- also that v has all its' context and that it's a MetaVar
  sorte <- checkTerm meta ctx (TermInCtx [varName] v)
  -- check that the sort of what is tm
  whatSort <- checkTerm meta ctx what
  if whatSort /= varSort
    then throwError "Can't subst non term sort!"
    else lift (lowerCtx sorte)
checkTerm meta ctx Subst{} = throwError "May substitute only into metavars"

-- old subst check
-- -- we check that what is a tm
-- (a, b) <- lift (lookupName (AST.mName . fst) name meta)
-- -- we check that out x in T[x:=term] is in our metavars context
-- unless (varName `elem` mContext a) $
--   throwError "Variable substituted has to be in context"
-- -- we also check that this var isn't in Judgements context
-- when (varName `elem` ctx) $
--   throwError "There shouldn't be naming conflicts during subst"

--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Reductions
typecheckReductions :: TypeCheckM ()
typecheckReductions = return ()


--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Main

-- State = Set DepVars, Set Vars, Map funcs,

mainCheck :: FilePath -> IO ()
mainCheck file = do
  str <- readFile file
  let lang = parseLang (show file) str
  putStrLn $ case runTypecheck lang of
    Left err -> "hmm " ++ err
    x -> show x

mainParse :: FilePath -> IO ()
mainParse file = do
  str <- readFile file
  let k = parseLang (show file) str
  case k of
    Right x -> putStr $ show x
    Left x -> putStr x






---
