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
, _iSymAxiomMap  :: Map.Map Name Name -- intro axioms of funSyms
-- , _reductions    :: Map.Map Name
} deriving (Eq, Show)

makeLenses ''SymbolTable

type TypeCheckM = StateT SymbolTable (Either TypeError)
type TypeError = String
type Context = [(MetaVar, Sort)]

varsInit :: SymbolTable
varsInit = SymbolTable Set.empty Set.empty Map.empty Map.empty Map.empty

typecheck :: LangSpec -> TypeCheckM ()
typecheck lsp = do
    typecheckSorts lsp
    typecheckFunSyms (AST.funSyms lsp)
    typecheckAxioms (AST.axioms lsp)
    typecheckReductions

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
  funSym <- getAxFunSym ax'
  st <- get
  when (isJust $ Map.lookup funSym (st^.TypeCheck.iSymAxiomMap)) $
    throwError $ "There is already an intro axiom for " ++ funSym
  modify $ over TypeCheck.iSymAxiomMap (Map.insert funSym (nameAx ax'))

  typecheckAxioms axs

-- could be less monadic, but it's easier to throw errors this way
getAxFunSym :: Axiom -> TypeCheckM Name
getAxFunSym (Axiom _ _ _ (Statement _ (FunApp name _) _)) = return name
getAxFunSym (Axiom _ _ _ Statement {}) =
  throwError "Implementation bug, should have FunApp here(?)"
getAxFunSym _ = throwError "Implementation bug, cannot have equality judgement here"

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
  prem' <- mapM (checkJudgem forall') prem
  concl' <- checkJudgem forall' concl

  return (Axiom name forall' prem' concl')

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
checkForallVars :: [(MetaVar, Sort)] -> TypeCheckM [(MetaVar, Sort)]
checkForallVars forall = do
  -- changes the sort to appropriate depth (if it's dependent at all)
  forall' <- mapM (\ (a , b) -> do
    b' <- checkSortByName (length $ mContext a) (getSortName b)
    return (a , b') ) forall

  -- check for dups in captures and x.x situations
  mapM_ (\ (a , _) -> lift . checkForDups "Duplicates in captures" $ mName a : mContext a) forall'
  lift . checkForDups "Duplicates in metas" $ map (mName . fst) forall'

  -- adding binders to list of variables - shouldn't do this
  let vars = Set.toList . Set.fromList $ concatMap (mContext . fst) forall'  -------------------------------- usage of tm
  let vars' = map (\x -> (MetaVar [] x , varSort)) vars

  return forall' -- ++ vars'

-- given meta vars (forall) and a judgement - typechecks it
checkJudgem :: [(MetaVar, Sort)] -> Judgement -> TypeCheckM Judgement
checkJudgem meta st@(Statement ctx tm ty) = do
  meta' <- checkCtxVars meta
  checkTerm meta' tm
  checkTerm meta' ty
  return st

checkCtxVars ::

-- Given a context (forall) and the sort of the term check it's correctness
-- Not all high level terms have to be sort checked (only statements)
checkTerm :: [(MetaVar, Sort)] -> Term -> TypeCheckM ()
-- проверить все аппы на корректность сортов
-- все сабсты на корректность перем
-- передаем контекст внутрь
checkTerm meta vns = undefined

--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Reductions
typeCheckReductions :: TypeCheckM ()
typeCheckReductions = return ()


--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Main

-- State = Set DepVars, Set Vars, Map funcs,

main' :: FilePath -> IO ()
main' file = do
  str <- readFile file
  let lang = parseLang (show file) str
  putStrLn $ case runTypecheck lang of
    Left err -> "hmm " ++ show err
    x -> show x

mainP :: FilePath -> IO ()
mainP file = do
  str <- readFile file
  let k = parseLang (show file) str
  case k of
    Right x -> putStr $ show x
    Left x -> putStr x






---
