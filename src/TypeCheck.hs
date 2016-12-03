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

type TypeError = String
data SymbolTable = SymbolTable {
  _depSorts      :: Set.Set SortName
, _simpleSorts   :: Set.Set SortName
, _funSyms       :: Map.Map Name FunctionalSymbol
, _axioms        :: Map.Map Name Axiom
-- , _reductions    :: Map.Map Name
} deriving (Eq, Show)

makeLenses ''SymbolTable

lookupSortByName :: ContextDepth -> SortName -> TypeCheckM Sort
lookupSortByName depth name = do
  st <- get
  if Set.member name (st^.simpleSorts)
    then return (SimpleSort name)
    else
      if Set.member name (st^.depSorts)
        then return (DepSort name depth)
      else
        throwError $ "Sort " ++ name ++ " is not defined"

type TypeCheckM = StateT SymbolTable (Either TypeError)

varsInit :: SymbolTable
varsInit = SymbolTable Set.empty Set.empty Map.empty Map.empty

runTypecheck :: Either String LangSpec -> Either TypeError SymbolTable
runTypecheck lsp = do
  lsp' <- lsp
  execStateT (typecheck lsp') varsInit

-- SortName or VarName
checkForDups :: String -> [Name] -> Either TypeError (Set.Set Name)
checkForDups msg lst = do
  let deps = Set.fromList lst
  when (length lst /= Set.size deps) $ throwError msg
  return deps

typecheckSorts :: LangSpec -> TypeCheckM ()
typecheckSorts lsp = do
  deps <- lift . checkForDups "Duplicates in sorts" $ AST.depSortNames lsp
  sims <- lift . checkForDups "Duplicates in sorts" $ AST.simpleSortNames lsp
  when (Set.size (Set.intersection sims deps) /= 0) $ throwError "Dependent and simple sorts can't intersect"
  modify $ set depSorts deps
  modify $ set simpleSorts sims


-- Checks func redefinition, checks depsorts and simplesorts
-- Adds the return sort
checkFun :: FunctionalSymbol -> TypeCheckM FunctionalSymbol
checkFun fs@(FunSym n args res) = do
  st <- get

  when (isJust $ Map.lookup n (st^.TypeCheck.funSyms)) $
    throwError $ "Function redefinition " ++ n

  -- Adding the type knowledge of the result here
  let fs' = if Set.member (getSortName res) (st^.simpleSorts)
              then fs
              else FunSym n args (DepSort (getSortName res) 0)

  let isIn f st = Set.size (Set.difference (Set.fromList (map getSortName (filter f args))) st) == 0
  unless (isIn isDepSort (st^.depSorts)) $
    throwError $ show n ++ " functional symbol's dependent sorts are not completely defined"
  unless (isIn (not . isDepSort) (st^.simpleSorts)) $
    throwError $ show n ++ " functional symbol's simple sorts are not completely defined"

  return fs'

-- typecheck and populate the state (sorts of return types may need modification - we do it here)
typecheckFunSyms :: [FunctionalSymbol] -> TypeCheckM ()
typecheckFunSyms [] = return ()
typecheckFunSyms (fs : fss) = do
  fs' <- checkFun fs
  modify $ over TypeCheck.funSyms (Map.insert (nameFun fs') fs')
  typecheckFunSyms fss

-- need to check forall var types and change them if need be
typecheckAxioms :: [Axiom] -> TypeCheckM ()
typecheckAxioms [] = return ()
typecheckAxioms (ax : axs) = do
  ax' <- checkAx ax
  modify $ over TypeCheck.axioms (Map.insert (nameAx ax') ax')
  typecheckAxioms axs


-- check redefinition, fix forallvars, check types inside each judgement
checkAx :: Axiom -> TypeCheckM Axiom
checkAx ax@(Axiom name forall prem concl) = do
  st <- get

  when (isJust $ Map.lookup name (st^.TypeCheck.axioms)) $
    throwError $ "Axiom redefinition: " ++ name

  when (isEqJudgement concl) $
    throwError $ "Equality is not allowed in the conclusion of typing rules: " ++ name

  forall' <- checkForallVars forall
  prem' <- mapM (checkJudgem forall') prem
  concl' <- checkJudgem forall' concl

  return (Axiom name forall' prem' concl')

-- checks and modifies one vars and checks for dups
checkForallVars :: [(MetaVar, Sort)] -> TypeCheckM [(MetaVar, Sort)]
checkForallVars forall = do
  -- just map the function inside a monad (yup this is just plumbing for making args fit)
  forall' <- mapM (\ (a , b) -> do
    b' <- lookupSortByName (length $ mContext a) (getSortName b)
    return (a , b') ) forall

  -- check for dups
  mapM_ (\ (a , b) -> lift . checkForDups "Duplicates in captures" $ mName a : mContext a) forall'

  -- adding binders to list
  let vars = Set.toList . Set.fromList $ concatMap (mContext . fst) forall'  -------------------------------- usage of tm
  let vars' = map (\x -> (MetaVar [] x , varSort)) vars

  lift . checkForDups "Duplicates in metas" $ map (\ (a , b) -> mName a) forall'

  return $ vars' ++ forall'

checkJudgem :: [(MetaVar, Sort)] -> Judgement -> TypeCheckM Judgement
checkJudgem meta st@(Statement ctx tm ty) = do
--  checkTerm meta tm
--  checkType meta ty
  mapM (checkTerm varSort meta) (map fst ctx)
  mapM (checkType meta) (map snd ctx)
  return st

checkTerm :: [(MetaVar, Sort)] -> Sort -> Term -> TypeCheckM ()
checkTerm meta vns = undefined

typecheck :: LangSpec -> TypeCheckM ()
typecheck lsp = do
    typecheckSorts lsp
    typecheckFunSyms (AST.funSyms lsp)
    typecheckAxioms (AST.axioms lsp)
    typecheckReductions

typecheckReductions :: TypeCheckM ()
typecheckReductions = return ()

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
