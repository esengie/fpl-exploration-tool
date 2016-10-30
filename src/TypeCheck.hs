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
data VarMap    = VarMap {
  _depSorts     :: Set.Set SortName
, _simpleSorts  :: Set.Set SortName
, _funSyms      :: Map.Map Name FunctionalSymbol
} deriving (Eq, Show)

makeLenses ''VarMap

type TypeCheckM = StateT VarMap (Either TypeError)

varsInit :: VarMap
varsInit = VarMap Set.empty Set.empty Map.empty

runTypecheck :: Either String LangSpec -> Either TypeError VarMap
runTypecheck lsp = do
  lsp' <- lsp
  execStateT (typecheck lsp') varsInit

typecheck :: LangSpec -> TypeCheckM ()
typecheck lsp = do
    typecheckSorts lsp
    typecheckFunSyms (AST.funSyms lsp)

checkForDups :: [SortName] -> Either TypeError (Set.Set SortName)
checkForDups lst = do
  let deps = Set.fromList lst
  when (length lst /= Set.size deps) $ throwError "Duplicates in sorts"
  return deps

typecheckSorts :: LangSpec -> TypeCheckM ()
typecheckSorts lsp = do
  deps <- lift . checkForDups $ AST.depSortNames lsp
  sims <- lift . checkForDups $ AST.simpleSortNames lsp
  when (Set.size (Set.intersection sims deps) /= 0) $ throwError "Dependent and simple sorts can't intersect"
  modify $ set depSorts deps
  modify $ set simpleSorts sims


-- checks and modifies the return sort if need be
checkFun :: FunctionalSymbol -> TypeCheckM FunctionalSymbol
checkFun fs@(FunSym n args res) = do
  st <- get
  -- Adding the type knowledge here
  let fs' = if Set.member (getSortName res) (st^.simpleSorts)
              then fs
              else FunSym n args (DepSort (getSortName res) 0)

  let isIn f st = Set.size (Set.difference (Set.fromList (map getSortName (filter f args))) st) == 0
  unless (isIn isDepSort (st^.depSorts)) $
    throwError $ show n ++ " functional symbol's dependent sorts are not completely defined"
  unless (isIn (not . isDepSort) (st^.simpleSorts)) $
    throwError $ show n ++ " functional symbol's simple sorts are not completely defined"

  when (isJust $ Map.lookup (nameFun fs') (st^.TypeCheck.funSyms)) $
    throwError $ "Function redefinition " ++ nameFun fs'
  return fs'

-- typecheck and populate the state
typecheckFunSyms :: [FunctionalSymbol] -> TypeCheckM ()
typecheckFunSyms [] = return ()
typecheckFunSyms (fs : fss) = do
  fs' <- checkFun fs
  modify $ over TypeCheck.funSyms (Map.insert (nameFun fs') fs')
  typecheckFunSyms fss

-- State = Set DepVars, Set Vars, Map funcs,

main' :: FilePath -> IO ()
main' file = do
  str <- readFile file
  let lang = parseLang (show file) str
  putStrLn $ case runTypecheck lang of
    Left err -> "hmm " ++ show err
    x -> show x











---
