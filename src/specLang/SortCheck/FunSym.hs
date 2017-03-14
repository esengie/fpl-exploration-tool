module SortCheck.FunSym(
  sortCheckFunSyms
) where

import Control.Monad.Trans.State.Lazy
import Control.Monad.Except (throwError)
import Control.Lens
import Data.Maybe (isJust)
import Control.Monad (when, unless)

import qualified Data.Map as Map
import qualified Data.Set as Set

import AST
import SortCheck.SymbolTable as SymbolTable

--------------------------------------------------------------------------------
-- FunSyms

-- SortCheck and populate the state with funsyms
-- (sorts of func return types may need modification - we do it here)
sortCheckFunSyms :: [FunctionalSymbol] -> SortCheckM ()
sortCheckFunSyms [] = return ()
sortCheckFunSyms (fs : fss) = do
  fs' <- checkFun fs
  modify $ over SymbolTable.funSyms (Map.insert (name fs') fs')
  sortCheckFunSyms fss

-- Checks func redefinition, checks depsorts and simplesorts
-- Adds the return sort
checkFun :: FunctionalSymbol -> SortCheckM FunctionalSymbol
checkFun fs@(FunSym name args res) = do
  st <- get

  when (isJust $ Map.lookup name (st^.SymbolTable.funSyms)) $
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




---
