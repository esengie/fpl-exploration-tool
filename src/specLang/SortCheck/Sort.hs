module SortCheck.Sort(
  sortCheckSorts,
  checkTmSort,
  checkTySort,
  checkEqSorts
) where

import Control.Monad.Trans.State.Lazy
import Control.Monad.Except (throwError)
import Control.Monad.Trans.Class (lift)
import Control.Lens
import Control.Monad (when, unless)

import qualified Data.Set as Set

import AST
import SortCheck.SymbolTable as SymbolTable

--------------------------------------------------------------------------------
-- Sorts

-- SortName or VarName
-- Throws Error when there are duplicatese in a list of names
checkForDups :: String -> [Name] -> Either SortError (Set.Set Name)
checkForDups msg lst = do
  let deps = Set.fromList lst
  when (length lst /= Set.size deps) $ throwError msg
  return deps

-- Checks for duplicates, intersections and sets the sorts
sortCheckSorts :: LangSpec -> SortCheckM ()
sortCheckSorts lsp = do
  deps <- lift . checkForDups "Duplicates in sorts" $ AST.depSortNames lsp
  sims <- lift . checkForDups "Duplicates in sorts" $ AST.simpleSortNames lsp
  when (Set.size (Set.intersection sims deps) /= 0) $ throwError "Dependent and simple sorts can't intersect"
  unless (Set.member tmName deps) $ throwError $ "Need to have a " ++ tmName ++ " sort"
  unless (Set.member tyName deps || Set.member tyName sims) $
    throwError $ "Need to have a " ++ tyName ++ " sort"
  modify $ set depSorts deps
  modify $ set simpleSorts sims

--------------------------------------------------------------------
-- given a sort checks if it's equal to universal tm sort
checkTmSort :: Sort -> Term -> SortCheckM ()
checkTmSort tmSort tm =
  let sName = getSortName tmSort in
  checkEqSorts sName tmName $ "Left of : is not a term, but " ++ show sName ++
                               "\n in " ++ show tm

checkTySort :: Sort -> Term -> SortCheckM ()
checkTySort tySort tm =
  let sName = getSortName tySort in
  checkEqSorts sName tyName $ "Right of : is not a type, but " ++ show sName ++
                                "\n in " ++ show tm

checkEqSorts :: SortName -> SortName -> String -> SortCheckM ()
checkEqSorts l r msg =  when (l /= r) $ throwError msg

---
