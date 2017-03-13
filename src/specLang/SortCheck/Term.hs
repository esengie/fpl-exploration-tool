module SortCheck.Term (
  Ctx(..),
  checkTerm
) where

import Control.Monad.Trans.State.Lazy
import Control.Monad.Except (throwError)
import Control.Monad.Trans.Class (lift)
import Control.Lens
import Control.Monad (when, unless)

import qualified Data.Map as Map

import AST
import SortCheck.SymbolTable as SymbolTable
import SortCheck.Forall (MetaCtx)

type Ctx = [VarName]

-- Given a context + forall. (The sort of the term was checked)
-- ??Not all high level terms have to be sort checked (only statements)
checkTerm :: MetaCtx -> Ctx -> Term -> SortCheckM Sort
-- проверить все аппы на корректность сортов
checkTerm meta ctx (Var name) = do
      -- if name `elem` ctx
      --   then return varSort -- is this bullshit?
      --   else do
    -- so we're a metavar: check we have all we need in ctx and return our sort
    (mVar, sort) <- lift (lookupName (AST.mName . fst) name meta)
    unless (isSubset (mContext mVar) ctx) $
      throwError $ "Not all vars of a metavar are in context! Have:\n\t" ++
        show ctx ++ "\nNeed:\n\t" ++ show (mContext mVar)
    return sort
checkTerm meta ctx (TermInCtx vars tm) = do
  unless (allUnique $ vars ++ ctx) $
    throwError $ "Added vars that shadow other vars in ctx:\n" ++ show ctx ++ show vars
  checkTerm meta (vars ++ ctx) tm

checkTerm meta ctx (FunApp f args) = do
  st <- get
  case Map.lookup f (st^.SymbolTable.funSyms) of
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
    then throwError $ "Can't subst " ++ show whatSort ++ " into a var of sort " ++ show varSort
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





---
