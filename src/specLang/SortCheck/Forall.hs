module SortCheck.Forall (
  MetaCtx(..),
  checkForallVars
) where

import Control.Monad.Trans.State.Lazy
import Control.Monad.Except (throwError)
import Control.Monad.Trans.Class (lift)
import Control.Lens
import Data.Maybe (isJust)
import Control.Monad (when, unless)

import qualified Data.Set as Set

import AST
import SortCheck.SymbolTable as SymbolTable

type MetaCtx = [(MetaVar, Sort)]

--------------------------------------------------------------------------------
-- ForallVars

-- This function looks up a sortName in state
-- ContextDepth is needed for forming the sort (not lookup)
checkSortByName :: ContextDepth -> SortName -> SortCheckM Sort
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
checkForallVars :: MetaCtx -> SortCheckM MetaCtx
checkForallVars forall = do
  -- changes the sort to appropriate depth (if it's dependent at all)
  forall' <- mapM (\ (a , b) -> do
    b' <- checkSortByName (length $ mContext a) (getSortName b)
    return (a , b') ) forall
  -- check for dups in captures and x.x situations
  mapM_ (\ (a , _) -> unless (allUnique (mName a : mContext a)) $
                        throwError "Duplicates in captures") forall'
  unless (allUnique $ map (mName . fst) forall') $
    throwError "Duplicates in metas"

  return forall'




---
