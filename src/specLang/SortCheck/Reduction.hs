module SortCheck.Reduction (
  sortCheckReductions
) where

import Control.Monad.Trans.State.Lazy
import Control.Monad.Except (throwError)
import Control.Monad.Trans.Class (lift)
import Control.Lens
import Data.Maybe (isJust)
import Control.Monad (when, unless)

import qualified Data.Map as Map

import AST
import AST.Reduction
import SortCheck.SymbolTable as SymbolTable
import SortCheck.Judgement
import SortCheck.Forall

--------------------------------------------------------------------------------
-- Reductions
sortCheckReductions :: [Reduction] -> SortCheckM ()
sortCheckReductions [] = return ()
sortCheckReductions (red : reds) = do
  red' <- checkRed red
  modify $ over SymbolTable.reductions (Map.insert (name red') red')

  sortCheckReductions reds

checkRed :: Reduction -> SortCheckM Reduction
checkRed red@(Reduction name forall prem concl) = do
  st <- get

  when (isJust $ Map.lookup name (st^.SymbolTable.reductions)) $
    throwError $ "Reduction redefinition: " ++ name

  unless (isRedJudgement concl) $
    throwError $ "Must be a reduction: " ++ name

  forall' <- checkForallVars forall
  mapM_ (checkJudgem forall') prem
  checkJudgem forall' concl

  return (Reduction name forall' prem concl)




---
