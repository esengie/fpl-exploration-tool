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
import AST.Reduction as Reduction
import SortCheck.SymbolTable as SymbolTable
import SortCheck.Judgement
import SortCheck.Forall

--------------------------------------------------------------------------------
-- Reductions

sortCheckReductions :: [Reduction] -> SortCheckM ()
sortCheckReductions [] = return ()
sortCheckReductions (red : reds) = do
  red' <- checkRed red
  modify $ over SymbolTable.reductions (Map.insert (Reduction.name red') red')

  checkConclRed (conclusion red')
  sortCheckReductions reds

checkRed :: Reduction -> SortCheckM Reduction
checkRed red@(Reduction name stab forall prem concl) = do
  st <- get

  when (isJust $ Map.lookup name (st^.SymbolTable.reductions)) $
    throwError $ "Reduction redefinition: " ++ name

  unless (isRedJudgement concl) $
    throwError $ "Must be a reduction: " ++ name

  forall' <- checkForallVars forall
  prem' <- mapM (checkJudgem forall') prem
  concl' <- checkJudgem forall' concl

  return (Reduction name stab forall' prem' concl')

checkConclRed :: Judgement -> SortCheckM ()
checkConclRed r@(Reduct _ lft rt _) = do
  noSubstLeft ("Subst on the left of reduction " ++ show r) lft
  unless (isSubset (toListM rt) (toListM lft)) $
    throwError $ "Not all metavars on right of " ++ show r ++ " are on the left"

noSubstLeft :: String -> Term -> SortCheckM ()
noSubstLeft msg (Subst{}) = throwError msg
noSubstLeft msg (FunApp _ lst) = mapM_ (noSubstLeft msg . snd) lst
noSubstLeft _ _ = return ()


---
