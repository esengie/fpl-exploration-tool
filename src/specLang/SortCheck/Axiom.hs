module SortCheck.Axiom (
  sortCheckAxioms
)
  where

import Control.Monad.Trans.State.Lazy
import Control.Monad.Except (throwError)
import Control.Monad.Trans.Class (lift)
import Control.Lens
import Data.Maybe (isJust)
import Control.Monad (when, unless)

import qualified Data.Map as Map

import AST
import AST.Axiom
import SortCheck.SymbolTable as SymbolTable
import SortCheck.Judgement
import SortCheck.Forall

--------------------------------------------------------------------------------
-- Axioms

sortCheckAxioms :: [Axiom] -> SortCheckM ()
sortCheckAxioms axs = do
  sortCheckAxioms' axs
  st <- get
  when (Map.size (st^.iSymAxiomMap) < Map.size (st^.SymbolTable.funSyms)) $
    throwError "Not all funSyms have intro axioms"

sortCheckAxioms' :: [Axiom] -> SortCheckM ()
sortCheckAxioms' [] = return ()
sortCheckAxioms' (ax : axs) = do
  ax' <- checkAx ax
  modify $ over SymbolTable.axioms (Map.insert (name ax') ax')

  -- check there is only one funSym intro axiom
  -- can't have equalities in the conclusion
  funSym <- getAxFunSym ax'
  st <- get
  when (isJust $ Map.lookup funSym (st^.iSymAxiomMap)) $
    throwError $ "There is already an intro axiom for " ++ funSym
  modify $ over iSymAxiomMap (Map.insert funSym (name ax'))
  sortCheckAxioms' axs

-- could be less monadic, but it's easier to throw errors this way
-- statements are always funSym intros
getAxFunSym :: Axiom -> SortCheckM Name
getAxFunSym (Axiom _ _ _ (Statement _ (FunApp name tms) _)) = do
  checkArgsAreMetaVars tms
  return name
  where
    checkArgsAreMetaVars :: [Term] -> SortCheckM ()
    checkArgsAreMetaVars [] = return ()
    checkArgsAreMetaVars (Var _ : xs) = checkArgsAreMetaVars xs
    checkArgsAreMetaVars (TermInCtx _ (Var _) : xs) = checkArgsAreMetaVars xs
    checkArgsAreMetaVars _ = throwError $ "Not all terms in " ++ name ++ " are metavars"

getAxFunSym (Axiom _ _ _ Statement {}) =
  throwError "Implementation bug, should have FunApp here"
getAxFunSym _ = throwError "Implementation bug, cannot have equality judgement in conclusion"

-- need to check forall var types and change them if need be
-- check redefinition, fix forallvars, check types inside each judgement
checkAx :: Axiom -> SortCheckM Axiom
checkAx ax@(Axiom name forall prem concl) = do
  st <- get

  when (isJust $ Map.lookup name (st^.SymbolTable.axioms)) $
    throwError $ "Axiom redefinition: " ++ name

  when (isEqJudgement concl) $
    throwError $ "Equality is not allowed in the conclusion of typing rules: " ++ name ++ "\nUse reductions"

  -- unless (isFunSym tm) $
  --   throwError $ "Statements must define fun syms\n" ++ show st

  forall' <- checkForallVars forall
  mapM_ (checkJudgem forall') prem
  checkJudgem forall' concl

  return (Axiom name forall' prem concl)





---
