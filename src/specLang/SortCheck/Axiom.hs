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
import AST.Axiom as Axiom
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
  modify $ over SymbolTable.axioms (Map.insert (Axiom.name ax') ax')

  -- check there is only one funSym intro axiom
  -- can't have equalities in the conclusion
  funSym <- getAxFunSym ax'
  st <- get
  when (isJust $ Map.lookup funSym (st^.iSymAxiomMap)) $
    throwError $ "There is already an intro axiom for " ++ funSym
  modify $ over iSymAxiomMap (Map.insert funSym (Axiom.name ax'))
  sortCheckAxioms' axs

-- statements are always funSym intros
-- we're here strictly after simple checking of terms => have all the funsyms we need
getAxFunSym :: Axiom -> SortCheckM Name
getAxFunSym (Axiom nm fvs _ (Statement _ (FunApp name tms) ty)) = do
  checkArgsAreMetaVars (map fst fvs) tms
  (FunSym _ _ srt) <- uses (SymbolTable.funSyms) (unJust . Map.lookup name)
  when (null ty && srt == varSort) $
    throwError $ "Axiom that introduces a funsym of sort tm must have a type: " ++ nm
  return name
  where
    checkArgsAreMetaVars :: [MetaVar] -> [(Ctx, Term)] -> SortCheckM ()
    checkArgsAreMetaVars fvs [] = return ()
    checkArgsAreMetaVars fvs ((ct, Meta (MetaVar _ mvN)): xs) = do
      let mv = MetaVar ct mvN
      let fv = unJust $ lookup mv (zip fvs fvs)
      unless (identicalMV mv fv) $
        throwError $ show mv ++ " has different context from " ++
                     show fv ++ " in funsym intro axiom: " ++ nm
      checkArgsAreMetaVars fvs xs
    checkArgsAreMetaVars _ _ = throwError $ "Not all terms in " ++ name ++
                                            " are metavars in: " ++ nm

getAxFunSym (Axiom _ _ _ Statement {}) =
  throwError "Implementation bug, should have FunApp here"
getAxFunSym _ = throwError "Implementation bug, cannot have equality judgement in conclusion"

-- need to check forall var types and change them if need be
-- check redefinition, fix forallvars, check types inside each judgement
checkAx :: Axiom -> SortCheckM Axiom
checkAx ax@(Axiom name forall prem concl) = do
  st <- get

  when (Map.member name $ st^.SymbolTable.axioms) $
    throwError $ "Axiom redefinition: " ++ name

  when (isEqJudgement concl) $
    throwError $ "Equality is not allowed in the conclusion of typing rules: " ++ name ++ "\nUse reductions"

  unless (null $ _jContext concl) $
    throwError $ "Conclusion must have empty context: " ++ name

  forall' <- checkForallVars forall
  prem' <- mapM (checkJudgem forall') prem
  concl' <- checkJudgem forall' concl

  return (Axiom name forall' prem' concl')





---
