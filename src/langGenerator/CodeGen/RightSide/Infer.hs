module CodeGen.RightSide.Infer(
  buildRightInfer,
  populateForalls,
  genCheckMetaEq
) where

import Control.Monad.State
import Control.Monad.Except (throwError, lift)
import Control.Lens
import Language.Haskell.Exts.Simple

import qualified Data.Map as Map

import AST hiding (Var, name, Name)
import qualified AST (Term(Var), Name)
import AST.Axiom hiding (name)

import CodeGen.Common hiding (count)
import CodeGen.RightSide.Common
import CodeGen.RightSide.Helpers
import CodeGen.RightSide.Exprs

buildRightInfer :: (Map.Map AST.Name FunctionalSymbol) -> FunctionalSymbol -> Axiom -> ErrorM Exp
buildRightInfer fss fs ax = -- pure ExprHole
  runBM fss (buildRight' fs ax)

buildRight' :: FunctionalSymbol -> Axiom -> BldRM Exp
buildRight' fs ax = do
  genCheckStability $ stab ax
  -- populate foralls
  populateForalls (forallVars ax)
  -- write all metas given as args
  correctFresh ax
  -- check metas for equality and leave only one in map if many
  genCheckMetaEq
  -- find all used Metavars + check for equality where needed
  -- First check all guys of the smth : T - build up the map (metavars : Term)
  mapM_ labelJudgement (premise ax)

  -- returns checks for contexts and infers the part after |-
  -- equality goes like this "checkEq a b >> infer (consCtx v) a"
  -- [[Exp]]
  -- [MetaVar]
  metaJs <- use (juds.metaTyDefs)
  expsMeta <- mapM (buildInferExps . snd) metaJs
  stmtsMeta <- mapM stmtsAndMetaLast $ zipWith
              (\(a,jud) c -> (a, judCtx jud,c))
              metaJs  expsMeta
  mapM_ appendStmt (concat stmtsMeta)
  -- check metas for equality after all of them are added
  genCheckMetaEq
  ------------------------------------------------------------------------------
  ctTerms <- use (juds.notDefsTy)
  expsTyTms <- mapM (buildInferExps . snd) ctTerms
  stmtsTyTms <- mapM stmtsAndTmEqLast $ zipWith
              (\(a,jud) c -> (a, judCtx jud,c))
              ctTerms expsTyTms
  mapM_ appendStmt (concat stmtsTyTms)
  ------------------------------------------------------------------------------
  -- a = b >> check ctx TyDef expr
  expsDef <- join $ uses (juds.otherJuds) (mapM buildCheckExps)
  mapM_ appendExp (concat expsDef)

  genReturnSt fs (conclusion ax)
  uses doStmts doExp

-- >>= \t -> remvars (Metavar) this
stmtsAndMetaLast :: (MetaVar, Ctx, [Exp]) -> BldRM [Stmt]
stmtsAndMetaLast (_, _,  []) = throwError "stmtsAndMetaLast must be called with at least one expr"
-- this is a metaVar def
stmtsAndMetaLast (m, ct, x:[]) = do
  -- v_i <- x
  vn <- fresh
  let vname = var (name vn)
  -- trim ctx of metavar given in here and put it into the metamap
  -- v_i+1 <- trim v_i
  (mct, mvarExp) <- trimMeta (mContext m) (ct, nf 0 vname)
  vm <- fresh
  metas %= updateMap m (mct, var (name vm))
  -- return first v <- infer ..., then m <- trimmed
  -- the benefit of using remove here is that it's in TC too,
  -- every other place we just use Identity monad!
  return [generator vn x, generator vm mvarExp]
stmtsAndMetaLast (m, ct, x:xs) = do
  xs' <- stmtsAndMetaLast (m, ct, xs)
  return $ Qualifier x : xs'

-- >>= \t -> remvars (Metavar) this
stmtsAndTmEqLast :: (Term, Ctx, [Exp]) -> BldRM [Stmt]
stmtsAndTmEqLast (_,_,[]) = throwError "stmtsAndTmEqLast must be called with at least one expr"
-- this is a ": Exp" situation so we check it for equality
stmtsAndTmEqLast (tm, ct, x:[]) = do
  vn <- fresh
  let vExp = var (name vn)
  tmExp <- buildTermExp ct tm
  return [generator vn x, Qualifier $ eqCheckExp tmExp vExp]
stmtsAndTmEqLast (tm, ct, x:xs) = do
  xs' <- stmtsAndTmEqLast (tm, ct, xs)
  return $ Qualifier x : xs'

--------------------------------------------------------------------------------
-- first vars are already used
-- also axioms are always of the form like this
correctFresh :: Axiom -> BldRM ()
correctFresh (Axiom _ _ _ _ (Statement _ (FunApp _ lst) _)) = populateSt lst
  where
    populateSt ((ct, Meta mv):xs) = do
      v <- fresh
      metas %= updateMap mv (ct, fromScope (length ct) $ var (name v))
      populateSt xs
    populateSt [] = return ()
    populateSt _ = throwError "Can't have a non metavariable in an axiom concl"
correctFresh _ = throwError $ "error: Only axioms with funsym intro are allowed"

populateForalls :: [(MetaVar, Sort)] -> BldRM ()
populateForalls [] = return ()
populateForalls ((m, sort):xs) = do
  foralls %= Map.insert m sort
  populateForalls xs

--------------------------------------------------------------------------------
-- Check terms for equality
genCheckMetaEq :: BldRM ()
genCheckMetaEq = do
  ms <- gets _metas
  metas <~ sequence (genMetaEq <$> ms)
  -- metas .= res

-- generate code for meta equality checking
genMetaEq :: [(Ctx, Exp)] -> BldRM [(Ctx, Exp)]
genMetaEq [] = return []
genMetaEq (x : []) = return [x]
genMetaEq (tm : y'@(ct2, y) : xs) = do
  ex <- conniveMeta ct2 tm
  let ex' = eqCheckExp ex y
  appendExp ex'
  genMetaEq (y' : xs)

--------------------------------------------------------------------------------
genReturnSt :: FunctionalSymbol -> Judgement -> BldRM ()
genReturnSt (FunSym _ _ res) (Statement _ _ Nothing) = do
  appendExp $ retExp (tyCtor $ sortToTyCtor $ getSortName res)
genReturnSt _ (Statement _ _ (Just ty)) = do
  ret <- buildTermExp [] ty
  appendExp $ retExp ret
genReturnSt _ _ = throwError "Can't have anything but Statement in conclusion"











---
