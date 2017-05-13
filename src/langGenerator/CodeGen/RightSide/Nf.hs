module CodeGen.RightSide.Nf(
  buildRightNf
) where

import Control.Monad.State
import Control.Monad.Except (throwError, lift)
import Control.Lens
import Language.Haskell.Exts.Simple

import qualified Data.Map as Map

import AST hiding (Var, name, Name)
import qualified AST (Term(Var), Name)
import AST.Reduction hiding (name)

import CodeGen.Common hiding (count)
import CodeGen.RightSide.Common
import CodeGen.RightSide.Helpers
import CodeGen.RightSide.Exprs

buildRightNf :: FunctionalSymbol -> [Reduction] -> ErrorM Exp
buildRightNf fs ax = pure ExprHole
  -- runBM fss (buildRight' fs ax)

-- buildRight' :: FunctionalSymbol -> Axiom -> BldRM Exp
-- buildRight' fs ax = do
--   -- populate foralls
--   populateForalls ax
--   -- write all metas given as args
--   correctFresh ax
--   -- check metas for equality and leave only one in map if many
--   genCheckMetaEq
--   -- find all used Metavars + check for equality where needed
--   -- First check all guys of the smth : T - build up the map (metavars : Term)
--   mapM_ labelJudgement (premise ax)
--
--   -- returns checks for contexts and infers the part after |-
--   -- equality goes like this "checkEq a b >> infer (consCtx v) a"
--   -- [[Exp]]
--   -- [MetaVar]
--   metaJs <- use (juds.metaTyDefs)
--   expsMeta <- mapM (buildInferExps . snd) metaJs
--   stmtsMeta <- mapM stmtsAndMetaLast $ zipWith
--               (\(a,jud) c -> (a, judCtx jud,c))
--               metaJs  expsMeta
--   mapM_ appendStmt (concat stmtsMeta)
--   -- check metas for equality after all of them are added
--   genCheckMetaEq
--   ------------------------------------------------------------------------------
--   ctTerms <- use (juds.notDefsTy)
--   expsTyTms <- mapM (buildInferExps . snd) ctTerms
--   stmtsTyTms <- mapM stmtsAndTmEqLast $ zipWith
--               (\(a,jud) c -> (a, judCtx jud,c))
--               ctTerms expsTyTms
--   mapM_ appendStmt (concat stmtsTyTms)
--   ------------------------------------------------------------------------------
--   -- a = b >> check ctx TyDef expr
--   expsDef <- join $ uses (juds.otherJuds) (mapM buildCheckExps)
--   mapM_ appendExp (concat expsDef)
--
--   genReturnSt fs (conclusion ax)
--   uses doStmts doE
--
-- --------------------------------------------------------------------------------
-- -- first vars are already used
-- -- also axioms are always of the form like this
-- correctFresh :: Axiom -> BldRM ()
-- correctFresh (Axiom _ _ _ (Statement _ (FunApp _ lst) _)) = populateSt lst
--   where
--     populateSt ((ct, Meta (MetaVar _ nm)):xs) = do
--       v <- fresh
--       metas %= updateMap (MetaVar ct nm) (fromScope (length ct) $ var (name v))
--       populateSt xs
--     populateSt [] = return ()
--     populateSt _ = throwError "Can't have a non metavariable in an axiom concl"
-- correctFresh _ = throwError $ "error: Only axioms with funsym intro are allowed"
--
-- --------------------------------------------------------------------------------
-- genReturnSt :: FunctionalSymbol -> Judgement -> BldRM ()
-- genReturnSt (FunSym _ _ res) (Statement _ _ Nothing) = do
--   appendExp $ retExp (tyCtor $ sortToTyCtor $ getSortName res)
-- genReturnSt _ (Statement _ _ (Just ty)) = do
--   ret <- buildTermExp [] ty
--   appendExp $ retExp ret
-- genReturnSt _ _ = throwError "Can't have anything but Statement in conclusion"
--










---
