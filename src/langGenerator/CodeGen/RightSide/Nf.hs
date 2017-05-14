module CodeGen.RightSide.Nf(
  buildRightNf
) where

import Control.Monad.State
import Control.Monad.Except (throwError, lift)
import Control.Lens
import Language.Haskell.Exts.Simple

import qualified Data.Map as Map

import AST hiding (Var, name, Name)
import qualified AST (Term(Var), Name, name)
import AST.Reduction hiding (name)

import CodeGen.Common hiding (count)
import CodeGen.RightSide.Common
import CodeGen.RightSide.Helpers
import CodeGen.RightSide.Exprs
import CodeGen.RightSide.Infer

-- returns a pair (nf right side, nf' binds as many as judgments passed)
buildRightNf :: FunctionalSymbol -> [Reduction] -> ErrorM (Exp, [Match])
buildRightNf (AST.FunSym nm args _) [] = do
  let nfRight = appFunS nm (normalise $ AST.getSortDepth <$> args)
  return (nfRight, [])
buildRightNf (AST.FunSym nm args _) xs = do
  let nfRight = appFunS nm (normalise $ AST.getSortDepth <$> args)
  let n = length xs
  let nfRight' = appFun (var nf'N) [buildCnt n, nfRight]

  let cnts = reverse (take n [0..])
  matches <- mapM (\(cnt, j) -> runBM Map.empty (buildNf' cnt j)) (zip cnts xs)

  return (nfRight', matches)


buildNf' :: Int -> Reduction -> BldRM Match
buildNf' cnt red = do
  -- populate foralls
  populateForalls (forallVars red)
  -- gen left side & write all metas given as args
  leftFun <- buildLeft cnt (conclusion red)
  -- check metas for equality and leave only one in map if many
  genCheckMetaEq
  genReturnSt (conclusion red)
  -- this time we get the stms and wrap them in 'case' exp
  inside <- uses doStmts doE

  return $ Match nf'N
                [leftFun]
                (UnGuardedRhs $ caseRight cnt inside)
                Nothing

--------------------------------------------------------------------------------
-- only conceptually different part from infer
buildLeft :: Int -> Judgement -> BldRM Pat
buildLeft = undefined

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


--------------------------------------------------------------------------------
genReturnSt :: Judgement -> BldRM ()
genReturnSt (Reduct _ r _) = do
  ret <- buildTermExp [] r
  appendExp $ retExp ret
genReturnSt _ = throwError "Can't have anything but Reduct in conclusion"

-- given a funsym generates its' args' normalisations according to their ctx
normalise :: [ContextDepth] -> [Exp]
normalise lst = (\(n, v) -> nf n (var $ name v)) <$> lst'
  where lst' = zip lst vars


-- given a do block in Either wraps it into a case, Right x -> x
caseRight :: Int -> Exp -> Exp
caseRight cnt ex = caseE ex [alt (pApp (name "Left") [PWildCard])
                                 (nonMatch cnt),
                             alt (pApp (name "Right") [pvar $ name "x"])
                                 (var $ name "x")]

-- feed it (U(U(U(U(U(Bot)))))) to rec call in case of failure
-- we must call one less than us, so if we are number 1 out of 3
-- we call U(U(Bot))
nonMatch :: Int -> Exp
nonMatch n = appFun (var nf'N) [buildCnt n, var tmAlias]

tmAlias = name ("tmAlias")
nf'N = name "nf'"










--










---
