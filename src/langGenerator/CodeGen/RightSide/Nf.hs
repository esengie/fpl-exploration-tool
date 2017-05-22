module CodeGen.RightSide.Nf(
  buildRightNf
) where

import Control.Monad.State
import Control.Monad.Except (throwError, lift)
import Control.Lens
import Language.Haskell.Exts.Simple
import Debug.Trace

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
  -- genCheckStability $ stab red
  -- populate foralls
  populateForalls (forallVars red)
  -- gen left side & write all metas given as args -- don't mess with contexts
  leftFun <- buildLeft (cnt + 1) (conclusion red)
  -- shorten & check metas for equality and leave only one in map if many
  -- xyc.Z -> forallCtx(Z)
  genShortenMetas
  genCheckMetaEq
  genReturnSt (conclusion red)
  -- this time we get the stms and wrap them in 'case' exp
  inside <- uses doStmts doExp

  -- mets <- uses metas (\m -> (\(x, c) -> (x,fst <$> c)) <$> Map.toList m)
  -- trace (show mets) $
  return $ Match nf'N
                leftFun
                (UnGuardedRhs $ caseRight cnt inside)
                Nothing

--------------------------------------------------------------------------------
-- only conceptually different part from infer
buildLeft :: Int -> Judgement -> BldRM [Pat]
buildLeft n (Reduct _ l _ _) = do
  pat <- buildTermPat [] l
  return [buildCntP n, PAsPat tmAlias pat]

buildLeft _ _ = throwError "Can't have anything but Reduct in conclusion"

--------------------------------------------------------------------------------
-- we're in nf, no need to nf like in infer case
genShortenMetas :: BldRM ()
genShortenMetas = do
  -- (meta, [(ctx, exp)])
  mets <- uses metas (Map.toList)
  mapM_ genShortenMeta mets

genShortenMeta :: (MetaVar, [(Ctx, Exp)]) -> BldRM ()
genShortenMeta (mv@(MetaVar ctx _), lst) = do
  freshs <- replicateM (length lst) fresh
  exps <- mapM (trimMeta ctx) lst

  let v_to_ex = zipWith (\f (_, ex) -> (f,ex)) freshs exps
  mapM_ (appendStmt . (uncurry generator)) v_to_ex

  let new_exps = zipWith (\f (ct, _) -> (ct, var $ name f)) freshs exps
  metas %= Map.insert mv new_exps

--------------------------------------------------------------------------------
genReturnSt :: Judgement -> BldRM ()
genReturnSt (Reduct _ _ r _) = do
  ret <- buildTermExp [] r
  appendExp $ retExp ret
genReturnSt _ = throwError "Can't have anything but Reduct in conclusion"

--------------------------------------------------------------------------------
-- given a funsym generates its' args' normalisations according to their ctx
normalise :: [ContextDepth] -> [Exp]
normalise lst = (\(n, v) -> nf n (var $ name v)) <$> lst'
  where lst' = zip lst vars


-- given a do block in Either wraps it into a case, Right x -> x
caseRight :: Int -> Exp -> Exp
caseRight cnt ex = caseE ex [alt (pApp (name "Left") [PWildCard])
                                 (nonMatch cnt),
                             alt (pApp (name "Right") [pvar $ name "x"])
                                 (app (var nfN) $ var (name "x"))]

-- feed it (U(U(U(U(U(Bot)))))) to rec call in case of failure
-- we must call one less than us, so if we are number 1 out of 3
-- we call U(U(Bot))
nonMatch :: Int -> Exp
nonMatch n = appFun (var nf'N) [buildCnt n, var tmAlias]










--










---
