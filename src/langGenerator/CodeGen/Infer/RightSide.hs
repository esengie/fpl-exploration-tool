{-# LANGUAGE TemplateHaskell #-}

module CodeGen.Infer.RightSide(
  buildRight
) where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except (throwError, lift)
import Control.Lens
import Language.Haskell.Exts.Simple
import Debug.Trace

import qualified Data.Set as Set
import qualified Data.Map as Map

import SortCheck
import AST hiding (Var, name, Name)
import qualified AST (Term(Var))
import AST.Axiom hiding (name)

import CodeGen.Common hiding (count)

data Q = Q {
  _count :: Int,
  -- metaVar as in forall x.T -> termExp
  _metas :: Map.Map MetaVar [(Ctx, Exp)],
  _doStmts :: [Stmt], -- this will be concatted

  -- we define some metavars on the right of :, others we need to check
  _juds  :: Juds,
  -- various counters - the outer monad will have to use this
  _toGen :: ToGen
}

data Juds = Juds {
  _metaTyDefs :: [(MetaVar, Judgement)], -- some var will be added to the metas map (|- g : T)
  _notDefsTy :: [Judgement], -- here it will not, so v_i <- infer ...  (|- g : exp(T, G))
  _notDefsVar :: [Name],
  _otherJuds :: [Judgement]  -- |- g def
}

makeLenses ''Juds
makeLenses ''Q

type BldRM = StateT Q (ErrorM)

buildRight :: FunctionalSymbol -> Axiom -> ErrorM Exp
buildRight fs ax = runBM (buildRight' fs ax)

buildRight' :: FunctionalSymbol -> Axiom -> BldRM Exp
buildRight' fs ax = do
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
  expsMeta <- uses (juds.metaTyDefs) (mapM (buildInferExps . snd))
  -- [MetaVar]
  metas <- uses (juds.metaTyDefs) (map fst)
  stmtsMeta <- mapM stmtsAndMetaLast $ zip metas expsMeta
  mapM_ appendStmt (concat stmtsMeta)
  genCheckMetaEq
  ------------------------------------------------------------------------------
  expsTyTms <- uses (juds.notDefsTy) (mapM buildInferExps)
  stmtsTyTms <- mapM stmtsAndTmEqLast $ expsTyTms
  mapM_ appendStmt (concat stmtsTyTms)
  ------------------------------------------------------------------------------
  -- a = b >> check ctx TyDef expr
  expsDef <- uses (juds.notDefsTy) (mapM buildCheckExps)
  mapM_ appendExp (concat expsDef)

  -- check metas for equality after all of them are added
  genReturnSt fs (conclusion ax)
  uses doStmts doE

-- >>= \t -> remvars (Metavar) this
stmtsAndMetaLast :: (MetaVar, [Exp]) -> BldRM [Stmt]
stmtsAndMetaLast (_, []) = throwError "stmtsAndMetaLast must be called with at least one expr"
-- this is a metaVar def
stmtsAndMetaLast (m, x:[]) = do
  vn <- fresh
  return []
stmtsAndMetaLast (m, x:xs) = do
  xs' <- stmtsAndMetaLast (m, xs)
  return $ Qualifier x : xs'

-- >>= \t -> remvars (Metavar) this
stmtsAndTmEqLast :: [Exp] -> BldRM [Stmt]
stmtsAndTmEqLast [] = throwError "stmtsAndTmEqLast must be called with at least one expr"
-- this is a metaVar def
stmtsAndTmEqLast (x:[]) = do
  vn <- fresh
  return []
stmtsAndTmEqLast (x:xs) = do
  xs' <- stmtsAndTmEqLast xs
  return $ Qualifier x : xs'


labelJudgement :: Judgement -> BldRM ()
labelJudgement jud = undefined

-- ctx, ctx, ctx, a = b >> infer cxzzczc
buildInferExps :: Judgement -> [Exp]
buildInferExps = undefined

buildCheckExps :: Judgement -> [Exp]
buildCheckExps = undefined
--------------------------------------------------------------------------------
-- first vars are already used
-- also axioms are always of the form like this
correctFresh :: Axiom -> BldRM ()
correctFresh (Axiom _ _ _ (Statement _ (FunApp _ lst) _)) = do
  populateSt lst
  where
    populateSt ((ct, Meta (MetaVar _ nm)):xs) = do
      v <- fresh
      metas %= updateMap (MetaVar ct nm) (var (name v))
      populateSt xs
    populateSt [] = return ()
    populateSt _ = throwError "Can't have a non metavariable in an axiom"

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
  let ex' = eqCheckExp (toScope (length ct2) ex) y
  appendExp ex'

  genMetaEq (y' : xs)

-- we take a metavar + its' term and transform it into a metavar in different ctx
-- and return the transformation (it's context manipulation xzy.T -> yxz.T)
-- this is the most difficult function, builds a not scoped repr
conniveMeta :: Ctx -> (Ctx, Exp) -> BldRM Exp
conniveMeta ctx (oldCt, expr) = undefined

--------------------------------------------------------------------------------
genReturnSt :: FunctionalSymbol -> Judgement -> BldRM ()
genReturnSt (FunSym _ _ res) (Statement _ _ Nothing) = do
  appendExp $ retExp (tyCtor $ sortToTyCtor $ getSortName res)
genReturnSt _ (Statement _ _ (Just ty)) = do
  ret <- buildTermExp [] ty
  appendExp $ retExp ret
genReturnSt _ _ = throwError "Can't have anything but funsym in conclusion"

appendExp :: Exp -> BldRM ()
appendExp ex = appendStmt (Qualifier ex)

appendStmt :: Stmt -> BldRM ()
appendStmt st = doStmts %= (++ [st])
--------------------------------------------------------------------------------
-- walk the term and build it var by var
-- untyped => problematic
buildTermExp :: Ctx -> Term -> BldRM Exp
buildTermExp ctx (AST.Var vn) = return $ buildVar ctx vn -- builds up stuff like F(F(F(F(B()))))
buildTermExp ctx (Subst into vn what) = do
  intoE <- buildTermExp (vn:ctx) into
  whatE <- buildTermExp (vn:ctx) what
  return $ inst1 whatE (toScope 1 intoE) -- 1 scope only
buildTermExp ctx (Meta mv) = do
  res <- uses metas (Map.lookup mv)
  case res of
    Nothing -> throwError $ "MetaVar " ++ show mv ++ " not found in terms"
    -- we store metavar values as list, but we fold it
    Just res' -> conniveMeta ctx (res' !! 0)
buildTermExp ctx (FunApp nm lst) = do
  -- see ctx ++ ctx', differs from our treatment in subst (*)
  lst' <- mapM (\(ctx', tm) -> buildTermExp (ctx ++ ctx') tm) lst
  let lst'' = (\((ctx', _), ex) -> toScope (length ctx') ex) <$> zip lst lst'
  return $ appFunS nm lst''

-- (*) x.T -> lam(S, z.(lam(S, y.T[x:=true][v:=false]))) -- xvzy.T
--         ctx: z -> z+y -> v+zy -> x+vzy


--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

appFunS :: VarName -> [Exp] -> Exp
appFunS nm lst = undefined

retExp :: Exp -> Exp
retExp ex = undefined

eqCheckExp :: Exp -> Exp -> Exp
eqCheckExp ex1 ex2 = undefined

tyCtor :: String -> Exp
tyCtor st = Con (UnQual (Ident st))

-- txyz : x = F(F(B()))
buildVar :: Ctx -> VarName -> Exp
buildVar = undefined

inst1 :: Exp -> Exp -> Exp -- generates instantiate1 v x code
inst1 ex1 ex2 = undefined

toScope :: Int -> Exp -> Exp
toScope n ex = undefined

fromScope :: Int -> Exp -> Exp
fromScope n ex = undefined

swap :: (Int, Int) -> Exp -> Exp
swap (n,m) e
  | n == m = e
  | n > m = swap (m,n) e
  | otherwise = undefined

rem :: Int -> Exp -> Exp
rem n ex = undefined

add :: Int -> Exp -> Exp
add n ex = undefined

initJuds :: Juds
initJuds = Juds [] [] [] []

runBM :: BldRM a -> ErrorM a
runBM mon = evalStateT mon (Q 0 Map.empty [] initJuds initGen)

fresh :: BldRM VName
fresh = do
  i <- gets _count
  count += 1
  return (vars !! i)

updateMap :: MetaVar -> v -> Map.Map MetaVar [(Ctx,v)] -> Map.Map MetaVar [(Ctx,v)]
updateMap k v m = case Map.lookup k m of
  Nothing -> Map.insert k [(mContext k,v)] m
  (Just vs) -> Map.insert k ((mContext k,v):vs) m

generator :: VarName -> Exp -> Stmt
generator vn ex = Generator (PVar $ name vn) ex

---
