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
import AST hiding (Var, name)
import qualified AST (Term(Var))
import AST.Axiom hiding (name)

import CodeGen.Common hiding (count)

data Q = Q {
  _count :: Int,
  -- metaVar as in forall x.T -> termExp
  _metas :: Map.Map MetaVar [(Ctx, Exp)],
  _doExps :: [Exp], -- this will be concatted

  -- we define some metavars on the right of :, others we need to check
  _juds  :: Juds,
  -- various counters - the outer monad will have to use this
  _toGen :: ToGen
}

data Juds = Juds {
  _metaTyDefs :: [Judgement], -- some var will be added to the metas map
  _notDefsTy :: [Judgement], -- here it will not, so
  _notDefsVar :: [Exp]
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
  ---------------------- xy.T == yx.T ?? - no


  genCheckMetaEq
  genReturnExp (conclusion ax)
  fr <- fresh
  return (Var (UnQual $ sym fr))
  -- find all used Metavars + check for equality where needed
  -- First check all guys of the smth : T - build up the map (metavars : Term)

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
  genEqCheck (toScope (length ct2) ex) y                               -- !! gen
  genMetaEq (y' : xs)

genEqCheck :: Exp -> Exp -> BldRM ()
genEqCheck ex1 ex2 = undefined

-- we take a metavar + its' term and transform it into a metavar in different ctx
-- and return the transformation (it's context manipulation xzy.T -> yxz.T)
-- this is the most difficult function, builds a not scoped repr
conniveMeta :: Ctx -> (Ctx, Exp) -> BldRM Exp
conniveMeta ctx (oldCt, expr) = undefined

--------------------------------------------------------------------------------
genReturnExp :: Judgement -> BldRM ()
genReturnExp (Statement _ _ Nothing) = undefined
genReturnExp (Statement _ _ (Just ty)) = do
  ret <- buildTermExp [] ty
  doExps %= (++ [retExp ret]) -- append to list
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
initJuds = Juds [] [] []

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

---
