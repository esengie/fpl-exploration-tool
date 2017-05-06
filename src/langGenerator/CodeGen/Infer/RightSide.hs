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
import AST.Axiom hiding (name)

import CodeGen.Common hiding (count)

data Q = Q {
  _count :: Int,
  -- metaVar as in forall x.T -> termExp
  _metas :: Map.Map MetaVar [Exp],
  _doExps :: [Exp], -- this will be concatted

  -- we define some metavars on the right of :, others we need to check
  _juds  :: Juds,
  -- various counters - the bigger monad will have to use this
  _toGen :: ToGen
}

data Juds = Juds {
  _metaTyDefs :: [Judgement], -- some var will be added to the metas map
  _notDefsTy :: [Judgement], -- here it will not, so
  _notDefsVar :: [Exp]
}

initJuds :: Juds
initJuds = Juds [] [] []

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


  -- genCheckMetaEq
  -- genReturnExp


  fr <- fresh
  return (Var (UnQual $ sym fr))
  -- find all used Metavars + check for equality where needed
  -- First check all guys of the smth : T - build up the map (metavars : Term)

-- first vars are already used
-- also axioms are always of the form like this
correctFresh :: Axiom -> BldRM ()
correctFresh (Axiom _ _ _ (Statement _ (FunApp _ lst) _)) = do
  populateSt lst
  where
    populateSt ((ct, Meta (MetaVar _ nm)):xs) = do
      v <- fresh
      metas %= Map.insert (MetaVar ct nm) ([var (name v)])
      populateSt xs
    populateSt [] = return ()
    populateSt _ = throwError "Can't have a non metavariable in an axiom"

--------------------------------------------------------------------------------
-- Check terms for equality
genCheckMetaEq :: BldRM ()
genCheckMetaEq = do
  ms <- gets _metas
  let res = genEq <$> ms
  metas .= res

genMetaEq :: [Exp] -> BldRM [Exp]
genMetaEq [] = return []
genEq (x : []) = return x
genEq (x : y : xs) = do

  genEq (y : xs)

--------------------------------------------------------------------------------

runBM :: BldRM a -> ErrorM a
runBM mon = evalStateT mon (Q 0 Map.empty [] initJuds initGen)

fresh :: BldRM VName
fresh = do
  i <- gets _count
  count += 1
  return (vars !! i)

---
