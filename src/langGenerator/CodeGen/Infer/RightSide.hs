module CodeGen.Infer.RightSide(
  buildRight
) where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except (throwError, lift)
import Language.Haskell.Exts.Simple
import Debug.Trace

import qualified Data.Set as Set
import qualified Data.Map as Map

import SortCheck
import AST hiding (Var)
import AST.Axiom

import CodeGen.Common hiding (count)

data Q = Q {
  count :: Int
}

type BldRM = StateT Q (ErrorM)

buildRight :: FunctionalSymbol -> Axiom -> ErrorM Exp
buildRight fs ax = runBM (buildRight' fs ax)

buildRight' :: FunctionalSymbol -> Axiom -> BldRM Exp
buildRight' fs ax = do
  correctFresh fs
  fr <- fresh
  return (Var (UnQual $ sym fr))
  -- find all used Metavars + check for equality where needed
  -- First check all guys of the smth : T - build up the map (metavars : Term)

-- first vars are already used
correctFresh :: FunctionalSymbol -> BldRM ()
correctFresh (FunSym _ lst _) = replicateM_ (length lst) fresh

runBM :: BldRM a -> ErrorM a
runBM mon = evalStateT mon (Q 0)

fresh :: BldRM VName
fresh = do
  i <- gets count
  modify (\st -> st{ count = i + 1})
  return (vars !! i)

---
