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
  count :: Int,
  -- metaVar as in forall x.T -> termExp
  metas :: Map.Map MetaVar [Exp],

  doExps :: [Exp],
  metaDefs :: [Judgement],
  notDefs :: [Judgement]
}

type BldRM = StateT Q (ErrorM)

buildRight :: FunctionalSymbol -> Axiom -> ErrorM Exp
buildRight fs ax = runBM (buildRight' fs ax)

buildRight' :: FunctionalSymbol -> Axiom -> BldRM Exp
buildRight' fs ax = do
  correctFresh ax
  ---------------------- xy.T == yx.T ?? - no
  -- genCheckMetaEq


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
  where populateSt ((ct, (MetaVar )))

runBM :: BldRM a -> ErrorM a
runBM mon = evalStateT mon (Q 0 Map.empty [] [] [])

fresh :: BldRM VName
fresh = do
  i <- gets count
  modify (\st -> st{ count = i + 1})
  return (vars !! i)

---
