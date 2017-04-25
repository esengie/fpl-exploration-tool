module CodeGen.Infer(
  genInfer
) where

import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Lazy
import Control.Monad.Except (throwError, lift)
import Language.Haskell.Exts.Simple
import Control.Lens
import Debug.Trace

import qualified Data.Set as Set
import qualified Data.Map as Map

import SortCheck
import AST hiding (Var)

import CodeGen.Common
import CodeGen.MonadInstance (funToPat, vars)

funInfer :: [[Pat]] -> [Exp] -> Decl
funInfer pat exps = FunBind $ zipWith (\x y -> Match (Ident "infer") x (UnGuardedRhs y) Nothing) pat exps

funLeft :: FunctionalSymbol -> [Pat]
funLeft f = [PVar (Ident "ctx"), PParen $ funToPat f]

genInfer :: GenM ()
genInfer = do
  st <- ask

  let sorts = (\x -> FunSym (sortToTyCtor x) [] varSort) <$> sortsWO_tm st
  let lefts = funLeft <$> Map.elems (st^.SortCheck.funSyms) ++ sorts
  let rights = take (length lefts) (repeat ExprHole)

  lst <- lift get
  (_ , n) <- getDecl "infer"
  lift $ put (replace n [funInfer lefts rights] lst)






---
