module CodeGen.Infer
-- (
--   genInfer
-- )
where

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
import AST.Axiom

import CodeGen.Common
import CodeGen.MonadInstance (funToPat)

funInfer :: [[Pat]] -> [Exp] -> Decl
funInfer pat exps = FunBind $ zipWith (\x y -> Match (Ident "infer") x (UnGuardedRhs y) Nothing) pat exps

funLeft :: FunctionalSymbol -> [Pat]
funLeft f = [PVar (Ident "ctx"), PParen $ funToPat f]

errStarStar :: String -> Exp
errStarStar str = App (Var (UnQual (Ident "report"))) (Lit (String str))

buildRight :: FunctionalSymbol -> Axiom -> Exp
buildRight = undefined
  -- find all used Metavars + check for equality where needed
  -- First check all guys of the smth : T - build up the map (metavars : Term)

genInfer :: GenM ()
genInfer = do
  st <- ask

  --- Var work
  let varL = funLeft (FunSym "Var" [varSort] varSort)
  let varR = ExprHole
  ------
  --- Errors of type ty(*) = *
  let sortsL = (\x -> funLeft $ FunSym (sortToTyCtor x) [] varSort) <$> sortsWO_tm st
  let sortsR = (errStarStar . sortToTyCtor) <$> sortsWO_tm st
  ------

  let fsyms = Map.elems (st^.SortCheck.funSyms)
  let fLeft = funLeft <$> fsyms
  -- We've checked our lang, can unJust
  let fRight = (\f -> buildRight f $ (unJust . funToAx st) f) <$> fsyms

  --- Gather and build a resulting function
  let res = funInfer (varL : sortsL ++ fLeft) (varR : sortsR ++ fRight)
  lst <- lift get
  (_ , n) <- getDecl "infer"
  lift $ put lst{decls = replace n [res] (decls lst)}





---
