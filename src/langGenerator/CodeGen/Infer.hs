module CodeGen.Infer
-- (
--   genInfer
-- )
where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except (throwError, lift)
import Language.Haskell.Exts.Simple
import Control.Lens
import Debug.Trace

import qualified Data.Set as Set
import qualified Data.Map as Map

import SortCheck
import AST hiding (Var)
import qualified AST(Term(Var))
import AST.Axiom

import CodeGen.Common
import CodeGen.MonadInstance (funToPat)
import CodeGen.Infer.RightSide (buildRight)

--------------------------------------------------------------------------
bri = buildRight
fFunS = (FunSym "f" [DepSort "asd" 12, DepSort "a" 22] (DepSort "as" 1))
fTm = Subst (AST.Var "asd") "asd" (AST.Var "er")
fJud = Equality [] fTm fTm Nothing
fAx = Axiom "as" [] [] fJud
--------------------------------------------------------------------------

funInfer :: [[Pat]] -> [Exp] -> Decl
funInfer pat exps = FunBind $ zipWith (\x y -> Match (Ident "infer") x (UnGuardedRhs y) Nothing) pat exps

funLeft :: FunctionalSymbol -> [Pat]
funLeft f = [PVar (Ident "ctx"), PParen $ funToPat f]

errStarStar :: String -> Exp
errStarStar str = App (Var (UnQual (Ident "report"))) (Lit (String str))

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
  let fRight' = (\f -> buildRight f $ (unJust . funToAx st) f) <$> fsyms
  fRight <- lift . lift $ sequence fRight'

  --- Gather and build a resulting function
  let res = funInfer (varL : sortsL ++ fLeft) (varR : sortsR ++ fRight)
  lst <- get
  (_ , n) <- getDecl "infer"
  put lst{decls = replace n [res] (decls lst)}





---
