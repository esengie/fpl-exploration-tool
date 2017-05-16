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
import AST hiding (Var, name)
import qualified AST(Term(Var))
import AST.Axiom hiding (name)

import CodeGen.Common
import CodeGen.MonadInstance (funToPat)
import CodeGen.RightSide.Infer (buildRightInfer)

--------------------------------------------------------------------------
bri = buildRightInfer
fMap = Map.insert "f" fFunS Map.empty
fFunS = (FunSym "f" [DepSort "asd" 12, DepSort "a" 22] (DepSort "as" 1))
fTm = Subst (AST.Var "asd") "asd" (AST.Var "er")
fJud = Statement [] fTm Nothing
fAx = Axiom "as" Nothing [] [] fJud
--------------------------------------------------------------------------

fsymLeft :: FunctionalSymbol -> [Pat]
fsymLeft f = [PVar (Ident "ctx"), funToPat f]

errStarStar :: String -> Exp
errStarStar str = App (Var (UnQual (Ident "report"))) (Lit (String str))

genInfer :: GenM ()
genInfer = do
  st <- ask

  --- Var work
  let varL = fsymLeft (FunSym "Var" [varSort] varSort)
  let varR = app (var $ name "ctx") (var $ name $ vars !! 0)
  ------
  --- Errors of type ty(*) = *
  let sortsL = (\x -> fsymLeft $ FunSym (sortToTyCtor x) [] varSort) <$> sortsWO_tm st
  let sortsR = (errStarStar . sortToTyCtor) <$> sortsWO_tm st
  ------

  let fsyms = Map.elems (st^.SortCheck.funSyms)
  let fLeft = fsymLeft <$> fsyms
  -- We've checked our lang, can unJust
  let fRight' = (\f -> buildRightInfer (st^.SortCheck.funSyms)
                                        f
                                        $ (unJust . funToAx st) f)
                                       <$> fsyms
  fRight <- lift . lift $ sequence fRight'

  --- Gather and build a resulting function
  let res = funLeft "infer" (varL : sortsL ++ fLeft) (varR : sortsR ++ fRight)
  replaceDecls "infer" [res]




---
