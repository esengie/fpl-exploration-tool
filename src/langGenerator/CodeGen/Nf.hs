module CodeGen.Nf
-- (
--   genInfer
-- )
where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except (throwError, lift)
import Language.Haskell.Exts.Simple
import Control.Lens

import qualified Data.Set as Set
import qualified Data.Map as Map

import SortCheck
import AST hiding (Var, name)
import qualified AST(Term(Var))
import AST.Axiom hiding (name)

import CodeGen.Common
import CodeGen.MonadInstance (funToPat)

buildRight 

--------------------------------------------------------------------------
bri = buildRight
fMap = Map.insert "f" fFunS Map.empty
fFunS = (FunSym "f" [DepSort "asd" 12, DepSort "a" 22] (DepSort "as" 1))
fTm = Subst (AST.Var "asd") "asd" (AST.Var "er")
fJud = Statement [] fTm Nothing
fAx = Axiom "as" [] [] fJud
--------------------------------------------------------------------------

funNf :: [[Pat]] -> [Exp] -> Decl
funNf pat exps = FunBind $ zipWith (\x y -> Match (Ident "nf") x (UnGuardedRhs y) Nothing) pat exps

funLeft :: FunctionalSymbol -> [Pat]
funLeft f = [PParen $ funToPat f]

genNf :: GenM ()
genNf = do
  st <- ask

  --- Var work
  let varL = funLeft (FunSym "Var" [varSort] varSort)
  let varR = (var $ name $ vars !! 0)
  ------
  --- TyDefs
  let sortsL = (\x -> funLeft $ FunSym (sortToTyCtor x) [] varSort) <$> sortsWO_tm st
  let sortsR = (var . name . sortToTyCtor) <$> sortsWO_tm st
  ------

  let fsyms = Map.elems (st^.SortCheck.funSyms)
  let fLeft = funLeft <$> fsyms
  -- We've checked our lang, can unJust
  let fRight' = (\f -> buildRight (st^.SortCheck.funSyms)
                                  f
                                  $ (unJust . funToAx st) f)
                                  <$> fsyms
  fRight <- lift . lift $ sequence fRight'

  --- Gather and build a resulting function
  let res = funNf (varL : sortsL ++ fLeft) (varR : sortsR ++ fRight)
  lst <- get
  (_ , n) <- getDecl "nf"
  put lst{decls = replace n [res] (decls lst)}





---
