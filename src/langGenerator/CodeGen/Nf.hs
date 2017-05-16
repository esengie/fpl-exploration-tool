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
import CodeGen.RightSide.Nf (buildRightNf)
import CodeGen.RightSide.Helpers (tyCtor)


funNf' :: [Match] -> Decl
funNf' ms = FunBind (ms ++ [Match (Ident "nf'")
                                  [PWildCard, pvar (name "x")]
                                  (UnGuardedRhs $ var (name "x"))
                                  Nothing])

fsymLeft :: FunctionalSymbol -> [Pat]
fsymLeft f = [funToPat f]

genNf :: GenM ()
genNf = do
  st <- ask

  --- Var work
  let varL = fsymLeft (FunSym "Var" [varSort] varSort)
  let varR = app (tyCtor "Var") (var $ name $ vars !! 0)
  ------
  --- TyDefs
  let sortsL = (\x -> fsymLeft $ FunSym (sortToTyCtor x) [] varSort) <$> sortsWO_tm st
  let sortsR = (var . name . sortToTyCtor) <$> sortsWO_tm st
  ------

  let fsyms = Map.elems (st^.SortCheck.funSyms)
  let fLeft = fsymLeft <$> fsyms
  -- We've checked our lang, can unJust
  let fRight' = (\f -> do reds <- reducts st f
                          buildRightNf f reds) <$> fsyms
  fRight <- lift . lift $ sequence fRight'
  let nfRs = fst <$> fRight
  let nf'Rs = concat (snd <$> fRight)

  --- Gather and build a resulting function
  let res = funLeft "nf" (varL : sortsL ++ fLeft) (varR : sortsR ++ nfRs)
  replaceDecls "nf" [res]
  replaceDecls "nf'" [funNf' nf'Rs]




---
