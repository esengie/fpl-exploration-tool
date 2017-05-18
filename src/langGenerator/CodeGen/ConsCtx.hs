module CodeGen.ConsCtx
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
import CodeGen.RightSide.Common
import CodeGen.RightSide.Exprs (buildStabilityExp)

consCtx :: Exp -> Decl
consCtx ex = FunBind ([Match (Ident "consCtx")
                                    [pvar (name "x")]
                                    (UnGuardedRhs $ rhs ex)
                                    Nothing])
  where
    rhs ex = undefined

errCons :: Exp -> Exp
errCons = App (Var (UnQual (Ident "consErr")))

genConsCtx :: GenM ()
genConsCtx = do
  stab <- reader _stabs
  stab' <- lift . lift $ runBM Map.empty $ buildStabilityExp stab
  case stab' of
    Nothing -> return ()
    Just x -> do
      let res = consCtx x
      replaceDecls "consCtx" [res]



---
