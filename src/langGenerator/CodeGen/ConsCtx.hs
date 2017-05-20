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

errCons :: [Exp] -> Exp
errCons = appFun (var $ name "consErr")

genConsCtx :: GenM ()
genConsCtx = do
  stab <- reader _stabs
  stab' <- lift . lift $ runBM Map.empty $ buildStabilityExp stab
  case stab' of
    Nothing -> return ()
    Just x -> do
      let res = consCtx x
      replaceDecls "consCtx" [res]

consCtx :: Exp -> Decl
consCtx lst = funDecl "consCtx" [pvar . name <$> vars] [If check trueCons $ errCons [varX, lst]]
  where
    vars = ["x", "ct", "var"]
    varX = var $ name "x"
    check = appFun (var $ name "elem") [varX, lst]
    trueCons = appFun (var $ name "consCtx'") (var . name <$> vars)


---
