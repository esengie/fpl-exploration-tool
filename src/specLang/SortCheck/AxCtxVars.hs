module SortCheck.AxCtxVars (
  checkCtxVars
) where

import Control.Monad.Trans.State.Lazy
import Control.Monad.Except (throwError)
import Control.Monad.Trans.Class (lift)
import Control.Lens
import Data.Maybe (isJust)
import Control.Monad (when, unless)

import qualified Data.Map as Map

import AST
import AST.Axiom as Axiom
import SortCheck.SymbolTable as SymbolTable
import SortCheck.Term(checkStab)
import SortCheck.Judgement
import SortCheck.Forall

--------------------------------------------------------------------------------

-- We have f(B, x.A, yz.S) + [y:0, z:1], [x:0]
-- |- z : S -- all vars are to the left of S, can have cycles. say x:R |- t1:S, y:S |- t2:R
-- forall z.B, z.T -- z : A |- B : T -- check z uses only leftmost guys
-- same with |- lam(t, x.(lam tm, y.k)) -- y : tm -- all metas here are to the left of y.k

checkCtxVars :: [MetaVar] -> Axiom -> SortCheckM ()
checkCtxVars mvs juds = do
  -- let ct = jud^.jContext
  return ()
  -- mvs' <- expandWithTys mvs prems

---
