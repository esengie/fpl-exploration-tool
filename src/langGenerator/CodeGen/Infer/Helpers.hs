module CodeGen.Infer.Helpers
  where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except (throwError, lift)
import Control.Lens
import Language.Haskell.Exts.Simple

import qualified Data.Set as Set
import qualified Data.Map as Map

import SortCheck
import AST hiding (Var, name, Name)
import qualified AST (Term(Var), Name(..))
import AST.Axiom hiding (name)

import CodeGen.Common hiding (count)
import CodeGen.Infer.Common

judCtx :: Judgement -> Ctx
judCtx jud = jud^.jContext.to (map fst)

appFunS :: VarName -> [Exp] -> Exp
appFunS nm lst = undefined

retExp :: Exp -> Exp
retExp ex = undefined

eqCheckExp :: Exp -> Exp -> Exp
eqCheckExp ex1 ex2 = undefined

tyCtor :: String -> Exp
tyCtor st = Con (UnQual (Ident st))

-- txyz : x = F(F(B()))
buildVar :: Ctx -> VarName -> Exp
buildVar = undefined

inst1 :: Exp -> Exp -> Exp -- generates instantiate1 v x code
inst1 ex1 ex2 = undefined

toScope :: Int -> Exp -> Exp
toScope n ex = undefined

fromScope :: Int -> Exp -> Exp
fromScope n ex = undefined

swap :: (Int, Int) -> Exp -> Exp
swap (n,m) e
  | n == m = e
  | n > m = swap (m,n) e
  | otherwise = undefined

rem :: Int -> Exp -> Exp
rem n ex = undefined

add :: Int -> Exp -> Exp
add n ex = undefined

generator :: VarName -> Exp -> Stmt
generator vn ex = Generator (PVar $ name vn) ex

---
