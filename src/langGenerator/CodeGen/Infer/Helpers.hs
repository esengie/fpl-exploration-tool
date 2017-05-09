module CodeGen.Infer.Helpers
  where

import Data.List
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
appFunS nm lst = appFun (var $ name nm) lst

retExp :: Exp -> Exp
retExp ex = app (var $ name "return") $ ex

eqCheckExp :: Exp -> Exp -> Exp
eqCheckExp ex1 ex2 = app (app (var (name "checkEq")) ex1) ex2

tyCtor :: String -> Exp
tyCtor st = Con (UnQual (Ident st))

-- txyz : x = F(F(B()))
buildVar :: Ctx -> VarName -> ErrorM Exp
buildVar ct vn =
  let fs = repeat (tyCtor "F")
      bb = (app (tyCtor "B") unit_con)
      tyCt = tyCtor "Var"
  in case elemIndex vn ct of
      Nothing -> throwError $ "Varible is not in context, sortchecking error!"
      Just n -> return $ foldr (\x y -> app x y) bb (tyCt : take (length ct - 1 - n) fs)

inst1 :: Exp -> Exp -> Exp -- generates instantiate1 v x code
inst1 ex1 ex2 = app (app (var (name "instantiate1")) $ ex1)
                    (toScope 1 ex2)

toScope :: Int -> Exp -> Exp
toScope n ex | n == 1 = app (var (name "toScope")) ex
             | otherwise = app (var (name $ "toScope" ++ show n)) ex

fromScope :: Int -> Exp -> Exp
fromScope n ex | n == 1 = app (var (name "fromScope")) ex
               | otherwise = app (var (name $ "fromScope" ++ show n)) ex

swap :: (Int, Int) -> Exp -> Exp
swap (n,m) ex
  | n == m = ex
  | n > m = swap (m,n) ex
  | otherwise = app (var (name $ "swap" ++ show n ++ "'"  ++ show m)) ex

rem :: Int -> Exp -> Exp
rem n ex = app (var (name $ "rem" ++ show n)) ex

add :: Int -> Exp -> Exp
add n ex = app (var (name $ "add" ++ show n)) ex

generator :: VarName -> Exp -> Stmt
generator vn ex = Generator (PVar $ name vn) ex

---
