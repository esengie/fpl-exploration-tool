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
appFunS nm lst = foldl (\x y -> app x $ paren y) (var $ name nm) lst

retExp :: Exp -> Exp
retExp ex = app (var $ name "return") $ paren ex

eqCheckExp :: Exp -> Exp -> Exp
eqCheckExp ex1 ex2 = app (app (var (name "checkEq")) $ paren ex1) $ paren ex2

tyCtor :: String -> Exp
tyCtor st = Con (UnQual (Ident st))

-- txyz : x = F(F(B()))
buildVar :: Ctx -> VarName -> ErrorM Exp
buildVar ct vn =
  let fs = repeat (tyCtor "F")
      bb = paren (app (tyCtor "B") unit_con)
      tyCt = tyCtor "Var"
  in case elemIndex vn ct of
      Nothing -> throwError $ "Varible is not in context, sortchecking error!"
      Just n -> return $ foldr (\x y -> paren $ app x y) bb (tyCt : take (length ct - 1 - n) fs)

inst1 :: Exp -> Exp -> Exp -- generates instantiate1 v x code
inst1 ex1 ex2 = app (app (var (name "instantiate1")) $ paren ex1)
                    (paren $ toScope 1 ex2)

toScope :: Int -> Exp -> Exp
toScope n ex | n == 1 = app (var (name "toScope")) $ paren ex
             | otherwise = app (var (name $ "toScope" ++ show n)) $ paren ex

fromScope :: Int -> Exp -> Exp
fromScope n ex | n == 1 = app (var (name "fromScope")) $ paren ex
               | otherwise = app (var (name $ "fromScope" ++ show n)) $ paren ex

swap :: (Int, Int) -> Exp -> Exp
swap (n,m) ex
  | n == m = ex
  | n > m = swap (m,n) ex
  | otherwise = app (var (name $ "swap" ++ show n ++ "'"  ++ show m)) $ paren ex

rem :: Int -> Exp -> Exp
rem n ex = app (var (name $ "rem" ++ show n)) $ paren ex

add :: Int -> Exp -> Exp
add n ex = app (var (name $ "add" ++ show n)) $ paren ex

generator :: VarName -> Exp -> Stmt
generator vn ex = Generator (PVar $ name vn) ex

---
