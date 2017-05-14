module CodeGen.RightSide.Helpers
  where

import Data.List
import Control.Monad.Except (throwError, lift)
import Control.Lens
import Language.Haskell.Exts.Simple

import AST hiding (Var, name, Name)

import CodeGen.Common hiding (count)

appFunS :: VarName -> [Exp] -> Exp
appFunS nm lst = appFun (var $ name (caps nm)) lst

retExp :: Exp -> Exp
retExp ex = app (var $ name "pure") $ ex

eqCheckExp :: Exp -> Exp -> Exp
eqCheckExp ex1 ex2 = app (app (var (name "checkEq")) ex1) ex2

tyCtor :: String -> Exp
tyCtor st = Con (UnQual (Ident st))

buildCnt :: Int -> Exp
buildCnt 0 = tyCtor "Bot"
buildCnt n = app (tyCtor "U") $ buildCnt (n-1)

buildCntP :: Int -> Pat
buildCntP 0 = PWildCard
buildCntP n = pApp (name "U") [buildCntP (n-1)]


-- txyz : x = F(F(B()))
buildVar :: Ctx -> VarName -> ErrorM Exp
buildVar ct vn =
  let fs = repeat (tyCtor "F")
      bb = (tyCtor "B")
      tyCt = tyCtor "Var"
  in case elemIndex vn ct of
      Nothing -> throwError $ "Varible is not in context, sortchecking error!"
      Just n -> return $ foldr (\x y -> app x y) bb (tyCt : take (length ct - 1 - n) fs)

inst1 :: Exp -> Exp -> Exp -- generates instantiate v x code
inst1 ex1 ex2 = appFun (var (name "instantiate")) [ex1, ex2]

nf :: Int -> Exp -> Exp
nf n ex | n < 1 = app (var (name "nf")) ex
      | otherwise = app (var (name $ "nf" ++ show n)) ex

toScope :: Int -> Exp -> Exp
toScope n ex | n < 1 = ex
      | n == 1 = app (var (name "toScope")) ex
      | otherwise = app (var (name $ "toScope" ++ show n)) ex

fromScope :: Int -> Exp -> Exp
fromScope n ex | n < 1 = ex
      | n == 1 = app (var (name "fromScope")) ex
      | otherwise = app (var (name $ "fromScope" ++ show n)) ex

swap :: (Int, Int) -> Exp -> Exp
swap (n,m) ex
  | n == m = ex
  | n > m = swap (m,n) ex
  | otherwise = appFun rtE [var (name $ "swap" ++ show n ++ "'"  ++ show m), ex]

rmv :: Int -> Exp
rmv n = app travE $ var (name $ "rem" ++ show n)

add :: Int -> Exp -> Exp
add n ex = appFun rtE [var (name $ "add" ++ show n), ex]

generator :: VarName -> Exp -> Stmt
generator vn ex = Generator (PVar $ name vn) ex

infE = var (name "infer")
checkE = var (name "checkT")
ctxE = var (name "ctx")
consCtxE = var (name "consCtx")
rtE = var (name "rt")
travE = var (name "traverse")
sortToExp nm = tyCtor $ sortToTyCtor nm

---
