module CodeGen.MonadInstance(
  genMonad,
  funToPat
) where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except (throwError, lift)
import Language.Haskell.Exts.Simple
import Control.Lens hiding (op)

import qualified Data.Map as Map

import SortCheck.SymbolTable
import AST.Term hiding (Var, name)

import CodeGen.Common

inApp a op' b = infixApp (varExp a) (qvarOp op') (varExp b)
  where qvarOp nm = op (sym nm)
        varExp nm = var (name nm)

-- f(x.A, x.B, y.t) ---> ... (F v1 v2 v3) = ...
funToPat :: FunctionalSymbol -> Pat
funToPat (FunSym nm lst _) = PApp (UnQual (Ident (caps nm)))
                                  (map (PVar . Ident) (take (length lst) vars))

infixMatch :: FunctionalSymbol -> Exp -> Match
infixMatch f@(FunSym nm lst _) exp = InfixMatch (funToPat f)
                                            (Symbol ">>=")
                                            [PVar (Ident fname)]
                                            (UnGuardedRhs exp)
                                            Nothing

boundBind :: FunctionalSymbol -> Exp
boundBind f@(FunSym nm lst _) = foldl App (Con (UnQual (Ident $ caps nm))) (map smart (zip lst vars))
  where
    vn x = var (name x)
    smart (srt, nm) | getSortDepth srt == 0 = inApp nm ">>=" fname
                    | getSortDepth srt == 1 = inApp nm ">>>=" fname
                    | otherwise = appFun (vn $ "ap" ++ (show $ getSortDepth srt)) (vn <$> [nm, fname])

bindVarA :: Match
bindVarA = infixMatch (FunSym "Var" [varSort] varSort)
                      (App (Var (UnQual (Ident fname))) (Var (UnQual (Ident $ vars !! 0))))

monadTerm :: [Match] -> Decl
monadTerm lst = InstDecl Nothing
                  (IRule Nothing Nothing
                         (IHApp (IHCon (UnQual (Ident "Monad"))) (TyCon (UnQual (Ident "Term")))))
                  (Just [InsDecl (FunBind lst)])

genMonad :: GenM ()
genMonad = do
  st <- ask
  let sorts = (\x -> FunSym (sortToTyCtor x) [] varSort) <$> sortsWO_tm st
  let matches = (\f -> infixMatch f (boundBind f)) <$> Map.elems (st^.SortCheck.SymbolTable.funSyms) ++ sorts
  let monadInst = monadTerm (bindVarA : matches)

  replaceDecls "instance Monad Term" [monadInst]








---
