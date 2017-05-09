module CodeGen.Infer.Exprs
  where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except (throwError, lift)
import Control.Lens hiding (op)
import Language.Haskell.Exts.Simple
import Debug.Trace

import qualified Data.Set as Set
import qualified Data.Map as Map

import SortCheck
import AST hiding (Var, name, Name)
import qualified AST (Term(Var))
import AST.Axiom hiding (name)

import CodeGen.Common hiding (count)
import CodeGen.Infer.Common
import CodeGen.Infer.Helpers

-- [x,y,z] -> [x, x.y, xy.z]
-- but it's types
buildConsCtxExps :: [(VarName, Term)] -> BldRM [Exp]
buildConsCtxExps [] = return []
buildConsCtxExps xs = undefined

-- given x,y,z,r -> check ctx TyDef x, check (consCtx x) TyDef y ...
buildCtxCheckExps :: [Exp] -> [Exp]
buildCtxCheckExps xs = helper xs []
  where
    helper [] _ = []
    helper (x:xs) ys = appFun checkE [consCtxes (reverse ys), sortToExp tyName, x]
      : helper xs (x:ys)

-- consCtxes ["x","y","z"] -> consCtx z (consCtx y (consCtx x ctx))
consCtxes :: [Exp] -> Exp
consCtxes ctxExps = foldr (\x y -> appFun consCtxE [x,y]) ctxE (reverse ctxExps)

checkHasType :: Judgement -> BldRM Term
checkHasType j = case jType j of Nothing -> throwError $ show j ++ " has no type"
                                 Just x -> return x
checkHasNoType :: Judgement -> BldRM ()
checkHasNoType j = case jType j of Nothing -> return ()
                                   Just _ -> throwError $ show j ++ " has type"

-- ctx, ctx, ctx, a = b >> infer cxzzczc
-- x, y |- infer (consCtx y (consCtx x))
buildInferExps :: Judgement -> BldRM [Exp]
buildInferExps jud = do
  checkHasType jud
  -- if we have equality, then return a func: \x -> a = b >> x (+ exp & term)
  (f, ex, _) <- buildEq jud
  ctxExps <- buildConsCtxExps (_jContext jud)
  let inf = appFun infE [consCtxes ctxExps, ex]
  let others = buildCtxCheckExps ctxExps
  return $ others ++ [f inf]

buildEq :: Judgement -> BldRM (Exp -> Exp, Exp, Term)
buildEq j@(Statement _ tm _) = do
  ex <- buildTermExp (judCtx j) tm
  return (id, ex, tm)
buildEq (Reduct{}) = throwError $ "Reduct in infer is an implem error"
buildEq j@(Equality _ l r _) = do
  let ct = judCtx j
  ex <- buildTermExp ct l
  rex <- buildTermExp ct r
  let eq = eqCheckExp ex rex
  return (\x -> infixApp eq (op (sym ">>")) x , ex, l)

infE = var (name "infer")
checkE = var (name "check")
ctxE = var (name "ctx")
consCtxE = var (name "consCtx")
sortToExp nm = tyCtor $ sortToTyCtor nm

buildCheckExps :: Judgement -> BldRM [Exp]
buildCheckExps jud = do
  checkHasNoType jud
  (f, ex, tm) <- buildEq jud
  ctxExps <- buildConsCtxExps (_jContext jud)

  st <- termSort tm
  let inf = if st == tmName
               then appFun infE [consCtxes ctxExps, ex]
               else appFun checkE [consCtxes ctxExps, sortToExp st,  ex]

  let others = buildCtxCheckExps ctxExps
  return $ others ++ [f inf]


termSort :: Term -> BldRM SortName
termSort (AST.Var _) = return tmName
termSort (Meta mv) = do
  st <- uses foralls (Map.lookup mv)
  case st of
    Nothing -> throwError $ "error in sortchecking, metavar not in foralls " ++ show mv
    Just s -> return (getSortName s)
termSort (Subst tm _ _) = termSort tm
termSort (FunApp nm _) = do
  st <- uses funsyms (Map.lookup nm)
  case st of
    Nothing -> throwError $ "error in sortchecking, funsym not in funsyms " ++ show nm
    Just s -> return (getSortName $ result s)

-- we take a metavar + its' term and transform it into a metavar in different ctx
-- and return the transformation (it's context manipulation xzy.T -> yxz.T)
-- this is the most difficult function, builds a not scoped repr
conniveMeta :: Ctx -> (Ctx, Exp) -> BldRM Exp
conniveMeta ctx (oldCt, expr) = undefined

trimMeta :: Ctx -> (Ctx, Exp) -> BldRM (Ctx, Exp)
trimMeta ctx (oldCt, expr) = undefined

--------------------------------------------------------------------------------
-- walk the term and build it var by var
-- returns as unscoped as can be
buildTermExp :: Ctx -> Term -> BldRM Exp
buildTermExp ctx (AST.Var vn) = lift $ buildVar ctx vn -- builds up stuff like F(F(F(F(B()))))
buildTermExp ctx (Subst into vn what) = do
  intoE <- buildTermExp (vn:ctx) into
  whatE <- buildTermExp (vn:ctx) what
  return $ inst1 whatE (toScope 1 intoE) -- 1 scope only
buildTermExp ctx (Meta mv) = do
  res <- uses metas (Map.lookup mv)
  case res of
    Nothing -> throwError $ "MetaVar " ++ show mv ++ " not found in terms"
    -- we store metavar values as list, but we fold it
    Just res' -> conniveMeta ctx (res' !! 0)
buildTermExp ctx (FunApp nm lst) = do
  -- see ctx ++ ctx', differs from our treatment in subst (*)
  lst' <- mapM (\(ctx', tm) -> buildTermExp (ctx ++ ctx') tm) lst
  let lst'' = (\((ctx', _), ex) -> toScope (length ctx') ex) <$> zip lst lst'
  return $ appFunS nm lst''

-- (*) x.T -> lam(S, z.(lam(S, y.T[x:=true][v:=false]))) -- xvzy.T
--         ctx: z -> z+y -> v+zy -> x+vzy











---