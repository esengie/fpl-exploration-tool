{-# LANGUAGE TemplateHaskell #-}

import Language.Haskell.Exts.Simple

data Term a = Var a | Bool | Lam (Type a) (Scope () Term a)

type Type = Term

instance Monad Term where
  Var a     >>= f = f a
  TyK       >>= f = TyK
  If a t x y >>= f = If (a >>>= f) (t >>= f) (x >>= f) (y >>= f)

-- from reductions
rnf :: Term a -> Term a
rnf (Var a) = Var a
rnf TyK     = TyK
rnf True     = True
rnf False    = False
rnf Bool     = Bool
rnf (If a t x y) = case (rnf t) of
      True  -> (rnf x)
      False -> (rnf y)
      x -> If (toScope $ rnf $ fromScope a) x (rnf x) (rnf y)
rnf (Lam ty t) = Lam (rnf ty) (toScope $ rnf $ fromScope t)
rnf (Pi ty t) = Pi  (rnf ty) (toScope $ rnf $ fromScope t)
rnf (App t1 t2) = case (rnf t1, rnf t2) of
        (Lam ty t1, t2) -> rnf (instantiate1 t2 t1)
        (f, x)  -> App f x

infer :: (Show a, Eq a) => Ctx a -> Term a -> TC (Type a)
infer ctx (Var a) = ctx a
infer ctx TyK  = throwError "Can't have star : star"
infer ctx False = pure Bool
infer ctx Bool = pure TyK
infer ctx (If a t x y) = do
  check ctx Bool t
  check (consCtx (nf Bool) ctx) TyK (fromScope a)
  check ctx (instantiate1 True a) x
  pure $ nf (instantiate1 t a)
infer ctx (Lam ty t) = do
  check ctx TyK ty
  Pi (nf ty) . toScope <$> infer (consCtx (nf ty) ctx) (fromScope t)
  pure TyK
infer ctx (Pi ty t) = do
    check ctx TyK ty
    check (consCtx (nf ty) ctx) TyK (fromScope t)
    pure TyK
infer ctx (App f x) = do
  v <- infer ctx f
  case v of
      Pi ty t -> do
        check ctx ty x
        pure $ instantiate1 x t
      _ -> Left "can't apply non-function"



FunBind [Match (Ident "infer")
               [PVar (Ident "ctx"), PParen (PApp (UnQual (Ident "Var")) [PVar (Ident "a")])]
               (UnGuardedRhs (App (Var (UnQual (Ident "ctx")))
                                  (Var (UnQual (Ident "a")))))
               Nothing,
         Match (Ident "infer")
               [PVar (Ident "ctx"), PApp (UnQual (Ident "TyK")) []]
               (UnGuardedRhs (App (Var (UnQual (Ident "throwError")))
                                  (Lit (String "Can't have star : star" "Can't have star : star"))))
               Nothing,
         Match () (Ident () "infer") [PVar () (Ident () "ctx"),PApp () (UnQual () (Ident () "False")) []] (UnGuardedRhs () (App () (Var () (UnQual () (Ident () "pure"))) (Con () (UnQual () (Ident () "Bool"))))) Nothing,
         Match () (Ident () "infer") [PVar () (Ident () "ctx"),PApp () (UnQual () (Ident () "Bool")) []] (UnGuardedRhs () (App () (Var () (UnQual () (Ident () "pure"))) (Con () (UnQual () (Ident () "TyK"))))) Nothing,
         Match () (Ident () "infer") [PVar () (Ident () "ctx"),PParen () (PApp () (UnQual () (Ident () "If")) [PVar () (Ident () "a"),PVar () (Ident () "t"),PVar () (Ident () "x"),PVar () (Ident () "y")])] (UnGuardedRhs () (Do () [Qualifier () (App () (App () (App () (Var () (UnQual () (Ident () "check"))) (Var () (UnQual () (Ident () "ctx")))) (Con () (UnQual () (Ident () "Bool")))) (Var () (UnQual () (Ident () "t")))),Qualifier () (App () (App () (App () (Var () (UnQual () (Ident () "check"))) (Paren () (App () (App () (Var () (UnQual () (Ident () "consCtx"))) (Paren () (App () (Var () (UnQual () (Ident () "nf"))) (Con () (UnQual () (Ident () "Bool")))))) (Var () (UnQual () (Ident () "ctx")))))) (Con () (UnQual () (Ident () "TyK")))) (Paren () (App () (Var () (UnQual () (Ident () "fromScope"))) (Var () (UnQual () (Ident () "a")))))),Qualifier () (App () (App () (App () (Var () (UnQual () (Ident () "check"))) (Var () (UnQual () (Ident () "ctx")))) (Paren () (App () (App () (Var () (UnQual () (Ident () "instantiate1"))) (Con () (UnQual () (Ident () "True")))) (Var () (UnQual () (Ident () "a")))))) (Var () (UnQual () (Ident () "x")))),Qualifier () (InfixApp () (Var () (UnQual () (Ident () "pure"))) (QVarOp () (UnQual () (Symbol () "$"))) (App () (Var () (UnQual () (Ident () "nf"))) (Paren () (App () (App () (Var () (UnQual () (Ident () "instantiate1"))) (Var () (UnQual () (Ident () "t")))) (Var () (UnQual () (Ident () "a")))))))])) Nothing,
         Match () (Ident () "infer") [PVar () (Ident () "ctx"),PParen () (PApp () (UnQual () (Ident () "Lam")) [PVar () (Ident () "ty"),PVar () (Ident () "t")])] (UnGuardedRhs () (Do () [Qualifier () (App () (App () (App () (Var () (UnQual () (Ident () "check"))) (Var () (UnQual () (Ident () "ctx")))) (Con () (UnQual () (Ident () "TyK")))) (Var () (UnQual () (Ident () "ty")))),Qualifier () (InfixApp () (InfixApp () (App () (Con () (UnQual () (Ident () "Pi"))) (Paren () (App () (Var () (UnQual () (Ident () "nf"))) (Var () (UnQual () (Ident () "ty")))))) (QVarOp () (UnQual () (Symbol () "."))) (Var () (UnQual () (Ident () "toScope")))) (QVarOp () (UnQual () (Symbol () "<$>"))) (App () (App () (Var () (UnQual () (Ident () "infer"))) (Paren () (App () (App () (Var () (UnQual () (Ident () "consCtx"))) (Paren () (App () (Var () (UnQual () (Ident () "nf"))) (Var () (UnQual () (Ident () "ty")))))) (Var () (UnQual () (Ident () "ctx")))))) (Paren () (App () (Var () (UnQual () (Ident () "fromScope"))) (Var () (UnQual () (Ident () "t"))))))),Qualifier () (App () (Var () (UnQual () (Ident () "pure"))) (Con () (UnQual () (Ident () "TyK"))))])) Nothing,
         Match () (Ident () "infer") [PVar () (Ident () "ctx"),PParen () (PApp () (UnQual () (Ident () "Pi")) [PVar () (Ident () "ty"),PVar () (Ident () "t")])] (UnGuardedRhs () (Do () [Qualifier () (App () (App () (App () (Var () (UnQual () (Ident () "check"))) (Var () (UnQual () (Ident () "ctx")))) (Con () (UnQual () (Ident () "TyK")))) (Var () (UnQual () (Ident () "ty")))),Qualifier () (App () (App () (App () (Var () (UnQual () (Ident () "check"))) (Paren () (App () (App () (Var () (UnQual () (Ident () "consCtx"))) (Paren () (App () (Var () (UnQual () (Ident () "nf"))) (Var () (UnQual () (Ident () "ty")))))) (Var () (UnQual () (Ident () "ctx")))))) (Con () (UnQual () (Ident () "TyK")))) (Paren () (App () (Var () (UnQual () (Ident () "fromScope"))) (Var () (UnQual () (Ident () "t")))))),Qualifier () (App () (Var () (UnQual () (Ident () "pure"))) (Con () (UnQual () (Ident () "TyK"))))])) Nothing,
         Match () (Ident () "infer") [PVar () (Ident () "ctx"),PParen () (PApp () (UnQual () (Ident () "App")) [PVar () (Ident () "f"),PVar () (Ident () "x")])] (UnGuardedRhs () (Do () [Generator () (PVar () (Ident () "v")) (App () (App () (Var () (UnQual () (Ident () "infer"))) (Var () (UnQual () (Ident () "ctx")))) (Var () (UnQual () (Ident () "f")))),Qualifier () (Case () (Var () (UnQual () (Ident () "v"))) [Alt () (PApp () (UnQual () (Ident () "Pi")) [PVar () (Ident () "ty"),PVar () (Ident () "t")]) (UnGuardedRhs () (Do () [Qualifier () (App () (App () (App () (Var () (UnQual () (Ident () "check"))) (Var () (UnQual () (Ident () "ctx")))) (Var () (UnQual () (Ident () "ty")))) (Var () (UnQual () (Ident () "x")))),Qualifier () (InfixApp () (Var () (UnQual () (Ident () "pure"))) (QVarOp () (UnQual () (Symbol () "$"))) (App () (App () (Var () (UnQual () (Ident () "instantiate1"))) (Var () (UnQual () (Ident () "x")))) (Var () (UnQual () (Ident () "t")))))])) Nothing,Alt () (PWildCard ()) (UnGuardedRhs () (App () (Con () (UnQual () (Ident () "Left"))) (Lit () (String () "can't apply non-function" "can't apply non-function")))) Nothing]
         )])) Nothing]


---
