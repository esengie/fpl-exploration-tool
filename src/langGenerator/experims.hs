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
infer ctx True = pure Bool
infer ctx False = pure Bool
infer ctx Bool = pure TyK
infer ctx (If a t x y) = do
  check ctx Bool t
  check (consCtx Bool ctx) TyK (fromScope a)
  check ctx (instantiate1 True a) x
  check ctx (instantiate1 False a) y
  pure $ instantiate1 t a
infer ctx (Lam ty t) = do
  check ctx TyK ty
  Pi ty . toScope <$> infer (consCtx ty ctx) (fromScope t)
infer ctx (Pi ty t) = do
  check ctx TyK ty
  check (consCtx ty ctx) TyK (fromScope t)
  pure TyK
infer ctx (App f x) = do
  v <- infer ctx f
  case v of
      Pi ty t -> do
        check ctx ty x
        pure $ instantiate1 x t
      _ -> Left "can't apply non-function"





---
