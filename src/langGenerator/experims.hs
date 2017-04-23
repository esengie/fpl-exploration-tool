{-# LANGUAGE LambdaCase, TemplateHaskell #-}

import Language.Haskell.Exts.Simple

instance Monad Term where
  Var a     >>= f = f a
  TyK       >>= f = TyK
  Bool      >>= f = Bool
  True      >>= f = True
  False     >>= f = False
  If a t x y >>= f = If (a >>>= f) (t >>= f) (x >>= f) (y >>= f)
  Lam ty t  >>= f = Lam (ty >>= f) (t >>>= f)
  Pi  ty t  >>= f = Pi  (ty >>= f) (t >>>= f)
  App t1 t2 >>= f = App (t1 >>= f) (t2 >>= f)

-- from reductions
rnf :: Term a -> Term a
rnf = \case
  Var a    -> Var a
  TyK      -> TyK
  True     -> True
  False    -> False
  Bool     -> Bool
  If a t x y -> case (rnf t) of
    True  -> (rnf x)
    False -> (rnf y)
    x -> If (toScope $ rnf $ fromScope a) x (rnf x) (rnf y)
  Lam ty t -> Lam (rnf ty) (toScope $ rnf $ fromScope t)
  Pi  ty t -> Pi  (rnf ty) (toScope $ rnf $ fromScope t)
  App t1 t2 -> case (rnf t1, rnf t2) of
    (Lam ty t1, t2) -> rnf (instantiate1 t2 t1)
    (f, x)  -> App f x

infer :: (Show a, Eq a) => Ctx a -> Term a -> TC (Type a)
infer ctx = \case
  Var a -> ctx a
  TyK  -> throwError "Can't have star : star"
  True -> pure Bool
  False -> pure Bool
  Bool -> pure TyK
  If a t x y -> do
    check ctx Bool t
    check (consCtx Bool ctx) TyK (fromScope a)
    check ctx (instantiate1 True a) x
    check ctx (instantiate1 False a) y
    pure $ instantiate1 t a
  Lam ty t -> do
    check ctx TyK ty
    Pi ty . toScope <$> infer (consCtx ty ctx) (fromScope t)
  Pi ty t -> do
    check ctx TyK ty
    check (consCtx ty ctx) TyK (fromScope t)
    pure TyK
  App f x ->
    infer ctx f >>= \case
      Pi ty t -> do
        check ctx ty x
        pure $ instantiate1 x t
      _ -> Left "can't apply non-function"





---
