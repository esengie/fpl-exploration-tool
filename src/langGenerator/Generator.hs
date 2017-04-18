{-# LANGUAGE LambdaCase, TemplateHaskell #-}

-- module Generator
--   where

import Prelude hiding (pi, False, True)
import Data.Deriving (deriveEq1, deriveShow1)
import Data.Functor.Classes
import Control.Applicative
import Control.Monad
import Control.Monad.Error.Class (throwError)
import Data.Traversable
import Bound

data Term a
  = Var a
  | Star
  | True
  | False
  | Bool
  | Lam (Type a) (Scope () Term a)
  | Pi  (Type a) (Scope () Type a)
  | App (Term a) (Term a)
  | If (Scope () Type a) (Term a) (Term a) (Term a)

type Type = Term

deriveEq1   ''Term
deriveShow1 ''Term

instance Eq a => Eq (Term a) where (==) = eq1
instance Show a => Show (Term a) where showsPrec = showsPrec1

instance Applicative Term where
  pure  = Var
  (<*>) = ap

instance Functor Term  where fmap       = fmapDefault
instance Foldable Term where foldMap    = foldMapDefault

instance Traversable Term where
  traverse f (Var a)    = Var <$> f a
  traverse _ Star       = pure Star
  traverse _ True       = pure True
  traverse _ False      = pure False
  traverse _ Bool       = pure Bool
  traverse f (If a t x y) = If <$> traverse f a <*> traverse f t <*> traverse f x <*> traverse f y
  traverse f (App x y)  = App <$> traverse f x <*> traverse f y
  traverse f (Lam ty e) = Lam <$> traverse f ty <*> traverse f e
  traverse f (Pi ty e)  = Pi <$> traverse f ty <*> traverse f e

instance Monad Term where
  Var a     >>= f = f a
  Star      >>= f = Star
  Bool      >>= f = Bool
  True      >>= f = True
  False      >>= f = False
  If a t x y >>= f = If (a >>>= f) (t >>= f) (x >>= f) (y >>= f)
  Lam ty t  >>= f = Lam (ty >>= f) (t >>>= f)
  Pi  ty t  >>= f = Pi  (ty >>= f) (t >>>= f)
  App t1 t2 >>= f = App (t1 >>= f) (t2 >>= f)

-- from reductions
rnf :: Term a -> Term a
rnf = \case
  Var a    -> Var a
  Star     -> Star
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

type TC    = Either String
type Cxt a = a -> TC (Type a)

consCxt :: Type a -> Cxt a -> Cxt (Var () a)
consCxt ty cxt (B ()) = pure (F <$> ty)
consCxt ty cxt (F a)  = (F <$>) <$> cxt a

check :: (Show a, Eq a) => Cxt a -> Type a -> Term a -> TC ()
check cxt want t = do
  have <- infer cxt t
  when (have /= want) $ Left $
    "type mismatch, have: " ++ (show have) ++ " want: " ++ (show want)

infer :: (Show a, Eq a) => Cxt a -> Term a -> TC (Type a)
infer cxt = \case
  Var a -> cxt a
  Star  -> throwError "Can't have star : star"
  True -> pure Bool
  False -> pure Bool
  Bool -> pure Star
  If a t x y -> do
    check cxt Bool t
    check (consCxt Bool cxt) Star (fromScope a)
    check cxt (rnf (instantiate1 True a)) x
    check cxt (rnf (instantiate1 False a)) y
    pure $ rnf (instantiate1 t a)
  Lam ty t -> do
    check cxt Star ty
    let ty' = rnf ty
    Pi ty' . toScope <$> infer (consCxt ty' cxt) (fromScope t)
  Pi ty t -> do
    check cxt Star ty
    check (consCxt (rnf ty) cxt) Star (fromScope t)
    pure Star
  App f x ->
    infer cxt f >>= \case
      Pi ty t -> do
        check cxt ty x
        pure $ rnf (instantiate1 x t)
      _ -> Left "can't apply non-function"

emptyCtx :: Cxt a
emptyCtx = (const $ Left "variable not in scope")

-- infer in the empty context
infer0 :: (Show a, Eq a) => Term a -> TC (Type a)
infer0 = infer emptyCtx

-- smart constructors

lam :: Eq a => a -> Type a -> Term a -> Term a
lam v ty t = Lam ty (abstract1 v t)

pi :: Eq a => a -> Type a -> Term a -> Term a
pi v ty t = Pi ty (abstract1 v t)

iff :: Eq a => a -> Type a -> Term a -> Term a -> Term a -> Term a
iff v ty t x y = If (abstract1 v ty) t x y

(==>) :: Type a -> Type a -> Type a -- non-dependent function type
a ==> b = Pi a (Scope $ fmap (F . pure) b)
infixr 5 ==>

fromList :: Eq a => [(a, Type a)] -> Cxt a
fromList [] = emptyCtx
fromList ((x,t):xs) = \y -> if (x == y)
                              then return t
                              else fromList xs y




---
