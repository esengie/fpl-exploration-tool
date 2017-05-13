{-# LANGUAGE TemplateHaskell, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

module SimpleBound(
    Scope(..),
    toScope,
    (>>>=),
    abstract,
    instantiate,
    Var(..)
) where

import Control.Monad
import Control.Monad.Trans
import Control.Applicative
import Data.Functor.Classes
import Data.Deriving (deriveShow1)

data Var a = B | F a
  deriving (Eq, Show, Functor, Foldable, Traversable)

deriveShow1 ''Var

varbe :: b -> (a -> b) -> Var a -> b
varbe n _ B  = n
varbe _ f (F x) = f x

instance Eq1 Var where
  liftEq _ B B = True
  liftEq f (F a) (F b) = f a b
  liftEq _ _ _ = False

newtype Scope f a = Scope { fromScope :: f (Var a) }
  deriving (Functor, Foldable, Traversable)

toScope :: f (Var a) -> Scope f a
toScope = Scope

instance (Monad f, Eq1 f, Eq a) => Eq (Scope f a) where (==) = eq1
instance (Show1 f, Show a) => Show (Scope f a) where showsPrec = showsPrec1

instance (Monad f, Eq1 f) => Eq1 (Scope f) where
  liftEq f m n = liftEq (liftEq f) (fromScope m) (fromScope n)

instance (Show1 f) => Show1 (Scope f) where
  liftShowsPrec f g d m = showsUnaryWith (liftShowsPrec f' g') "Scope" d (fromScope m) where
    f' = liftShowsPrec f g
    g' = liftShowList f g

instance Monad f => Applicative (Scope f) where
  pure = Scope . return . F
  (<*>) = ap

instance Monad f => Monad (Scope f) where
  return = Scope . return . F
  Scope m >>= f = Scope $ m >>= varbe (return B) (fromScope . f)

instance MonadTrans Scope where
  lift = Scope . liftM F

abstract :: (Functor f, Eq a) => a -> f a -> Scope f a
abstract x xs = Scope (fmap go xs) where
  go y = y <$ guard (x /= y)

instantiate :: Monad f => f a -> Scope f a -> f a
instantiate x (Scope xs) = xs >>= go where
  go B  = x
  go (F y) = return y

(>>>=) :: (Monad f) => Scope f a -> (a -> f b) -> Scope f b
m >>>= f = m >>= lift . f

instance Applicative Var where
  pure = F
  (<*>) = ap

instance Monad Var where
  return = pure
  F a >>= f = f a
  B >>= _ = B

instance Alternative Var where
    empty = B
    B <|> r = r
    l <|> _ = l



---
