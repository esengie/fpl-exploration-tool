{-# LANGUAGE TemplateHaskell #-}

module GenTemplate
  where

import Prelude hiding (pi, False, True)
import Data.Deriving (deriveEq1, deriveShow1)
import Data.Functor.Classes
import Control.Applicative
import Control.Monad
import Control.Monad.Error.Class (throwError)
import Data.Traversable (fmapDefault, foldMapDefault)
import Data.Traversable.Deriving
import Bound

type TC    = Either String
type Ctx a = a -> TC (Type a)

consCtx :: Type a -> Ctx a -> Ctx (Var () a)
consCtx ty ctx (B ()) = pure (F <$> ty)
consCtx ty ctx (F a)  = (F <$>) <$> ctx a

data Term a
  = Var a
  | TyK
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
deriveTraversable ''Term

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
rnf (Var a) = Var a
rnf TyK     = TyK
rnf True    = True
rnf False   = False
rnf Bool    = Bool
rnf (If a t x y) = case (rnf t) of
      True  -> (rnf x)
      False -> (rnf y)
      x -> If (toScope $ rnf $ fromScope a) x (rnf x) (rnf y)
rnf (Lam ty t)  = Lam (rnf ty) (toScope $ rnf $ fromScope t)
rnf (Pi ty t)   = Pi  (rnf ty) (toScope $ rnf $ fromScope t)
rnf (App t1 t2) = case (rnf t1, rnf t2) of
      (Lam ty t1, t2) -> rnf (instantiate1 t2 t1)
      (f, x)  -> App f x

check :: (Show a, Eq a) => Ctx a -> Type a -> Term a -> TC ()
check ctx want t = do
  have <- infer ctx t
  when (have /= (rnf want)) $ Left $
    "type mismatch, have: " ++ (show have) ++ " want: " ++ (show want)

infer :: (Show a, Eq a) => Ctx a -> Term a -> TC (Type a)
infer ctx (Var a) = ctx a
infer ctx TyK     = throwError "Can't have star : star"
infer ctx True    = pure Bool
infer ctx False   = pure Bool
infer ctx Bool    = pure TyK
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

emptyCtx :: Ctx a
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

fromList :: Eq a => [(a, Type a)] -> Ctx a
fromList [] = emptyCtx
fromList ((x,t):xs) = \y -> if (x == y)
                              then return t
                              else fromList xs y




---
