{-# LANGUAGE TemplateHaskell #-}

-- module GenTemplate
--   where

import Prelude hiding (pi, False, True)
import Data.Deriving (deriveEq1, deriveShow1)
import Data.Functor.Classes
import Control.Applicative
import Control.Monad
import Data.Foldable
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
  = Varg a
  | TyK
  | True
  | False
  | Bool
  | Lam (Type a) (Scope () Term a)
  | Pi  (Type a) (Scope () Type a)
  | App (Term a) (Term a)
  | If (Scope () Type a) (Term a) (Term a) (Term a)
  | Bg (Scope () Term (Var () a)) (Term a)

type Type = Term

deriveEq1   ''Term
deriveShow1 ''Term

instance Eq a => Eq (Term a) where (==) = eq1
instance Show a => Show (Term a) where showsPrec = showsPrec1

instance Applicative Term where
  pure  = Varg
  (<*>) = ap

instance Functor Term  where fmap       = fmapDefault
instance Foldable Term where foldMap    = foldMapDefault
deriveTraversable ''Term

instance Monad Term where
  Varg a     >>= f = f a
  TyK       >>= f = TyK
  Bool      >>= f = Bool
  True      >>= f = True
  False     >>= f = False
  If a t x y >>= f = If (a >>>= f) (t >>= f) (x >>= f) (y >>= f)
  Lam ty t  >>= f = Lam (ty >>= f) (t >>>= f)
  Pi  ty t  >>= f = Pi  (ty >>= f) (t >>>= f)
  App t1 t2 >>= f = App (t1 >>= f) (t2 >>= f)
  -- Bg t1 t2 >>= f = Bg ((fmap (>>>= f)) t1) (t2 >>= f)

-- from reductions
nf :: Term a -> Term a
nf (Varg a) = Varg a
nf TyK     = TyK
nf True    = True
nf False   = False
nf Bool    = Bool
nf (If a t x y) = case (nf t) of
      True  -> (nf x)
      False -> (nf y)
      x -> If (toScope $ nf $ fromScope a) x (nf x) (nf y)
nf (Lam ty t)  = Lam (nf ty) (toScope $ nf $ fromScope t)
nf (Pi ty t)   = Pi  (nf ty) (toScope $ nf $ fromScope t)
nf (App t1 t2) = case (nf t1, nf t2) of
      (Lam ty t1, t2) -> nf (instantiate1 t2 t1)
      (f, x)  -> App f x

check :: (Show a, Eq a) => Ctx a -> Type a -> Term a -> TC ()
check ctx want t = do
  have <- infer ctx t
  when (have /= (nf want)) $ Left $
    "type mismatch, have: " ++ (show have) ++ " want: " ++ (show want)


infer :: (Show a, Eq a) => Ctx a -> Term a -> TC (Type a) -- Type (Maybe (Maybe v)) x y.T -> x.T
infer ctx (Varg a) = ctx a
infer ctx TyK     = throwError "Can't have star : star"
infer ctx True    = pure Bool
infer ctx False   = pure Bool
infer ctx Bool    = pure TyK
infer ctx (If a t x y) = do
    check ctx Bool t
    check (consCtx Bool ctx) TyK (fromScope a)
    check ctx (instantiate1 True a) x
    check ctx (instantiate1 False a) y
    pure . nf $ instantiate1 t a
infer ctx (Lam ty t) = do
    check ctx TyK ty
    Pi ty . toScope <$> infer (consCtx ty ctx) (fromScope t)--(fromScope t)
infer ctx (Pi ty t) = do
    check ctx TyK ty
    check (consCtx ty ctx) TyK (fromScope t)
    pure TyK
infer ctx (Bg tt t) = do
    (\tt -> Bg tt t) . toScope <$> infer (consCtx (outBind1 t) $ (consCtx t ctx)) (fromScope tt)
infer ctx (App f x) = do
    v <- infer ctx f
    case v of
      Pi ty t -> do
        check ctx ty x
        pure . nf $ instantiate1 x t
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

abstract0 :: Monad f => f a -> Scope b f a
abstract0 = abstract (const Nothing)

-- flatten on var (traverse rem_i x - lowers ctx by one)
-- x y z. t --> x y. t
rem1 :: Var b a -> Maybe a
rem1 (B _) = Nothing
rem1 (F x) = Just x

-- x y z. t --> x z. t
rem2 :: Var b (Var b a) -> Maybe (Var b a)
rem2 (B x) = Just (B x)
rem2 (F (B _)) = Nothing
rem2 (F (F x)) = Just (F x)

-- x y z. t --> y z. t
rem3 :: Var b (Var b (Var b a)) -> Maybe (Var b (Var b a))
rem3 (B a) = Just (B a)
rem3 (F (B x)) = Just (F (B x))
rem3 (F (F (B _))) = Nothing
rem3 (F (F (F x))) = Just (F (F x))

-- r x y z. t --> x y z. t
rem4 :: Var b (Var b (Var b (Var b a))) -> Maybe (Var b (Var b (Var b a)))
rem4 (B a) = Just (B a)
rem4 (F (B x)) = Just (F (B x))
rem4 (F (F (B x))) = Just (F (F (B x)))
rem4 (F (F (F (B _)))) = Nothing
rem4 (F (F (F (F x)))) = Just (F (F (F x)))


zer = fromScope $ abstract1 "y" (Varg "y")
r = outBind2 $ fromScope $ abstract1 "y" (Varg "x")
l = inBind2 $ fromScope $ abstract1 "y" (Varg "x")

-- y.x -> f y.x
outBind1 :: Monad f => f a -> f (Var b a)
outBind1 x = fromScope $ abstract0 x

-- y.x -> f1 f2 y.x
outBind2 :: Monad f => f a -> f (Var b (Var b a))
outBind2 = outBind1 . outBind1

-- y.x -> f1 f2 f3 y.x
outBind3 :: Monad f => f a -> f (Var b (Var b (Var b a)))
outBind3 = outBind1 . outBind2

-- y.x -> y f.x
inBind1 :: Functor f => f a -> f (Var b a)
inBind1 x = F <$> x

-- y.x -> y f1 f2.x
inBind2 :: Functor f => f a -> f (Var b (Var b a))
inBind2 = inBind1 . inBind1

-- y.x -> y f1 f2 f3.x
inBind3 :: Monad f => f a -> f (Var b (Var b (Var b a)))
inBind3 = inBind1 . inBind2



---
