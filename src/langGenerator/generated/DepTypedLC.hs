{-# LANGUAGE TemplateHaskell #-}

-- module GenTemplate
--   where

import Prelude hiding (pi, False, True)
import Data.Deriving (deriveEq1, deriveShow1)
import Data.Functor.Classes
import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Data.Foldable
import Data.Functor.Identity
import Control.Monad.Error.Class (throwError)
import Data.Traversable (fmapDefault, foldMapDefault)
import Data.Traversable.Deriving
import Bound

import LangTemplate (rem1, rem2, rem3, rem4,
                     outBind1, outBind2, outBind3,
                     inBind1, inBind2, inBind3,
                     abstract0, (>>>>=), (>>>>>=), (>>>>>>=),
                     fromScope2, fromScope3, fromScope4,
                     toScope2, toScope3, toScope4,
                     add1, add2, add3, add4)

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
  | Bg (Scope () (Scope () Term) a) (Term a)

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
  Bg t1 t2 >>= f = Bg (t1 >>>>= f) (t2 >>= f)

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
    (\tt -> Bg tt t) . toScope2
      <$> infer (consCtx (outBind1 t) $ (consCtx t ctx)) (fromScope2 tt)
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



zer = fromScope $ abstract1 "y" (Varg "y")
r = outBind2 $ fromScope $ abstract1 "y" (Varg "x")
l = inBind2 $ fromScope $ abstract1 "y" (Varg "x")

r' = (fromScope $ abstract1 "y" (Varg "x"))

-- x.T -> lam(S, z.(lam(S, y.T[x:=true][v:=false]))) -- xvzy.T
-- z -> z+y -> v+zy -> x+vzy
-- fun :: Scope () Term a -> Term a -> Term a
fun t s x v = let tm = toScope4 $ rta1 (rta1 (rta1 (fromScope t)))
                  s2 = toScope $ rta1 s
                  tsub = (instantiate1 x tm)
                  tork = instantiate1 v tsub
          in
   Lam s (toScope $ Lam (fromScope s2) (fromScope tork))

inBool x = instantiate1 True x
rta1 x = runIdentity (traverse add1 x)

fals' = toScope3 $ rta1 (rta1 (rta1 False))
tru' = toScope2 $ rta1 (rta1 True)

---
