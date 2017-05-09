{-# LANGUAGE TemplateHaskell #-}

-- May change name and add exports etc.
module LangTemplate
  where

import Prelude hiding (pi, False, True)
import Data.Deriving (deriveEq1, deriveShow1)
import Data.Functor.Classes
import Data.Foldable
import Control.Applicative
import Control.Monad
import Data.Functor.Identity
import Control.Monad.Trans (lift)
import Control.Monad.Error.Class (throwError)
import Data.Traversable (fmapDefault, foldMapDefault)
import Data.Traversable.Deriving
import Bound

--- Don't make changes to the code here, may add you own functions and types
--- Codegen affects infer and nf functions and Term datatype + its' Monad instance.

type TC    = Either String
type Ctx a = a -> TC (Type a)

data Term a
  = Var a
  | TyDef

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
  Var a >>= f = f a
  TyDef >>= f = TyDef

type TermEq a = Term a -> Term a -> Bool

checkT :: (Show a, Eq a) => Ctx a -> Type a -> Term a -> TC ()
checkT ctx want t = do
  have <- infer ctx t
  when (nf have /= nf want) $ Left $
    "type mismatch, have: " ++ (show have) ++ " want: " ++ (show want)

checkEq :: (Show a, Eq a) => Term a -> Term a -> TC ()
checkEq want have = do
  when (nf have /= nf want) $ Left $
    "Terms are unequal, left: " ++ (show have) ++ " right: " ++ (show want)


infer :: (Show a, Eq a) => Ctx a -> Term a -> TC (Type a)
infer ctx (Var a) = ctx a
infer ctx TyDef   = throwError "Can't have def : def"

report :: String -> TC (Type a)
report nm = throwError $ "Can't have " ++ nm ++ " : " ++ nm

emptyCtx :: Ctx a
emptyCtx = (const $ Left "variable not in scope")

consCtx :: Type a -> Ctx a -> Ctx (Var () a)
consCtx ty ctx (B ()) = pure (F <$> ty)
consCtx ty ctx (F a)  = (F <$>) <$> ctx a

fromList :: Eq a => [(a, Type a)] -> Ctx a
fromList [] = emptyCtx
fromList ((x,t):xs) = \y -> if (x == y)
                              then return t
                              else fromList xs y

-- infer in the empty context
infer0 :: (Show a, Eq a) => Term a -> TC (Type a)
infer0 = infer emptyCtx

-- from reductions
nf :: Term a -> Term a
nf (Var a) = Var a
nf TyDef   = TyDef

rt f x = runIdentity (traverse f x)

-- flatten on var (traverse rem_i x - lowers ctx by one)
-- x y z. t --> x y. t
rem1 :: Var b a -> TC a
rem1 (B _) = Left "There is var at 1"
rem1 (F x) = pure x

add1 :: a -> Identity (Var b a)
add1 x = pure $ F x

-- x y z. t --> x z. t
rem2 :: Var b (Var b a) -> TC (Var b a)
rem2 (B x) = pure (B x)
rem2 (F (B _)) = Left "There is var at 2"
rem2 (F (F x)) = pure (F x)

add2 :: Var b a -> Identity (Var b (Var b a))
add2 (B x) = pure $ B x
add2 (F x) = pure $ F (F x)

-- x y z. t --> y z. t
rem3 :: Var b (Var b (Var b a)) -> TC (Var b (Var b a))
rem3 (B a) = pure (B a)
rem3 (F (B x)) = pure (F (B x))
rem3 (F (F (B _))) = Left "There is var at 3"
rem3 (F (F (F x))) = pure (F (F x))

add3 :: Var b (Var b a) -> Identity (Var b (Var b (Var b a)))
add3 (B x) = pure $ B x
add3 (F (B x)) = pure $ F (B x)
add3 (F x) = pure $ F (F x)

-- r x y z. t --> x y z. t
rem4 :: Var b (Var b (Var b (Var b a))) -> TC (Var b (Var b (Var b a)))
rem4 (B a) = pure (B a)
rem4 (F (B x)) = pure (F (B x))
rem4 (F (F (B x))) = pure (F (F (B x)))
rem4 (F (F (F (B _)))) = Left "There is var at 4"
rem4 (F (F (F (F x)))) = pure (F (F (F x)))

add4 :: Var b (Var b (Var b a)) -> Identity (Var b (Var b (Var b (Var b a))))
add4 (B x) = pure $ B x
add4 (F (B x)) = pure $ F (B x)
add4 (F (F (B x))) = pure $ F (F (B x))
add4 (F x) = pure $ F (F x)

-- -- Add useless binders
-- abstract0 :: Monad f => f a -> Scope b f a
-- abstract0 = abstract (const Nothing)
--
-- -- y.x -> f y.x
-- outBind1 :: Monad f => f a -> f (Var b a)
-- outBind1 x = fromScope $ abstract0 x
--
-- -- y.x -> f1 f2 y.x
-- outBind2 :: Monad f => f a -> f (Var b (Var b a))
-- outBind2 = outBind1 . outBind1
--
-- -- y.x -> f1 f2 f3 y.x
-- outBind3 :: Monad f => f a -> f (Var b (Var b (Var b a)))
-- outBind3 = outBind1 . outBind2
--
-- -- y.x -> y f.x
-- inBind1 :: Functor f => f a -> f (Var b a)
-- inBind1 x = F <$> x
--
-- -- y.x -> y f1 f2.x
-- inBind2 :: Functor f => f a -> f (Var b (Var b a))
-- inBind2 = inBind1 . inBind1
--
-- -- y.x -> y f1 f2 f3.x
-- inBind3 :: Monad f => f a -> f (Var b (Var b (Var b a)))
-- inBind3 = inBind1 . inBind2


------------- Swappers
swap12 :: Var b (Var b a) -> Identity (Var b (Var b a))
swap12 (B x) = pure (F (B x))
swap12 (F (B x)) = pure (B x)
swap12 x = pure x

swap23 :: Var b (Var b (Var b a)) -> Identity (Var b (Var b (Var b a)))
swap23 (B x) = pure (B x)
swap23 (F (B x)) = pure (F $ F $ B x)
swap23 (F (F (B x))) = pure (F $ B x)
swap23 x = pure x

swap13 :: Var b (Var b (Var b a)) -> Identity (Var b (Var b (Var b a)))
swap13 (B x) = pure (F $ F $ B x)
swap13 (F (B x)) = pure (F $ B x)
swap13 (F (F (B x))) = pure (B x)
swap13 x = pure x

-- n free vars
ap2 m f = m >>>= (lift . f)
ap3 m f = ap2 m (lift . f)
ap4 m f = ap3 m (lift . f)
ap5 m f = ap4 m (lift . f)
ap6 m f = ap5 m (lift . f)
ap7 m f = ap6 m (lift . f)

---------
fromScope2 x = fromScope $ fromScope x
fromScope3 x = fromScope $ fromScope2 x
fromScope4 x = fromScope $ fromScope3 x
fromScope5 x = fromScope $ fromScope4 x
fromScope6 x = fromScope $ fromScope5 x
fromScope7 x = fromScope $ fromScope6 x

toScope2 x = toScope $ toScope x
toScope3 x = toScope $ toScope2 x
toScope4 x = toScope $ toScope3 x
toScope5 x = toScope $ toScope4 x
toScope6 x = toScope $ toScope5 x
toScope7 x = toScope $ toScope6 x

---
