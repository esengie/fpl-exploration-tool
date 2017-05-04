{-# LANGUAGE TemplateHaskell #-}

-- May change name and add exports etc.
module GenTemplate
  where

import Prelude hiding (pi, False, True)
import Data.Deriving (deriveEq1, deriveShow1)
import Data.Functor.Classes
import Data.Foldable
import Control.Applicative
import Control.Monad
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

checkEq :: (Show a, Eq a) => Ctx a -> Type a -> Term a -> TC ()
checkEq ctx want have = do
  when (nf have /= nf want) $ Left $
    "type mismatch, have: " ++ (show have) ++ " want: " ++ (show want)


infer :: (Show a, Eq a) => Ctx a -> Term a -> TC (Type a)
infer ctx (Var a) = ctx a
infer ctx TyDef   = throwError "Can't have def : def"

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

-- Add useless binders
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
