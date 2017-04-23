{-# LANGUAGE TemplateHaskell #-}

-- May change name and add exports etc.
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

check' :: Show a => TermEq a -> Ctx a -> Type a -> Term a -> TC ()
check' f ctx want t = do
  have <- infer f ctx t
  when (not $ f have want) $ Left $
    "type mismatch, have: " ++ (show have) ++ " want: " ++ (show want)

infer :: Show a => TermEq a -> Ctx a -> Term a -> TC (Type a)
infer feq ctx (Var a) = ctx a
infer feq ctx TyDef   = throwError "Can't have def : def"

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
infer0 = infer (==) emptyCtx

checkSimple :: (Show a, Eq a) => Ctx a -> Type a -> Term a -> TC ()
checkSimple ctx want t = check' (==) ctx want t

checkNf :: (Show a, Eq a) => Ctx a -> Type a -> Term a -> TC ()
checkNf ctx want t = check' (\x y -> nf x == nf y) ctx want t

-- from reductions
nf :: Term a -> Term a
nf (Var a) = Var a
nf TyDef   = TyDef











---
