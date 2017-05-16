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
import SimpleBound

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

consCtx :: Type a -> Ctx a -> Ctx (Var a)
consCtx ty ctx B = pure (F <$> ty)
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
nf :: (Show a, Eq a) => Term a -> Term a
nf (Var a) = Var a
nf TyDef   = TyDef

nf':: (Show a, Eq a) => Cnt -> Term a -> Term a
nf' = undefined

stable :: (Show a, Eq a) => Ctx a -> Term a -> [Type a] -> TC (Type ())
stable ctx tm lst = traverse fun tm
  where
    fun x | any (\y -> ctx x == pure y) lst = pure ()
          | otherwise = Left $ "Term is not cstable " ++ show tm

rt f x = runIdentity (traverse f x)

nf1 x = (toScope $ nf $ fromScope x)
nf2 x = (toScope2 $ nf $ fromScope2 x)
nf3 x = (toScope3 $ nf $ fromScope3 x)
nf4 x = (toScope4 $ nf $ fromScope4 x)
nf5 x = (toScope5 $ nf $ fromScope5 x)
nf6 x = (toScope6 $ nf $ fromScope6 x)

-- flatten on var (traverse rem_i x - lowers ctx by one)
-- x y z. t --> x y. t
rem1 :: Var a -> TC a
rem1 B = Left "There is var at 1"
rem1 (F x) = pure x

add1 :: a -> Identity (Var a)
add1 x = pure $ F x

-- x y z. t --> x z. t
rem2 :: Var (Var a) -> TC (Var a)
rem2 B = pure B
rem2 (F B) = Left "There is var at 2"
rem2 (F (F x)) = pure (F x)

add2 :: Var a -> Identity (Var (Var a))
add2 B = pure $ B
add2 (F x) = pure $ F (F x)

-- x y z. t --> y z. t
rem3 :: Var (Var (Var a)) -> TC (Var (Var a))
rem3 (B ) = pure B
rem3 (F (B )) = pure (F (B ))
rem3 (F (F (B ))) = Left "There is var at 3"
rem3 (F (F (F x))) = pure (F (F x))

add3 :: Var (Var a) -> Identity (Var (Var (Var a)))
add3 (B ) = pure $ B
add3 (F (B )) = pure $ F (B )
add3 (F x) = pure $ F (F x)

-- r x y z. t --> x y z. t
rem4 :: Var (Var (Var (Var a))) -> TC (Var (Var (Var a)))
rem4 (B ) = pure (B )
rem4 (F (B )) = pure (F (B ))
rem4 (F (F (B ))) = pure (F (F (B )))
rem4 (F (F (F (B )))) = Left "There is var at 4"
rem4 (F (F (F (F x)))) = pure (F (F (F x)))

add4 :: Var (Var (Var a)) -> Identity (Var (Var (Var (Var a))))
add4 (B ) = pure $ B
add4 (F (B )) = pure $ F (B )
add4 (F (F (B ))) = pure $ F (F (B ))
add4 (F x) = pure $ F (F x)

-- -- Add useless binders
-- abstract0 :: Monad f => f a -> Scope b f a
-- abstract0 = abstract (const Nothing)
--
-- -- y.x -> f y.x
-- outBind1 :: Monad f => f a -> f (Var  a)
-- outBind1 x = fromScope $ abstract0 x
--
-- -- y.x -> f1 f2 y.x
-- outBind2 :: Monad f => f a -> f (Var  (Var  a))
-- outBind2 = outBind1 . outBind1
--
-- -- y.x -> f1 f2 f3 y.x
-- outBind3 :: Monad f => f a -> f (Var  (Var  (Var  a)))
-- outBind3 = outBind1 . outBind2
--
-- -- y.x -> y f.x
-- inBind1 :: Functor f => f a -> f (Var  a)
-- inBind1 x = F <$> x
--
-- -- y.x -> y f1 f2.x
-- inBind2 :: Functor f => f a -> f (Var  (Var  a))
-- inBind2 = inBind1 . inBind1
--
-- -- y.x -> y f1 f2 f3.x
-- inBind3 :: Monad f => f a -> f (Var  (Var  (Var  a)))
-- inBind3 = inBind1 . inBind2


------------- Swappers
swap1'2 :: Var  (Var  a) -> Identity (Var  (Var  a))
swap1'2 (B ) = pure (F (B ))
swap1'2 (F (B )) = pure (B)
swap1'2 x = pure x

swap2'3 :: Var  (Var  (Var  a)) -> Identity (Var  (Var  (Var  a)))
swap2'3 (B ) = pure (B )
swap2'3 (F (B )) = pure (F $ F $ B )
swap2'3 (F (F (B ))) = pure (F $ B )
swap2'3 x = pure x

swap1'3 :: Var  (Var  (Var  a)) -> Identity (Var  (Var  (Var  a)))
swap1'3 (B ) = pure (F $ F $ B )
swap1'3 (F (B )) = pure (F $ B )
swap1'3 (F (F (B ))) = pure (B )
swap1'3 x = pure x

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


data Cnt = Bot | U (Cnt)
  deriving(Eq, Show)

---
