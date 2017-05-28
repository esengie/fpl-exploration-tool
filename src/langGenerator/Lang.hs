{-# LANGUAGE TemplateHaskell #-}
module Lang (TC, Ctx, consCtx, Term(..), infer, infer0, nf) where
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

type TC = Either String

type Ctx a = a -> TC (Type a)

data Term a = Var a
            | TnDef
            | TyDef
            | App (Term a) (Term a) (Scope Type a)
            | Bool
            | False
            | Ff (Type a) (Term a)
            | If (Scope Type a) (Term a) (Term a) (Term a)
            | Lam (Type a) (Scope Term a)
            | Pi (Type a) (Scope Type a)
            | Sigma (Term a) (Term a) (Scope Type a) (Scope (Scope Type) a)
            | True

type Tn = Term

type Type = Term

deriveEq1 ''Term

deriveShow1 ''Term

instance Eq a => Eq (Term a) where
        (==) = eq1

instance Show a => Show (Term a) where
        showsPrec = showsPrec1

instance Applicative Term where
        pure = Var
        (<*>) = ap

instance Functor Term where
        fmap = fmapDefault

instance Foldable Term where
        foldMap = foldMapDefault

deriveTraversable ''Term

instance Monad Term where
        Var v1 >>= f = f v1
        App v1 v2 v3 >>= f = App (v1 >>= f) (v2 >>= f) (v3 >>>= f)
        Bool >>= f = Bool
        False >>= f = False
        Ff v1 v2 >>= f = Ff (v1 >>= f) (v2 >>= f)
        If v1 v2 v3 v4 >>= f
          = If (v1 >>>= f) (v2 >>= f) (v3 >>= f) (v4 >>= f)
        Lam v1 v2 >>= f = Lam (v1 >>= f) (v2 >>>= f)
        Pi v1 v2 >>= f = Pi (v1 >>= f) (v2 >>>= f)
        Sigma v1 v2 v3 v4 >>= f
          = Sigma (v1 >>= f) (v2 >>= f) (v3 >>>= f) (ap2 v4 f)
        True >>= f = True
        TnDef >>= f = TnDef
        TyDef >>= f = TyDef

checkT :: (Show a, Eq a) => Ctx a -> Type a -> Term a -> TC ()
checkT ctx want t
  = do have <- infer ctx t
       when (nf have /= nf want) $
         Left $
           "type mismatch, have: " ++ (show have) ++ " want: " ++ (show want)

checkEq :: (Show a, Eq a) => Term a -> Term a -> TC ()
checkEq want have
  = do when (nf have /= nf want) $
         Left $
           "Terms are unequal, left: " ++
             (show have) ++ " right: " ++ (show want)

report :: String -> TC (Type a)
report nm = throwError $ "Can't have " ++ nm ++ " : " ++ nm

emptyCtx :: (Show a, Eq a) => Ctx a
emptyCtx x = Left $ "Variable not in scope: " ++ show x

consCtx :: (Show a, Eq a) => Type a -> Ctx a -> Ctx (Var a)
consCtx x = consCtx' x

consCtx' :: (Show a, Eq a) => Type a -> Ctx a -> Ctx (Var a)
consCtx' ty ctx B = pure (F <$> ty)
consCtx' ty ctx (F a) = (F <$>) <$> ctx a

consErr ::
          (Show a, Eq a) => Type a -> [Type a] -> TC (Type (Var a))
consErr t lst = throwError $ show t ++ " is not in " ++ show lst

infer :: (Show a, Eq a) => Ctx a -> Term a -> TC (Type a)
infer ctx (Var v1) = ctx v1
infer ctx TnDef = report "TnDef"
infer ctx TyDef = report "TyDef"
infer ctx al@(App v1 v2 v3)
  = do stable ctx al [Bool]
       v4 <- infer ctx v2
       v5 <- pure (nf v4)
       v6 <- infer ctx v1
       checkEq (Pi v5 (toScope (fromScope v3))) v6
       checkT ctx TyDef v5
       checkT (consCtx v5 ctx) TyDef (fromScope v3)
       infer ctx v1
       infer ctx v2
       pure (instantiate v2 (toScope (fromScope v3)))
infer ctx al@Bool
  = do stable ctx al [Bool]
       pure TyDef
infer ctx al@False
  = do stable ctx al [Bool]
       pure Bool
infer ctx al@(Ff v1 v2)
  = do stable ctx al [Bool]
       checkT ctx TyDef v1
       checkT (consCtx v1 ctx) TyDef (rt add1 v1)
       v3 <- infer (consCtx (rt add1 v1) (consCtx v1 ctx))
               (rt add1 (rt add1 v2))
       v4 <- pure (nf v3) >>= traverse rem1 >>= traverse rem1
       checkT ctx TyDef v4
       v5 <- infer (consCtx v4 ctx) (rt add1 v2)
       checkEq Bool v5
       infer ctx v2
       pure TyDef
infer ctx al@(If v1 v2 v3 v4)
  = do stable ctx al [Bool]
       v5 <- infer ctx v2
       checkEq Bool v5
       v6 <- infer ctx v4
       checkEq (instantiate False (toScope (fromScope v1))) v6
       v7 <- infer ctx v3
       checkEq (instantiate True (toScope (fromScope v1))) v7
       checkT ctx TyDef Bool
       checkT (consCtx Bool ctx) TyDef (fromScope v1)
       infer ctx v2
       infer ctx v3
       infer ctx v4
       pure (instantiate v2 (toScope (fromScope v1)))
infer ctx al@(Lam v1 v2)
  = do stable ctx al [Bool]
       checkT ctx TyDef v1
       v3 <- infer (consCtx v1 ctx) (fromScope v2)
       v4 <- pure (nf v3)
       pure (Pi v1 (toScope v4))
infer ctx al@(Pi v1 v2)
  = do stable ctx al [Bool]
       checkT ctx TyDef v1
       checkT (consCtx v1 ctx) TyDef (fromScope v2)
       pure TyDef
infer ctx al@(Sigma v1 v2 v3 v4)
  = do stable ctx al [Bool]
       checkT ctx TyDef Bool
       v5 <- infer (consCtx Bool ctx) (rt add1 v2)
       v6 <- pure (nf v5)
       checkEq v6 (fromScope v3)
       checkT ctx TyDef Bool
       v7 <- infer (consCtx Bool ctx) (rt add1 v1)
       checkEq
         (Pi (fromScope v3)
            (toScope (instantiate True (toScope (rt add2 (fromScope2 v4))))))
         v7
       checkT ctx TyDef Bool
       checkT (consCtx Bool ctx) TyDef (fromScope v3)
       checkT (consCtx (fromScope v3) (consCtx Bool ctx)) TyDef
         (rt add1 (fromScope v3))
       checkT
         (consCtx (rt add1 (fromScope v3))
            (consCtx (fromScope v3) (consCtx Bool ctx)))
         TyDef
         (rt add3 (rt swap1'2 (fromScope2 v4)))
       infer ctx v1
       infer ctx v2
       pure
         (instantiate v2
            (toScope
               (instantiate (rt add1 v2) (toScope (rt swap1'2 (fromScope2 v4))))))
infer ctx al@True
  = do stable ctx al [Bool]
       pure Bool

infer0 :: (Show a, Eq a) => Term a -> TC (Type a)
infer0 = infer emptyCtx

nf :: (Show a, Eq a) => Term a -> Term a
nf (Var v1) = Var v1
nf TnDef = TnDef
nf TyDef = TyDef
nf (App v1 v2 v3) = nf' (U Bot) (App (nf v1) (nf v2) (nf1 v3))
nf Bool = Bool
nf False = False
nf (Ff v1 v2) = Ff (nf v1) (nf v2)
nf (If v1 v2 v3 v4)
  = nf' (U (U Bot)) (If (nf1 v1) (nf v2) (nf v3) (nf v4))
nf (Lam v1 v2) = Lam (nf v1) (nf1 v2)
nf (Pi v1 v2) = Pi (nf v1) (nf1 v2)
nf (Sigma v1 v2 v3 v4) = Sigma (nf v1) (nf v2) (nf1 v3) (nf2 v4)
nf True = True

nf' :: (Show a, Eq a) => Cnt -> Term a -> Term a
nf' (U _)
  al@(App (Lam v1 (Scope (App v2 v3 (Scope v4)))) v5 (Scope v6))
  = case
      do v7 <- pure v1
         v8 <- pure v6
         v9 <- pure v4 >>= traverse rem2
         v10 <- pure v5
         v11 <- pure v2 >>= traverse rem1
         v12 <- pure v3
         checkEq v8 v9
         checkEq v10 v11
         pure (instantiate v11 (toScope v12))
      of
        Left _ -> nf' Bot al
        Right x -> nf x
nf' (U (U _)) al@(If (Scope v1) True v2 v3)
  = case
      do v4 <- pure v1
         v5 <- pure v2
         v6 <- pure v3
         pure v5
      of
        Left _ -> nf' (U Bot) al
        Right x -> nf x
nf' (U _) al@(If (Scope v1) False v2 v3)
  = case
      do v4 <- pure v1
         v5 <- pure v2
         v6 <- pure v3
         pure v6
      of
        Left _ -> nf' Bot al
        Right x -> nf x
nf' _ x = x

stable ::
         (Show a, Eq a) => Ctx a -> Term a -> [Type a] -> TC (Type ())
stable ctx tm lst = traverse fun tm
  where fun x
          | any (\ y -> ctx x == pure y) lst = pure ()
          | otherwise = Left $ "Term is not cstable " ++ show tm
rt f x = runIdentity (traverse f x)
nf1 x = (toScope $ nf $ fromScope x)
nf2 x = (toScope2 $ nf $ fromScope2 x)
nf3 x = (toScope3 $ nf $ fromScope3 x)
nf4 x = (toScope4 $ nf $ fromScope4 x)
nf5 x = (toScope5 $ nf $ fromScope5 x)
nf6 x = (toScope6 $ nf $ fromScope6 x)

rem1 :: Var a -> TC a
rem1 B = Left "There is var at 1"
rem1 (F x) = pure x

add1 :: a -> Identity (Var a)
add1 x = pure $ F x

rem2 :: Var (Var a) -> TC (Var a)
rem2 B = pure B
rem2 (F B) = Left "There is var at 2"
rem2 (F (F x)) = pure (F x)

add2 :: Var a -> Identity (Var (Var a))
add2 B = pure $ B
add2 (F x) = pure $ F (F x)

rem3 :: Var (Var (Var a)) -> TC (Var (Var a))
rem3 (B) = pure B
rem3 (F (B)) = pure (F (B))
rem3 (F (F (B))) = Left "There is var at 3"
rem3 (F (F (F x))) = pure (F (F x))

add3 :: Var (Var a) -> Identity (Var (Var (Var a)))
add3 (B) = pure $ B
add3 (F (B)) = pure $ F (B)
add3 (F x) = pure $ F (F x)

rem4 :: Var (Var (Var (Var a))) -> TC (Var (Var (Var a)))
rem4 (B) = pure (B)
rem4 (F (B)) = pure (F (B))
rem4 (F (F (B))) = pure (F (F (B)))
rem4 (F (F (F (B)))) = Left "There is var at 4"
rem4 (F (F (F (F x)))) = pure (F (F (F x)))

add4 :: Var (Var (Var a)) -> Identity (Var (Var (Var (Var a))))
add4 (B) = pure $ B
add4 (F (B)) = pure $ F (B)
add4 (F (F (B))) = pure $ F (F (B))
add4 (F x) = pure $ F (F x)

swap1'2 :: Var (Var a) -> Identity (Var (Var a))
swap1'2 (B) = pure (F (B))
swap1'2 (F (B)) = pure (B)
swap1'2 x = pure x

swap2'3 :: Var (Var (Var a)) -> Identity (Var (Var (Var a)))
swap2'3 (B) = pure (B)
swap2'3 (F (B)) = pure (F $ F $ B)
swap2'3 (F (F (B))) = pure (F $ B)
swap2'3 x = pure x

swap1'3 :: Var (Var (Var a)) -> Identity (Var (Var (Var a)))
swap1'3 (B) = pure (F $ F $ B)
swap1'3 (F (B)) = pure (F $ B)
swap1'3 (F (F (B))) = pure (B)
swap1'3 x = pure x
ap2 m f = m >>>= (lift . f)
ap3 m f = ap2 m (lift . f)
ap4 m f = ap3 m (lift . f)
ap5 m f = ap4 m (lift . f)
ap6 m f = ap5 m (lift . f)
ap7 m f = ap6 m (lift . f)
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

data Cnt = Bot
         | U (Cnt)
         deriving (Eq, Show)
