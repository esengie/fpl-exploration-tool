module AST.Term(
  SortName(..),
  VarName(..),
  Name(..),
  ContextDepth(..),
  DefaultErr(..),
  Sort(..),
  Ctx(..),
  FunctionalSymbol(..),
  MetaVar(..),
  Term(..),
  varSort,
  tyName,
  tmName,
  getSortName,
  getSortDepth,
  addToCtx,
  lowerCtx,
  zero,
  isDepSort,
  lookupName,
  lookupName',
  isFunSym,
  isVar,
  allUnique,
  isSubset,
  changeError
) where

import qualified Data.Set as Set
import Data.List(intercalate)

type SortName = String
type VarName = String
type Name = String
type ContextDepth = Int
type DefaultErr = Either String

changeError :: String -> DefaultErr a -> DefaultErr a
changeError msg (Left x) = Left (msg ++ "\n\t" ++ x)
changeError _ x = x

data Sort = DepSort SortName !ContextDepth | SimpleSort SortName
  deriving (Eq)

type Ctx = [VarName]

bracket :: String -> String
bracket s = "(" ++ s ++ ")"

instance Show Sort where
  show (DepSort nm dp) = bracket $ nm ++ "," ++ show dp
  show (SimpleSort nm) = nm

varSort :: Sort
varSort = DepSort tmName 0

tyName :: SortName
tyName = "ty"

tmName :: SortName
tmName = "tm"

getSortName :: Sort -> SortName
getSortName (DepSort nm _) = nm
getSortName (SimpleSort nm) = nm

getSortDepth :: Sort -> ContextDepth
getSortDepth (SimpleSort _) = 0
getSortDepth (DepSort _ x) = x

zero :: Sort -> Sort
zero (DepSort nm _) = DepSort nm 0
zero x = x

addToCtx :: ContextDepth -> Sort -> DefaultErr Sort
addToCtx _ (SimpleSort _) = Left "Simple sorts can't have context"
addToCtx n (DepSort nm num) | num + n >= 0 = return (DepSort nm $ num + n)
                            | otherwise = Left "Context is empty already"

lowerCtx :: Sort -> DefaultErr Sort
lowerCtx = addToCtx (-1)

isDepSort :: Sort -> Bool
isDepSort (DepSort _ _) = True
isDepSort _ = False

data FunctionalSymbol = FunSym {
  name   :: Name
, arguments :: [Sort]
, result    :: Sort       --- hack in the parser that gets solved in the checking stage
} deriving (Eq)

instance Show FunctionalSymbol where
  show (FunSym nm [] res) = nm ++ ": " ++ show res
  show (FunSym nm args res) =
    nm ++ ": " ++ intercalate "*" (map show args) ++ "->" ++ show res

data MetaVar = MetaVar {
  mContext  :: [VarName]
, mName     :: VarName
}

instance Eq MetaVar where
  m == m' = (mName m) == (mName m')

instance Ord MetaVar where
  m < m' = (mName m) < (mName m')


showCtxVar :: [Name] -> String -> String
showCtxVar [] y = y
showCtxVar [x] y = x ++ "." ++ y
showCtxVar args y = bracket (unwords args) ++ "." ++ y

instance Show MetaVar where
  show (MetaVar x y) = showCtxVar x y

data Term = Var VarName              -- xyz
          | Meta MetaVar
          | FunApp Name [(Ctx, Term)]
          | Subst Term VarName Term
    deriving (Eq)

instance Show Term where
  show (Var nm) = nm
  show (Meta vr) = mName vr ++ "-m"
  show (FunApp nm []) = nm ++ "-f"
  show (FunApp nm args) = nm ++ bracket (intercalate ", " (map (\(x, y) -> showCtxVar x (show y)) args))
  show (Subst into vn what) = show into ++ "[" ++ vn ++ ":= " ++ show what ++ "]"

isFunSym :: Term -> Bool
isFunSym FunApp{} = True
isFunSym _ = False

isVar :: Term -> Bool
isVar Var{} = True
isVar _ = False

lookupName :: (a -> Name) -> Name -> [a] -> DefaultErr a
lookupName f = lookupName' (\x y -> f x == y)

-- f : tm + name = equal?
lookupName' :: (a -> Name -> Bool) -> Name -> [a] -> DefaultErr a
lookupName' idf name (x : xs) | idf x name = return x
  | otherwise = lookupName' idf name xs
lookupName' _ name _ = Left $ "Name " ++ show name ++ " not found!"

allUnique :: Ord a => [a] -> Bool
allUnique a = length a == Set.size (Set.fromList a)

isSubset :: Ord a => [a] -> [a] -> Bool
isSubset a b = 0 == Set.size (Set.difference (Set.fromList a) (Set.fromList b))





--
