module AST.Term(
  SortName(..),
  VarName(..),
  Name(..),
  ContextDepth(..),
  DefaultErr(..),
  Sort(..),
  FunctionalSymbol(..),
  MetaVar(..),
  Term(..),
  varSort,
  tyName,
  tmName,
  getSortName,
  lowerCtx,
  isDepSort,
  lookupName,
  lookupName',
  isFunSym,
  isVar,
  allUnique,
  isSubset
) where

import qualified Data.Set as Set
import Data.List(intercalate)

type SortName = String
type VarName = String
type Name = String
type ContextDepth = Int
type DefaultErr = Either String

data Sort = DepSort SortName !ContextDepth | SimpleSort SortName
  deriving (Eq)

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

lowerCtx :: Sort -> DefaultErr Sort
lowerCtx (SimpleSort _) = Left "Simple sorts can't have context"
lowerCtx (DepSort nm num) | num > 0 = return (DepSort nm $ num - 1)
                          | otherwise = Left "Context is empty already"
isDepSort :: Sort -> Bool
isDepSort (DepSort _ _) = True
isDepSort _ = False

data FunctionalSymbol = FunSym {
  nameFun   :: Name
, arguments :: [Sort]
, result    :: Sort       --- hack in the parser that gets solved in the checking stage
} deriving (Eq)

instance Show FunctionalSymbol where
  show (FunSym nm args res) =
    nm ++ ": " ++ intercalate "*" (map show args) ++ "->" ++ show res

data MetaVar = MetaVar {
  mContext  :: [VarName]
, mName     :: VarName
} deriving (Eq)

showCtxVar :: [Name] -> String -> String
showCtxVar [] y = y
showCtxVar [x] y = x ++ "." ++ y
showCtxVar args y = bracket (unwords args) ++ "." ++ y

instance Show MetaVar where
  show (MetaVar x y) = showCtxVar x y

data Term = Var VarName              -- xyz
          | TermInCtx [VarName] Term -- (x y).asd
          | FunApp Name [Term]
          | Subst Term VarName Term
    deriving (Eq)

instance Show Term where
  show (Var nm) = nm
  show (TermInCtx x y) = showCtxVar x (show y)
  show (FunApp nm args) = nm ++ bracket (intercalate ", " (map show args))
  show (Subst into vn what) = show into ++ "[" ++ vn ++ ":= " ++ show what ++ "]"

isFunSym :: Term -> Bool
isFunSym FunApp{} = True
isFunSym _ = False

isVar :: Term -> Bool
isVar Var{} = True
isVar (TermInCtx _ Var{}) = True
isVar _ = False

lookupName :: (a -> Name) -> Name -> [a] -> DefaultErr a
lookupName f = lookupName' (\x y -> f x == y)

-- f : tm + name = equal?
lookupName' :: (a -> Name -> Bool) -> Name -> [a] -> DefaultErr a
lookupName' idf name (x : xs) | idf x name = return x
  | otherwise = lookupName' idf name xs
lookupName' _ name _ = Left $ "Name " ++ name ++ " not found!"

allUnique :: Ord a => [a] -> Bool
allUnique a = length a == Set.size (Set.fromList a)

isSubset :: Ord a => [a] -> [a] -> Bool
isSubset a b = 0 == Set.size (Set.difference (Set.fromList a) (Set.fromList b))





--
