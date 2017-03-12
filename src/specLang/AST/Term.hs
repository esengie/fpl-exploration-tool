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
  isFunSym,
  isVar,
  allUnique,
  isSubset
) where

import qualified Data.Set as Set

type SortName = String
type VarName = String
type Name = String
type ContextDepth = Int
type DefaultErr = Either String

data Sort = DepSort SortName !ContextDepth | SimpleSort SortName
  deriving (Eq, Show)

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
} deriving (Eq, Show)

data MetaVar = MetaVar {
  mContext  :: [VarName]
, mName     :: VarName
} deriving (Eq, Show)

-- was Variable | FunApp FunSym [Term]
data Term = Var VarName              -- xyz
          | TermInCtx [VarName] Term -- (x y).asd
          | FunApp Name [Term]
          | Subst Term VarName Term
    deriving (Eq, Show)

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
