module AST
  where

import qualified Data.Set as Set
-- this AST is used as output of parsing and input of typechecking
-- this means there some things that are not fully correct after parsing
-- like some sorts are assumed independent, but in reality they are
data LangSpec = LangSpec {
  depSortNames    :: [SortName]
, simpleSortNames :: [SortName]
, funSyms         :: [FunctionalSymbol]
, axioms          :: [Axiom]
, reductions      :: [Reduction]
} deriving (Eq, Show)

-- instance Show LangSpec where
--   show (LangSpec _ _ _ axs) = show axs

type SortName = String
type VarName = String
type Name = String
type ContextDepth = Int
type DefaultErr = Either String

data Sort = DepSort SortName !ContextDepth | SimpleSort SortName
  deriving (Eq, Show)

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

varSort :: Sort
varSort = DepSort tmName 0

tyName :: SortName
tyName = "ty"

tmName :: SortName
tmName = "tm"

data Axiom = Axiom {
  nameAx     :: Name,
  forallVars :: [(MetaVar, Sort)],
  premise    :: [Judgement],
  conclusion :: Judgement
} deriving (Eq, Show)

data Reduction = Reduction {
  rName     :: Name,
  rForallVars :: [(MetaVar, Sort)],
  rPremise    :: [Judgement],
  rConclusion :: Judgement
} deriving (Eq, Show)

lookupName :: (a -> Name) -> Name -> [a] -> DefaultErr a
lookupName f = lookupName' (\x y -> f x == y)

-- f : tm + name = equal?
lookupName' :: (a -> Name -> Bool) -> Name -> [a] -> DefaultErr a
lookupName' idf name (x : xs) | idf x name = return x
  | otherwise = lookupName' idf name xs
lookupName' _ name _ = Left $ "Name " ++ name ++ " not found!"

-- instance Show Axiom where
--   show (Axiom n forall prem concl) = show forall

data Judgement =
  Statement {
  jContext   :: [(VarName, Term)] -- want Variable actually
, jTerm :: Term
, jType :: Maybe Term    -- def as maybe
} |
  Equality {
  jContext   :: [(VarName, Term)]
, jLeft  :: Term
, jRight  :: Term
, jType :: Maybe Term -- equality t1 = t2 : t3
} |
  Reduct {
  jContext   :: [(VarName, Term)]
, jLeft  :: Term
, jRight  :: Term
, jType :: Maybe Term -- equality t1 = t2 : t3
} deriving (Eq, Show)

isEqJudgement :: Judgement -> Bool
isEqJudgement Equality{} = True
isEqJudgement _ = False

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

allUnique :: Ord a => [a] -> Bool
allUnique a = length a == Set.size (Set.fromList a)

subset :: Ord a => [a] -> [a] -> Bool
subset a b = 0 == Set.size (Set.difference (Set.fromList a) (Set.fromList b))
-- doesn't take context into account ->







--
