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
} deriving (Eq, Show)

-- instance Show LangSpec where
--   show (LangSpec _ _ _ axs) = show axs

type SortName = String
type VarName = String
type Name = String
type ContextDepth = Int

data Sort = DepSort SortName !ContextDepth | SimpleSort SortName
  deriving (Eq, Show)

getSortName :: Sort -> SortName
getSortName (DepSort n _) = n
getSortName (SimpleSort n) = n

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

lookupName :: (a -> Name) -> Name -> [a] -> Either String a
lookupName f = lookupName' (\x y -> f x == y)

lookupName' :: (a -> Name -> Bool) -> Name -> [a] -> Either String a
lookupName' idf name (x : xs) | idf x name = return x
  | otherwise = lookupName' idf name xs
lookupName' _ name _ = Left $ "Name " ++ name ++ " not found!"

-- instance Show Axiom where
--   show (Axiom n forall prem concl) = show forall

data Judgement =
  Statement {
  jContext   :: [(VarName, Term)] -- want Variable actually
, jTerm :: Term
, jType :: Term    -- (Term, Term) or (Term, Term, Term)
} |
  Equality {
  jContext   :: [(VarName, Term)]
, eqL  :: Term
, eqR  :: Term
, jType :: Term -- equality t1 = t2 : t3
} deriving (Eq, Show)
-- def as maybe
--

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

allUnique :: Ord a => [a] -> Bool
allUnique a = length a == Set.size (Set.fromList a)

subset :: Ord a => [a] -> [a] -> Bool
subset a b = 0 == Set.size (Set.difference (Set.fromList a) (Set.fromList b))
-- doesn't take context into account ->







--
