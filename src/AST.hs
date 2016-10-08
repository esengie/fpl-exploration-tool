module AST -- (AST(..)
  -- )
  where

data AST
  = DependentSorts [DependentSort]
  | NotDependentSorts [NotDependentSort]
  | FunctionalSymbol [FunctionalSymbol]
  | Axioms [Axiom]
  deriving (Eq, Show)

type Name = String
type ContextDepth = Int

data DependentSort = DependentSort {
    nameDep :: Name,
    depth :: !ContextDepth
  } deriving (Eq, Show)

data NotDependentSort = NotDependentSort {
    nameNotDep :: Name
  } deriving (Eq, Show)

data Sort = DepSort DependentSort | NDepSort NotDependentSort
  deriving (Eq, Show)

data FunctionalSymbol = FunSym {
    arguments :: [Sort],
    result :: Sort
  } deriving (Eq, Show)


data Variable = DepVar [Name] Name | NDepVar Name
  deriving (Eq, Show)

data Axiom = Axiom {
  name :: Name,
  vars :: [(Variable, Sort)],
  premise :: [Sequent],
  conclusion :: Sequent
} deriving (Eq, Show)

data Sequent = Seq {
  leftS :: [Term],
  rightS :: Term    -- only one?
}
  deriving (Eq, Show)

data Term = Variable | FunApp FunctionalSymbol [Term]
    deriving (Eq, Show)







--
