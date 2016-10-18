module AST -- (AST(..)
  -- )
  where

data LangSpec = LangSpec {
  depSorts    :: [Sort]
, notDepSorts :: [Sort]
, funSyms     :: [FunctionalSymbol]
, axioms      :: [Axiom]
} deriving (Eq, Show)

type Name = String
type ContextDepth = Int

data Sort = DepSort Name !ContextDepth | NDepSort Name
  deriving (Eq, Show)

data FunctionalSymbol = FunSym {
  arguments :: [Sort]
, result    :: Sort
} deriving (Eq, Show)


data Variable = DepVar [Name] Name | NDepVar Name
  deriving (Eq, Show)

data Axiom = Axiom {
  name       :: Name,
  vars       :: [(Variable, Sort)],
  premise    :: [Judgement],
  conclusion :: Judgement
} deriving (Eq, Show)

data Judgement =
  Statement {  -- rename Judgement
  leftJ     :: [(Variable, Term)] -- pairs (Var , Term)
, rightTerm :: Term
, rightType :: Term    -- (Term, Term) or (Term, Term, Term)
} |
  Equality {
  leftJ     :: [(Variable, Term)]
, rightEqL  :: Term
, rightEqR  :: Term
, tightType :: Term -- equality t1 = t2 : t3
} deriving (Eq, Show)

data Term = Variable | FunApp FunctionalSymbol [Term]
    deriving (Eq, Show)







--
