module AST -- (AST(..)
  -- )
  where

data Spec = Spec {
   depSorts :: [Sort]
   , notDepSorts :: [Sort]
   , funSyms :: [FunctionalSymbol]
   , axioms :: [Axiom]
 }
  deriving (Eq, Show)

type Name = String
type ContextDepth = Int

data Sort = DepSort Name !ContextDepth | NDepSort Name
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
  premise :: [Judgement],
  conclusion :: Judgement
} deriving (Eq, Show)

data Judgement = RVar {  -- rename Judgement
  leftJ :: [(Variable, Term)], -- pairs (Var , Term)
  rightTy :: (Variable, Term)    -- (Var, Term) or (Term, Term, Type)
}              | REqual {
  leftJ :: [(Variable, Term)],
  rightEq :: (Term, Term, Term) -- equality
}
  deriving (Eq, Show)

data Term = Variable | FunApp FunctionalSymbol [Term]
    deriving (Eq, Show)







--
