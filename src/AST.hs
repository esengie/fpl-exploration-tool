module AST
  where

data LangSpec = LangSpec {
  depSortNames    :: [SortName]
, simpleSortNames :: [SortName]
, funSyms         :: [FunctionalSymbol]
, axioms          :: [Axiom]
} deriving (Eq, Show)

type SortName = String
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


data Variable = DepVar [Name] Name | SimpleVar Name
  deriving (Eq, Show)

data Axiom = Axiom {
  nameAx     :: Name,
  forallVars :: [(Variable, SortName)],
  premise    :: [Judgement],
  conclusion :: Judgement
} deriving (Eq, Show)

data Judgement =
  Statement {  -- rename Judgement
  context   :: [(Name, Term)] -- want Variable actually
, rightTerm :: Term
, rightType :: Term    -- (Term, Term) or (Term, Term, Term)
} |
  Equality {
  context   :: [(Name, Term)]
, rightEqL  :: Term
, rightEqR  :: Term
, tightType :: Term -- equality t1 = t2 : t3
} deriving (Eq, Show)

-- was Variable | FunApp FunSym [Term]
data Term = Var Name | TermInCtx [Name] Term  | FunApp Name [Term] | Subst Term Name Term
    deriving (Eq, Show)
-- doesn't take context into account ->







--
