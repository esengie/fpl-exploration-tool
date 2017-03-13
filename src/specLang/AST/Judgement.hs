module AST.Judgement(
  Judgement(..),
  isEqJudgement,
  isRedJudgement
  )
  where

import AST.Term

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
, jType :: Maybe Term
} deriving (Eq, Show)

isEqJudgement :: Judgement -> Bool
isEqJudgement Equality{} = True
isEqJudgement _ = False

isRedJudgement :: Judgement -> Bool
isRedJudgement Reduct{} = True
isRedJudgement _ = False







--
