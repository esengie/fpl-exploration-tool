module AST.Axiom(
  Axiom(..)
) where

import AST.Term
import AST.Judgement

data Axiom = Axiom {
  name     :: Name,
  forallVars :: [(MetaVar, Sort)],
  premise    :: [Judgement],
  conclusion :: Judgement
} deriving (Eq, Show)







--
