module AST.Reduction(
  Reduction(..)
) where

import AST.Term
import AST.Judgement

data Reduction = Reduction {
  name     :: Name,
  forallVars :: [(MetaVar, Sort)],
  premise    :: [Judgement],
  conclusion :: Judgement
} deriving (Eq, Show)





--
