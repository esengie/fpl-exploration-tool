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
} deriving (Eq)

instance Show Reduction where
  show (Reduction nm forall prem concl) = concat [nm, " =\n  ",
    showCtx (\ (mv, s) -> show mv ++ ": " ++ show s) forall, "\n    ",
    showCtx show prem, " |--- ", show concl, "\n"]





--
