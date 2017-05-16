module AST.Reduction(
  Reduction(..)
) where

import AST.Term
import AST.Judgement

data Reduction = Reduction {
  name     :: Name,
  stability  :: Maybe [Term],
  forallVars :: [(MetaVar, Sort)],
  premise    :: [Judgement],
  conclusion :: Judgement
}

instance Show Reduction where
  show (Reduction nm st forall prem concl) = concat [nm, " =\n  ",
    showCtx (\ (mv, s) -> show mv ++ ": " ++ show s) forall, "\n    ",
    showCtx show prem, " |--- ", show concl, "\n"]





--
