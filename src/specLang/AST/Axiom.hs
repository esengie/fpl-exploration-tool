module AST.Axiom(
  Axiom(..)
) where

import AST.Term
import AST.Judgement

data Axiom = Axiom {
  name       :: Name,
  forallVars :: [(MetaVar, Sort)],
  premise    :: [Judgement],
  conclusion :: Judgement
} deriving (Eq)

instance Show Axiom where
  show (Axiom nm forall prem concl) = concat [nm, " =\n  ",
    showCtx (\ (mv, s) -> show mv ++ ": " ++ show s) forall, "\n    ",
    showCtx show prem, " |--- ", show concl, "\n"]







--
