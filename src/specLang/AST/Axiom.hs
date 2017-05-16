module AST.Axiom(
  Axiom(..)
) where

import AST.Term
import AST.Judgement

data Axiom = Axiom {
  name       :: Name,
  stab       :: Stab,
  forallVars :: [(MetaVar, Sort)],
  premise    :: [Judgement],
  conclusion :: Judgement
}

instance Show Axiom where
  show (Axiom nm st forall prem concl) = concat [nm, " =\n  ",
    showCtx (\ (mv, s) -> show mv ++ ": " ++ show s) forall, "\n    ",
    showCtx show prem, " |--- ", show concl, "\n"]







--
