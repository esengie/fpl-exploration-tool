module AST(
  LangSpec(..),
  module X
) where

import AST.Term as X
import AST.Judgement as X
import AST.Axiom
import AST.Reduction

-- this AST is used as output of parsing and input of typechecking
-- this means there some things that are not fully correct after parsing
-- like some sorts are assumed independent, but in reality they are
data LangSpec = LangSpec {
  depSortNames    :: [SortName]
, simpleSortNames :: [SortName]
, funSyms         :: [FunctionalSymbol]
, axioms          :: [Axiom]
, reductions      :: [Reduction]
} deriving (Eq)

instance Show LangSpec where
  show (LangSpec dep simp fun ax red) = concat [
    "Dep:\n  ", showCtx id dep, "\n",
    "Sim:\n  ", showCtx id simp, "\n",
    "Fun:", showCtx (helper "\n  ") fun, "\n",
    "Ax:_______________________",
      showCtx (helper "\n") ax, "\n",
    "Red:______________________",
    showCtx (helper "\n") red, "\n"
    ]
    where
      helper :: (Show a) => String -> a -> String
      helper pref x = pref ++ show x







--
