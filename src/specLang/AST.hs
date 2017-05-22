module AST(
  LangSpec(..),
  addStabSpec,
  module X
) where

import AST.Term as X
import AST.Judgement as X
import AST.Axiom as Ax
import AST.Reduction as Red

-- this AST is used as output of parsing and input of typechecking
-- this means there some things that are not fully correct after parsing
-- like some sorts are assumed independent, but in reality they are
data LangSpec = LangSpec {
  stabilities     :: Stab
, depSortNames    :: [SortName]
, simpleSortNames :: [SortName]
, funSyms         :: [FunctionalSymbol]
, axioms          :: [Axiom]
, reductions      :: [Reduction]
}

deStabSpec :: LangSpec -> LangSpec
deStabSpec (LangSpec v2 v3 v4 v5 axes reds) = LangSpec v2 v3 v4 v5 axes' reds
  where axes' = (\ax -> ax{Ax.stab = deStab (Ax.stab ax)}) <$> axes
        -- reds' = (\r ->  r{Red.stab = deStab (Red.stab r)}) <$> reds

addStabSpec' :: LangSpec -> LangSpec
addStabSpec' (LangSpec v2 v3 v4 v5 axes reds) = LangSpec v2 v3 v4 v5 axes' reds
  where axes' = (\ax -> ax{Ax.stab = addStab (Ax.stab ax) v2}) <$> axes
        -- reds' = (\r ->  r{Red.stab = addStab (Red.stab r) v2}) <$> reds

addStabSpec :: LangSpec -> LangSpec
addStabSpec = addStabSpec' . deStabSpec

instance Show LangSpec where
  show (LangSpec lst dep simp fun ax red) = concat [
    "Glob stabs:\n", show lst,
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
