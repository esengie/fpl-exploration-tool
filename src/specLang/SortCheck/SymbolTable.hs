{-# LANGUAGE TemplateHaskell #-}

module SortCheck.SymbolTable(
  SymbolTable(..),
  SortCheckM(..),
  SortError(..),
  varsInit,
  stabs,
  depSorts,
  simpleSorts,
  funSyms,
  axioms,
  reductions,
  iSymAxiomMap,
  funToAx,
  reducts,
  unJust
) where

import Control.Monad.Trans.State.Lazy
import Control.Monad (filterM)
import Control.Monad.Trans.Class (lift)
import Control.Lens

import Data.Map as Map
import Data.Set as Set

import qualified AST
import AST.Axiom as Axiom
import AST.Reduction as Reduction

data SymbolTable = SymbolTable {
  _stabs         :: AST.Stab
, _depSorts      :: Set AST.SortName
, _simpleSorts   :: Set AST.SortName
, _funSyms       :: Map AST.Name AST.FunctionalSymbol
, _axioms        :: Map AST.Name Axiom
, _reductions    :: Map AST.Name Reduction
, _iSymAxiomMap  :: Map AST.Name AST.Name -- intro axioms of funSyms
}

makeLenses ''SymbolTable

type SortCheckM = StateT SymbolTable (Either SortError)
type SortError = String

varsInit :: SymbolTable
varsInit = SymbolTable Nothing Set.empty Set.empty Map.empty Map.empty Map.empty Map.empty

funToAx :: SymbolTable -> AST.FunctionalSymbol -> Maybe Axiom
funToAx table fun = do
  key <- Map.lookup (AST.name fun) (table^.iSymAxiomMap)
  Map.lookup key (table^.axioms)

reducts :: SymbolTable -> AST.FunctionalSymbol -> Either String [Reduction]
reducts table (AST.FunSym nm _ _) = filterM (eqTo nm) $ Map.elems (table^.reductions)
  where
    eqTo nm red = eqTo' nm (Reduction.conclusion red)
    eqTo' nm (AST.Reduct _ (AST.FunApp n _) _ _) = Right $ nm == n
    eqTo' nm x = Left $ "Error in checking, reduct must start with funsym and must be reducts\n" ++ show x


unJust :: Maybe a -> a
unJust (Just a) = a

unJustStr :: String -> (a -> String) -> Maybe a -> String
unJustStr msg _ Nothing = msg
unJustStr _ f (Just a) = f a

instance Show SymbolTable where
  show tb@(SymbolTable _ dep simp fun ax red symAx) = concat [
    "Dep:\n  ", AST.showCtx id (Set.toList dep), "\n",
    "Sim:\n  ", AST.showCtx id (Set.toList simp), "\n",
    "Fun:", AST.showCtx (\x -> helper "\n  " x ++ " intro: " ++
            (unJustStr "Implementation error (or usage): no intro axiom yet!" Axiom.name . funToAx tb) x)
                        (Map.elems fun), "\n",
    "Ax:_______________________",
      AST.showCtx (helper "\n") (Map.elems ax), "\n",
    "Red:______________________",
      AST.showCtx (helper "\n") (Map.elems red), "\n"
    ]
    where
      helper :: (Show a) => String -> a -> String
      helper pref x = pref ++ show x


---
