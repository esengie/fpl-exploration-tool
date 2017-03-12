{-# LANGUAGE TemplateHaskell #-}

module SortCheck.SymbolTable(
  SymbolTable(..),
  SortCheckM(..),
  SortError(..),
  varsInit,
  depSorts,
  simpleSorts,
  funSyms,
  axioms,
  reductions,
  iSymAxiomMap
) where

import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Class (lift)
import Control.Lens

import Data.Map as Map
import Data.Set as Set

import qualified AST
import AST.Axiom
import AST.Reduction

data SymbolTable = SymbolTable {
  _depSorts      :: Set AST.SortName
, _simpleSorts   :: Set AST.SortName
, _funSyms       :: Map AST.Name AST.FunctionalSymbol
, _axioms        :: Map AST.Name Axiom
, _reductions    :: Map AST.Name Reduction
, _iSymAxiomMap  :: Map AST.Name AST.Name -- intro axioms of funSyms
} deriving (Eq, Show)

makeLenses ''SymbolTable

type SortCheckM = StateT SymbolTable (Either SortError)
type SortError = String

varsInit :: SymbolTable
varsInit = SymbolTable Set.empty Set.empty Map.empty Map.empty Map.empty Map.empty




---
