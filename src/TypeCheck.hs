module TypeCheck
  where

import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Except

import qualified Data.Map as Map
import qualified Data.Set as Set

import AST
import Parser (parseLang)

type TypeError = String
type VarMap = Map.Map String Variable

type TC = ExceptT TypeError (Reader VarMap)

varsInit :: VarMap
varsInit = Map.empty

typecheck :: LangSpec -> TC ()
typecheck = undefined














---
