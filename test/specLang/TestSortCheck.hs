module TestTypeCheck
  where

import Control.Monad.Trans.State.Lazy
import Control.Monad.Except
import Control.Monad.Trans.Class (lift)
import Data.Maybe (isJust)
import Control.Monad (when)

import qualified Data.Map as Map
import qualified Data.Set as Set

import TypeCheck
import AST

funSms :: Map.Map Name FunctionalSymbol
funSms = Map.fromList []

intros :: Map.Map Name Axiom
intros = Map.fromList []

axims :: Map.Map Name Axiom
axims = Map.fromList []

synTable :: SymbolTable
synTable = SymbolTable (Set.fromList ["tm", "ty"]) Set.empty funSms
  axims (Map.fromList $ zip (Map.keys funSms) (Map.keys intros))

-- tests terms
test1 :: TypeCheckM ()
test1 = do
  st <- get

  return ()

--------------------------------------------------------------------------------
-- Main

-- State = Set DepVars, Set Vars, Map funcs,

typecheck' :: TypeCheckM () -> Either TypeError SymbolTable
typecheck' testFun = execStateT testFun synTable

mainCheck :: TypeCheckM () -> IO ()
mainCheck lang = putStrLn $
  case typecheck' lang of
    Left err -> "hmm " ++ show err
    x -> show x



---
