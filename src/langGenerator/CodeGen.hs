module CodeGen(
  module X,
  gen,
  gene
) where

import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Lazy
import Control.Applicative (liftA3)

import Control.Monad.Except (throwError, lift)
import Language.Haskell.Exts.Simple
import Control.Lens
import Debug.Trace

import SortCheck
import AST hiding (Var)

import CodeGen.Common as X
import CodeGen.ADT
import CodeGen.MonadInstance
import CodeGen.Infer

--------------------------------------------------------------------------------
-- Main place
--------------------------------------------------------------------------------
gen :: FilePath -> FilePath -> IO ()
gen template spec = do
  st <- sortCheckIO spec
  case st of
    Left msg -> putStrLn msg
    Right st' -> do
      k <- parseFile template -- could Fail to parse
      let m = liftA3 runGenM (buildModule <$> k) (pure st') k
      case m of
        ParseFailed _ msg -> putStrLn $ "Parse error: " ++ msg
        ParseOk (Left msg) -> putStrLn $ "Codegen error: " ++ msg
        ParseOk (Right m') -> (putStrLn . prettyPrint) (getDecls m' !! 2)

fileee = "src/langGenerator/GeneratorTemplates/LangTemplate.hs"
fileee' = "src/langGenerator/experims.hs"

gene :: IO ()
gene = gen fileee' "examples/langSpecs/depTypedLC.fpl"

--------------------------------------------------------------------------------

buildModule :: Module -> GenM Module
buildModule (Module a b c _) = do
  genTerms
  genSortTypes
  genMonad
  genInfer
  decl <- lift get
  return (Module a b c decl)
buildModule x = return x








---
