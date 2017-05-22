module CodeGen(
  codeGenIO
, gene
-- , module X
) where

import Control.Monad.State
import Control.Monad.Reader
import Data.Map as Map
import Control.Applicative (liftA3)

import Control.Monad.Except (throwError, lift)
import Language.Haskell.Exts.Simple
import Control.Lens

import SortCheck
import AST.Axiom as Ax
import AST.Reduction as Red

import CodeGen.Common as X
import CodeGen.ADT
import CodeGen.MonadInstance
import CodeGen.Infer as X
import CodeGen.Nf as X
import CodeGen.ConsCtx as X

--------------------------------------------------------------------------------
-- Main place
--------------------------------------------------------------------------------
genIO :: FilePath -> FilePath -> IO String
genIO template spec = do
  st <- sortCheckIO spec
  case st of
    Left msg -> putStrLn ("Sortcheck error: " ++ msg) >> return "err"
    Right st' -> do
      k <- parseFile template -- could Fail to parse
      let m = liftA3 runGenM (buildModule <$> k) (pure st') k
      case m of
        ParseFailed _ msg -> putStrLn ("Parse error: " ++ msg) >> return "err"
        ParseOk (Left msg) -> putStrLn ("Codegen error: " ++ msg) >> return "err"
        ParseOk (Right m') -> return $ prettyPrint m'

templateFile = "src/langGenerator/GeneratorTemplates/LangTemplate.hs"

codeGenIO :: FilePath -> IO String
codeGenIO = genIO templateFile

gene :: IO ()
gene = codeGenIO "examples/langSpecs/convoluted.fpl" >>= putStrLn

--------------------------------------------------------------------------------

buildModule :: Module -> GenM Module
buildModule (Module (Just (ModuleHead _ v1 v2)) b c _) = do
  ------------
  symtab <- ask
  unless (all Prelude.null $ Red.stab <$> (Map.elems $ symtab^.SortCheck.reductions)) $
    throwError "As of now all reductions are stable"
  -----------------------------------------------------------
  genTerms
  genSortTypes
  genMonad
  -- genConsCtx
  genInfer
  genNf
  decl <- lift $ gets decls
  return (Module (Just (ModuleHead (ModuleName "Lang") v1 v2)) b c decl)
buildModule x = return x






---
