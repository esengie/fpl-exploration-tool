module SortCheck (
  module X,
  sortCheck,
  runSortCheck,
  mainCheck,
  sortCheckIO
) where

import Control.Monad.Trans.State.Lazy

import AST
import SortCheck.SymbolTable as X
import SortCheck.Sort
import SortCheck.FunSym
import SortCheck.Axiom
import SortCheck.Reduction

import Parser (parseLang)

sortCheck :: LangSpec -> SortCheckM ()
sortCheck lsp = do
    sortCheckSorts lsp
    sortCheckFunSyms (AST.funSyms lsp)
    sortCheckAxioms (AST.axioms lsp)
    sortCheckReductions (AST.reductions lsp)

runSortCheck :: Either String LangSpec -> Either SortError SymbolTable
runSortCheck langSp = do
  lsp' <- langSp
  execStateT (sortCheck lsp') varsInit

--------------------------------------------------------------------------------
-- Main

mainCheck :: FilePath -> IO ()
mainCheck file = do
  st <- sortCheckIO file
  putStrLn $ case st of
    Left err -> "hmm " ++ err
    Right x -> show x

sortCheckIO :: FilePath -> IO (Either SortError SymbolTable)
sortCheckIO file = do
  str <- readFile file
  let lang = parseLang file str
  return $ runSortCheck lang



---
