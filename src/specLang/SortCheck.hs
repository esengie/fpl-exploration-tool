module SortCheck (
  module X,
  sortCheck,
  runSortCheck,
  mainCheck
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
  str <- readFile file
  let lang = parseLang (show file) str
  putStrLn $ case runSortCheck lang of
    Left err -> "hmm " ++ err
    Right x -> show x







---
