module Main where

import System.Environment

import CodeGen

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> gene
    x:xs -> codeGenIO x >>= putStrLn



----
