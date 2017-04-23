module CodeGen
  where

import Language.Haskell.Exts.Simple
import SortCheck

remodule :: Module -> Module
remodule = id








gen :: FilePath -> FilePath -> IO ()
gen template spec = do
  st <- sortCheckIO spec
  case st of
    Left msg -> putStrLn msg
    Right st' -> do
      k <- parseFile template
      putStrLn . show . prettyPrim $ fromParseResult (remodule <$> k)

gene :: IO ()
gene = gen "src/langGenerator/experims.hs" "examples/langSpecs/depTypedLC.fpl" 


---
