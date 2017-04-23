module CodeGen
-- (
--   gen,
--   gene
-- )
where

import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Lazy
import Control.Monad.Except (throwError, lift)
import Language.Haskell.Exts.Simple
import qualified Data.Char as Char

import SortCheck
import AST

type GenError = String
type ErrorM = Either GenError
type GenM = ReaderT SymbolTable (StateT [Decl] (ErrorM))

genTerms :: GenM ()
genTerms = do
  var <- getDecl "rnf"
  lift $ put [var]

genMonad :: GenM ()
genMonad = return ()

genInfer :: GenM ()
genInfer = return ()

-- looking using prettyPrint (yup)
getDecl :: String -> GenM Decl
getDecl nm = do
  decl <- lift get
  lift . lift $ getDecl' nm decl
  where getDecl' :: String -> [Decl] -> ErrorM Decl
        getDecl' nm (TypeSig{}:xs) = getDecl' nm xs
        getDecl' nm (x:xs) | take (length nm) (prettyPrint x) == nm = return x
                           | otherwise = getDecl' nm xs
        getDecl' nm [] = throwError $ "Haven't found " ++ nm
--------------------------------------------------------------------------------
-- Main place
--------------------------------------------------------------------------------
gen :: FilePath -> FilePath -> IO ()
gen template spec = do
  st <- sortCheckIO spec
  case st of
    Left msg -> putStrLn msg
    Right st' -> do
      k <- parseFile template
      let m = fromParseResult $ runGenM st' <$> k
      case m of
        Left msg -> putStrLn msg
        Right m' -> (putStrLn . show) (getDecls m' !! 0)

fileee = "src/langGenerator/GeneratorTemplates/LangTemplate.hs"
fileee' = "src/langGenerator/experims.hs"

gene :: IO ()
gene = gen fileee' "examples/langSpecs/depTypedLC.fpl"

--------------------------------------------------------------------------------

sortNmTyName :: String -> String
sortNmTyName nm
  | nm == tmName = "Term"
  | nm == tyName = "Type"
  | otherwise = caps nm
    where caps :: String -> String
          caps [] = []
          caps x = Char.toUpper (head x) : tail x


getDecls :: Module -> [Decl]
getDecls (Module _ _ _ x) = x
getDecls _ = [] -- error, but whatever, we parse Modules

typeDecl :: String -> Decl
typeDecl nm = TypeDecl (DHead (Ident (sortNmTyName nm))) (TyCon (UnQual (Ident (sortNmTyName tmName))))

runGenM :: SymbolTable -> Module -> Either GenError Module
runGenM st m = evalStateT (runReaderT (buildModule m) st) (getDecls m)

buildModule :: Module -> GenM Module
buildModule (Module a b c _) = do
  genTerms
  genMonad
  genInfer
  decl <- lift get
  return (Module a b c decl)
buildModule x = return x



---