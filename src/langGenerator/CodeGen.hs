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
import Control.Lens

import qualified Data.Char as Char
import qualified Data.Set as Set
import qualified Data.Map as Map

import SortCheck
import AST

type GenError = String
type Pos = Int
type ErrorM = Either GenError
type GenM = ReaderT SymbolTable (StateT [Decl] (ErrorM))

ctorVarA = QualConDecl Nothing Nothing (ctorDecl "Var" [aVar])

aVar = TyVar (Ident "a")
ctorDecl nm = ConDecl (Ident nm)
tyCon nm = TyCon $ UnQual (Ident nm)
scope1 = TyApp (tyCon "Scope") (TyCon $ Special UnitCon)

-- we know the result is always Term a - so, don't care about it here
-- // irrelevant here but gives insight into future refactoring possibilities
-- (at least until refactor using different types for different sort,
-- but then we'll have to throw away bound and write modules over bound monad - so it's workworkwork)
qualConDecl :: FunctionalSymbol -> QualConDecl
qualConDecl (FunSym nm args _) = QualConDecl Nothing Nothing (ctorDecl (caps nm) (map conArg args))

conArg :: Sort -> Type
conArg (SimpleSort nm) = TyParen (TyApp (tyCon $ sortNmTyName nm) aVar)
conArg (DepSort nm 0) = conArg (SimpleSort nm)
conArg (DepSort nm 1) = TyParen (TyApp (TyApp scope1 (tyCon $ sortNmTyName nm)) aVar)
conArg _ = conArg (SimpleSort "ERRROROROROROEREREROR")

termA = DataDecl DataType Nothing (DHApp (DHead (Ident "Term")) (UnkindedVar (Ident "a")))

-- take definition of a thing and replace with modified, better one
genTerms :: GenM ()
genTerms = do
  lst <- lift get
  (_ , n) <- getDecl "data Term"
  st <- ask
  let sortsWO_tms = Set.toList $ (Set.delete tmName (st^.depSorts)) `Set.union` (st^.simpleSorts)
  let sorts = (\x -> qualConDecl $ FunSym (caps x ++ "Def") [] varSort) <$> sortsWO_tms
  let funSymbs = map qualConDecl $ Map.elems (st^.SortCheck.funSyms) -- this is Lens
  let qConDecls = (ctorVarA : sorts) ++ funSymbs
  let termT = termA qConDecls Nothing
  lift $ put (replace n termT lst)

genMonad :: GenM ()
genMonad = return ()

genInfer :: GenM ()
genInfer = return ()

-- looking using prettyPrint (yup)
getDecl :: String -> GenM (Decl, Pos)
getDecl nm = do
  decl <- lift get
  lift . lift $ getDecl' 0 nm decl
  where getDecl' :: Pos -> String -> [Decl] -> ErrorM (Decl, Pos)
        getDecl' n nm (TypeSig{}:xs) = getDecl' (n+1) nm xs
        getDecl' n nm (x:xs) | take (length nm) (prettyPrint x) == nm = return (x,n)
                           | otherwise = getDecl' (n+1) nm xs
        getDecl' _ nm [] = throwError $ "Haven't found " ++ nm

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
      let m = runGenM st' <$> k
      case m of
        ParseFailed _ msg -> putStrLn $ "Parse error: " ++ msg
        ParseOk (Left msg) -> putStrLn $ "Codegen error: " ++ msg
        ParseOk (Right m') -> (putStrLn . prettyPrint) (getDecls m' !! 0)

fileee = "src/langGenerator/GeneratorTemplates/LangTemplate.hs"
fileee' = "src/langGenerator/experims.hs"

gene :: IO ()
gene = gen fileee' "examples/langSpecs/depTypedLC.fpl"

--------------------------------------------------------------------------------

typeDecl :: String -> Decl
typeDecl nm = TypeDecl (DHead (Ident (sortNmTyName nm))) (TyCon (UnQual (Ident (sortNmTyName tmName))))

sortNmTyName :: String -> String
sortNmTyName nm
  | nm == tmName = "Term"
  | nm == tyName = "Type"
  | otherwise = caps nm

getDecls :: Module -> [Decl]
getDecls (Module _ _ _ x) = x
getDecls _ = [] -- error, but whatever, we parse Modules

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


--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

caps :: String -> String
caps [] = []
caps x = Char.toUpper (head x) : tail x

replace :: Pos -> a -> [a] -> [a]
replace n a lst = (take (n-1) lst) ++ [a] ++ (drop n lst)



---
