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
import Debug.Trace

import qualified Data.Char as Char
import qualified Data.Set as Set
import qualified Data.Map as Map

import SortCheck
import AST hiding (Var)

type GenError = String
type Pos = Int
type ErrorM = Either GenError
type GenM = ReaderT SymbolTable (StateT [Decl] (ErrorM))


-- a
aVar = TyVar (Ident "a")
-- = (nm ...) | ...
ctorDecl nm = ConDecl (Ident nm)
-- typeCtor
tyCon nm = TyCon $ UnQual (Ident nm)
-- Scope ()
scope1 = TyApp (tyCon "Scope") (TyCon $ Special UnitCon)
-- data Term a = ...
termA = DataDecl DataType Nothing (DHApp (DHead (Ident "Term")) (UnkindedVar (Ident "a")))
-- Var a
ctorVarA = QualConDecl Nothing Nothing (ctorDecl "Var" [aVar])

-- generates a ctor for funSym
qualConDecl :: FunctionalSymbol -> QualConDecl
qualConDecl (FunSym nm args _) = QualConDecl Nothing Nothing (ctorDecl (caps nm) (map conArg args))

-- Genereates ctor part for funSym arg
conArg :: Sort -> Type
conArg (SimpleSort nm) = TyParen (TyApp (tyCon $ sortToTyName nm) aVar)
conArg (DepSort nm 0) = conArg (SimpleSort nm)
conArg (DepSort nm 1) = TyParen (TyApp (TyApp scope1 (tyCon $ sortToTyName nm)) aVar)
conArg _ = conArg (SimpleSort "ERRROROROROROEREREROR")

-- take definition of AST or function and replace with modified, better one
genTerms :: GenM ()
genTerms = do
  st <- ask
  -- this is Lens
  let sortsWO_tms = sortsWO_tm st
  let sorts = (\x -> qualConDecl $ FunSym (sortToTyCtor x) [] varSort) <$> sortsWO_tms
  let funSymbs = map qualConDecl $ Map.elems (st^.SortCheck.funSyms)
  let qConDecls = (ctorVarA : sorts) ++ funSymbs
  let termT = termA qConDecls Nothing
  -- Generate data Term a = ...
  lst <- lift get
  (_ , n) <- getDecl "data Term"
  lift $ put (replace n [termT] lst)
--------------------------------------------------------------------------------

-- "Type" -> type Type = Term
typeDecl :: String -> Decl
typeDecl nm = TypeDecl (DHead (Ident (sortToTyName nm))) (TyCon (UnQual (Ident (sortToTyName tmName))))

genSortTypes :: GenM ()
genSortTypes = do
  st <- ask
  -- Generate type Type = Term, etc.
  lst <- lift get
  (_, n)<- getDecl "type Type"
  let sortTypes = map typeDecl (sortsWO_tm st)
  lift $ put (replace n sortTypes lst)
--------------------------------------------------------------------------------

vars = zipWith (\x y -> x ++ show y) (repeat "v") ([1..] :: [Integer])
fname = "f"

inApp a op b = Paren (InfixApp (varExp a) (qvarOp op) (varExp b))
  where qvarOp nm = QVarOp (UnQual (Symbol nm))
        varExp nm = Var (UnQual (Ident nm))

funToPat :: FunctionalSymbol -> Pat
funToPat (FunSym nm lst _) = PApp (UnQual (Ident (caps nm)))
                                  (map (PVar . Ident) (take (length lst) vars))

infixMatch :: FunctionalSymbol -> Exp -> Match
infixMatch f@(FunSym nm lst _) exp = InfixMatch (funToPat f)
                                            (Symbol ">>=")
                                            [PVar (Ident fname)]
                                            (UnGuardedRhs exp)
                                            Nothing

boundBind :: FunctionalSymbol -> Exp
boundBind f@(FunSym nm lst _) = foldl App (Con (UnQual (Ident $ caps nm))) (map smart (zip lst vars))
  where smart (srt, nm) | getSortDepth srt == 0 = inApp nm ">>=" fname
                        | getSortDepth srt == 1 = inApp nm ">>>=" fname
                        | otherwise = inApp nm ">>>>>>>>>=" fname

bindVarA :: Match
bindVarA = infixMatch (FunSym "Var" [varSort] varSort)
                      (App (Var (UnQual (Ident fname))) (Var (UnQual (Ident $ vars !! 0))))

monadTerm :: [Match] -> Decl
monadTerm lst = InstDecl Nothing
                  (IRule Nothing Nothing
                         (IHApp (IHCon (UnQual (Ident "Monad"))) (TyCon (UnQual (Ident "Term")))))
                  (Just [InsDecl (FunBind lst)])

genMonad :: GenM ()
genMonad = do
  st <- ask
  let sorts = (\x -> FunSym (sortToTyCtor x) [] varSort) <$> sortsWO_tm st
  let matches = map (\f -> infixMatch f (boundBind f)) $ Map.elems (st^.SortCheck.funSyms) ++ sorts
  let monadInst = monadTerm (bindVarA : matches)

  lst <- lift get
  (_, n)<- getDecl "instance Monad Term"
  lift $ put (replace n [monadInst] lst)

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
        ParseOk (Right m') -> (putStrLn . prettyPrint) (getDecls m' !! 2)

fileee = "src/langGenerator/GeneratorTemplates/LangTemplate.hs"
fileee' = "src/langGenerator/experims.hs"

gene :: IO ()
gene = gen fileee' "examples/langSpecs/depTypedLC.fpl"

--------------------------------------------------------------------------------

getDecls :: Module -> [Decl]
getDecls (Module _ _ _ x) = x
getDecls _ = [] -- error, but whatever, we parse Modules

runGenM :: SymbolTable -> Module -> Either GenError Module
runGenM st m = evalStateT (runReaderT (buildModule m) st) (getDecls m)

buildModule :: Module -> GenM Module
buildModule (Module a b c _) = do
  genTerms
  genSortTypes
  genMonad
  genInfer
  decl <- lift get
  return (Module a b c decl)
buildModule x = return x


--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

sortToTyCtor :: String -> String
sortToTyCtor x = caps x ++ "Def"

sortToTyName :: String -> String
sortToTyName nm
  | nm == tmName = "Term"
  | nm == tyName = "Type"
  | otherwise = caps nm

sortsWO_tm :: SymbolTable -> [String]
sortsWO_tm st = Set.toList $ (Set.delete tmName (st^.depSorts)) `Set.union` (st^.simpleSorts)

caps :: String -> String
caps [] = []
caps x = Char.toUpper (head x) : tail x

replace :: Pos -> [a] -> [a] -> [a]
replace n xs lst = (take n lst) ++ xs ++ (drop (n + 1) lst)

dummy = [((fromParseResult . parseDecl) "x = 1212312323123123213")]

---
