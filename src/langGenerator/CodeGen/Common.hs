module CodeGen.Common
  where

import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Lazy
import Control.Monad.Except (throwError, lift)
import Language.Haskell.Exts.Simple
import Control.Lens

import qualified Data.Char as Char
import qualified Data.Set as Set

import SortCheck.SymbolTable
import AST.Term hiding (Var)

type GenError = String
type Pos = Int
type ErrorM = Either GenError
data CodeGen = Gen {
    count :: Int,
    decls :: [Decl]
  }
type GenM = ReaderT SymbolTable (StateT CodeGen (ErrorM))

-- looking using prettyPrint (yup)
getDecl :: String -> GenM (Decl, Pos)
getDecl nm = do
  decl <- lift $ gets decls
  lift . lift $ getDecl' 0 nm decl
  where getDecl' :: Pos -> String -> [Decl] -> ErrorM (Decl, Pos)
        getDecl' n nm (TypeSig{}:xs) = getDecl' (n+1) nm xs
        getDecl' n nm (x:xs) | take (length nm) (prettyPrint x) == nm = return (x,n)
                             | otherwise = getDecl' (n+1) nm xs
        getDecl' _ nm [] = throwError $ "Haven't found " ++ nm

--------------------------------------------------------------------------------

runGenM :: GenM a -> SymbolTable -> Module -> Either GenError a
runGenM mon st md = evalStateT (runReaderT mon st) (Gen 1 (getDecls md))

getDecls :: Module -> [Decl]
getDecls (Module _ _ _ x) = x
getDecls _ = [] -- error, but whatever, we parse Modules

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

sortToTyCtor :: String -> String
sortToTyCtor x = caps x ++ "Def"

fname = "f"

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

dummyDecl = [((fromParseResult . parseDecl) "x = 1212312323123123213")]

---
