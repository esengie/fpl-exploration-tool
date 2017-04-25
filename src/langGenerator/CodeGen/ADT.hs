module CodeGen.ADT(
  genTerms,
  genSortTypes
) where

import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Lazy
import Control.Monad.Except (throwError, lift)
import Language.Haskell.Exts.Simple
import Control.Lens

import qualified Data.Set as Set
import qualified Data.Map as Map

import SortCheck.SymbolTable
import AST.Term hiding (Var)

import CodeGen.Common

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
  let funSymbs = map qualConDecl $ Map.elems (st^.SortCheck.SymbolTable.funSyms)
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








---
