module SortCheck.Term (
  checkTerm,
  checkCtxShadowing
) where

import Control.Monad.Trans.State.Lazy
import Control.Monad.Except (throwError)
import Control.Monad.Trans.Class (lift)
import Control.Lens
import Control.Monad (when, unless)

import qualified Data.Map as Map

import AST
import SortCheck.SymbolTable as SymbolTable
import SortCheck.Forall (MetaCtx)

checkCtxShadowing :: Ctx -> Ctx -> SortCheckM Ctx
checkCtxShadowing ctx vars = do
  unless (allUnique $ vars ++ ctx) $
    throwError $ "Added vars that shadow other vars in ctx:\n" ++ show ctx ++ show vars
  return $ vars ++ ctx

checkTerm :: MetaCtx -> Ctx -> Term -> SortCheckM (Term, Sort)
checkTerm meta ctx tm = do
  tm' <- fixTerm meta ctx tm
  srt <- checkTerm' meta ctx tm'
  return (tm', srt)

-- Need this as a second pass parser stage, as all identifiers are parsed as vars initially
fixTerm :: MetaCtx -> Ctx -> Term -> SortCheckM Term
fixTerm meta ctx (Var name) = do
  st <- get
  if Map.member name (st^.SymbolTable.funSyms)
    then return (FunApp name [])
    else case lookupName (AST.mName . fst) name meta of
      Right (ret, _) -> return $ Meta ret
      Left _ -> return $ Var name

fixTerm meta ctx (FunApp f args) = do
  args' <- mapM (\(ctx', tm) -> do
    ctx'' <- checkCtxShadowing ctx ctx'
    tm' <- fixTerm meta ctx'' tm
    return (ctx', tm')) args
  return (FunApp f args')
fixTerm meta ctx (Subst wher v what) = do
  ctx' <- checkCtxShadowing ctx [v]
  wher' <- fixTerm meta ctx' wher
  what' <- fixTerm meta ctx what
  return (Subst wher' v what')

-- Given a context + forall. (The sort of the term was checked)
-- ??Not all high level terms have to be sort checked (only statements)
checkTerm' :: MetaCtx -> Ctx -> Term -> SortCheckM Sort
checkTerm' meta ctx (Var name) =
    if name `elem` ctx
      then return varSort
      else throwError $ name ++ " is not defined anywhere"
checkTerm' meta ctx (Meta vr) = do
    -- so we're a metavar: check we have all we need in ctx and return our sort
    (mVar, sort) <- lift (lookupName (AST.mName . fst) (mName vr) meta)
    unless (isSubset (mContext mVar) ctx) $
      throwError $ "Not all vars of a metavar are in context! Have:\n\t" ++
        show ctx ++ "\nNeed:\n\t" ++ show (mContext mVar)
    return $ zero sort -- easy for funapps

checkTerm' meta ctx fa@(FunApp f args) = do
  st <- get
  case Map.lookup f (st^.SymbolTable.funSyms) of
    Nothing -> throwError $ "Undefined funSym " ++ show f
    Just (FunSym _ needS res) -> do
      haveS <- mapM (\(ctx', tm) -> do
        ctx'' <- checkCtxShadowing ctx ctx'
        srt <- checkTerm' meta ctx'' tm
        lift $ addToCtx (length ctx') srt) args
      unless (all (uncurry (==)) (zip needS haveS)) $
        throwError $ "Arg sorts don't match, need:\n\t" ++ show needS ++
          "\nbut have:\n\t" ++ show haveS ++ "\nin: " ++ show fa
      return res
checkTerm' meta ctx ar@(Subst v varName what) = do
  -- v must(!) be a metavar
  checkMetaInSubst v
  -- we get: checking of compatibility of varName and v for free,
  -- also that v has all its' context and that it's a MetaVar
  ctx' <- checkCtxShadowing ctx [varName]
  sorte <- checkTerm' meta ctx' v
  -- check that the sort of what is tm
  whatSort <- checkTerm' meta ctx what
  if whatSort /= varSort
    then throwError $ "Can't subst " ++ show whatSort ++ " into a var of sort " ++ show varSort
    else return sorte

checkMetaInSubst :: Term -> SortCheckM ()
checkMetaInSubst (Meta _) = return()
checkMetaInSubst (Subst v _ _) = checkMetaInSubst v
checkMetaInSubst _ = throwError "May substitute only into metavars"

  -- st <- checkTerm' meta (vars ++ ctx) tm
  -- lift $ addToCtx (length vars) st

-- old subst check
-- -- we check that what is a tm
-- (a, b) <- lift (lookupName (AST.mName . fst) name meta)
-- -- we check that out x in T[x:=term] is in our metavars context
-- unless (varName `elem` mContext a) $
--   throwError "Variable substituted has to be in context"
-- -- we also check that this var isn't in Judgements context
-- when (varName `elem` ctx) $
--   throwError "There shouldn't be naming conflicts during subst"





---
