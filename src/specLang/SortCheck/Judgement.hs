module SortCheck.Judgement (
  checkJudgem
) where

-- import Control.Monad.Trans.State.Lazy
import Control.Monad.Except (throwError)
import Control.Monad.Trans.Class (lift)
import Control.Monad (when, unless)

import qualified Data.Set as Set

import AST

import SortCheck.SymbolTable as SymbolTable
import SortCheck.Forall (MetaCtx)
import SortCheck.Term
import SortCheck.Sort

--------------------------------------------------------------------------------
-- Judgements


-- given meta vars (forall) and a judgement - SortChecks it
-- first checks context
-- !!!then does all the different judgement specific ops
checkJudgem :: MetaCtx -> Judgement -> SortCheckM ()
checkJudgem meta st = do
  let ctx = jContext st
  vars <- checkCtx meta ctx
  checkJSpecific meta vars st

-- Specific stuff for judgements
-- Statement - check "tm : ty"
-- Equality & reduction - check sorts are same in = & =>
-- Reduction - check right has subset of metas & left starts with funsym
checkJSpecific :: MetaCtx -> Ctx -> Judgement -> SortCheckM ()
checkJSpecific meta ctx (Statement _ tm (Just ty)) = do
  tmSort <- checkTerm meta ctx tm
  tySort <- checkTerm meta ctx ty
  checkTmSort tmSort tm
  checkTySort tySort ty
checkJSpecific meta ctx (Statement _ tm Nothing) = do
  checkTerm meta ctx tm
  return ()
checkJSpecific meta ctx ax@Equality{} = checkEqAndRed meta ctx ax
-- left starts from funsym
checkJSpecific meta ctx red@(Reduct _ l@FunApp{} r ty) = do
  checkEqAndRed meta ctx red
  -- reduct specific stuff:
  -- all metas right in left
  unless (getMetas r `Set.isSubsetOf` getMetas l) $ throwError $
    "Metas to the right of reduction should be present on the left" ++ show red
  where
    getMetas :: Term -> Set.Set SortName
    getMetas = getMetas' Set.empty
      where
        getMetas' st (Var v) = Set.insert v st
        getMetas' st (TermInCtx _ tm) = getMetas' st tm
        getMetas' st (Subst to _ what) = Set.union (getMetas' st to) (getMetas' st what)
        getMetas' st (FunApp _ lst) = foldr (Set.union . getMetas' st) Set.empty lst

checkJSpecific _ _ red = throwError $ "Reducts should start from a funSym " ++ show red


checkEqAndRed :: MetaCtx -> Ctx -> Judgement -> SortCheckM ()
checkEqAndRed meta ctx judg = do
  lSort <- checkTerm meta ctx (jLeft judg)
  rSort <- checkTerm meta ctx (jRight judg)
  checkEqSorts (getSortName lSort) (getSortName rSort) $
    "Sorts are unequal in" ++ show judg
  case jType judg of
    Nothing -> return ()
    Just ty -> do
      checkTmSort lSort (jLeft judg)
      tySort <- checkTerm meta ctx ty
      checkTySort tySort ty

--------------------------------------------------------------------
-- Adds vars to Ctx as it checks
checkCtx :: MetaCtx -> [(VarName, Term)] -> SortCheckM Ctx
checkCtx mCtx = checkCtxVarsHelper mCtx []
  where
    checkCtxVarsHelper :: MetaCtx -> Ctx -> [(VarName, Term)] -> SortCheckM Ctx
    checkCtxVarsHelper _ ctx [] = return ctx
    checkCtxVarsHelper mCtx ctx ((vname, tm):xs) = do
      tySort <- checkTerm mCtx ctx tm
      checkTySort tySort tm

      -- check if it's in metas we have it fixed
      -- !!(this is here and not just
      --         case on "checkTerm mCtx ctx (Var vname)" -- same lookup is inside there!
      --         cause I forgot how destructure it)
      case lookupName (AST.mName . fst) vname mCtx of
        Right _ -> do
          tmSort <- checkTerm mCtx ctx (Var vname)
          checkTmSort tmSort (Var vname)
          checkCtxVarsHelper mCtx ctx xs
      -- ELSE it's a variable
        Left _ -> do
          lift $
            lookupName' (\x name -> name `elem` AST.mContext (fst x))
                        vname
                        mCtx
          checkCtxVarsHelper mCtx (vname : ctx) xs






---
