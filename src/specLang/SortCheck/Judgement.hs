module SortCheck.Judgement (
  checkJudgem
) where

-- import Control.Monad.Trans.State.Lazy
import Control.Monad.Except (throwError)
import Control.Monad.Trans.Class (lift)
import Control.Monad (when, unless)
import Control.Lens

import qualified Data.Set as Set

import AST

import SortCheck.SymbolTable as SymbolTable
import SortCheck.Forall (MetaCtx)
import SortCheck.Term
import SortCheck.Sort

--------------------------------------------------------------------------------
-- Judgements
type TypedCtx = [(VarName, Term)]

-- given meta vars (forall) and a judgement - SortChecks it
-- first checks context
-- !!!then does all the different judgement specific ops
checkJudgem :: MetaCtx -> Judgement -> SortCheckM Judgement
checkJudgem meta jud = do
  let ctx = _jContext jud
  (vctx, ctx') <- checkCtx meta ctx
  checkJSpecific meta vctx (jContext .~ ctx' $ jud)

-- Specific stuff for judgements
-- Statement - check "tm : ty"
-- Equality & reduction - check sorts are same in = & =>
-- Reduction - check right has subset of metas & left starts with funsym
checkJSpecific :: MetaCtx -> Ctx -> Judgement -> SortCheckM Judgement
checkJSpecific meta ctx (Statement ctxx tm (Just ty)) = do
  (tm', tmSort) <- checkTerm meta ctx tm
  (ty', tySort) <- checkTerm meta ctx ty
  checkTmSort tmSort tm'
  checkTySort tySort ty'
  return (Statement ctxx tm' (Just ty'))
checkJSpecific meta ctx (Statement ctxx tm Nothing) = do
  (tm', _) <- checkTerm meta ctx tm
  return (Statement ctxx tm' Nothing)
checkJSpecific meta ctx ax@Equality{} = checkEqAndRed meta ctx ax
-- left starts from funsym
checkJSpecific meta ctx red@(Reduct _ l@FunApp{} r ty) = do
  -- reduct specific stuff:
  -- all metas right in left
  unless (getMetas r `Set.isSubsetOf` getMetas l) $ throwError $
    "Metas to the right of reduction should be present on the left" ++ show red
  checkEqAndRed meta ctx red
  where
    getMetas :: Term -> Set.Set SortName
    getMetas = getMetas' Set.empty
      where
        getMetas' st (Meta v) = Set.insert (mName v) st
        getMetas' st (Var _) = st
        getMetas' st (Subst to _ what) = Set.union (getMetas' st to) (getMetas' st what)
        getMetas' st (FunApp _ lst) = foldr ((Set.union . getMetas' st) . snd) Set.empty lst

checkJSpecific _ _ red = throwError $ "Reducts should start from a funSym " ++ show red


checkEqAndRed :: MetaCtx -> Ctx -> Judgement -> SortCheckM Judgement
checkEqAndRed meta ctx judg = do
  (ltm, lSort) <- checkTerm meta ctx (jLeft judg)
  (rtm, rSort) <- checkTerm meta ctx (jRight judg)
  checkEqSorts (getSortName lSort) (getSortName rSort) $
    "Sorts are unequal in" ++ show judg
  case jType judg of
    Nothing -> return $ retNewJ judg ltm rtm Nothing
    Just ty -> do
      checkTmSort lSort (jLeft judg)
      (tytm, tySort) <- checkTerm meta ctx ty
      checkTySort tySort ty
      return $ retNewJ judg ltm rtm (Just tytm)
  where
    retNewJ :: Judgement -> Term -> Term -> Maybe Term -> Judgement
    retNewJ (Equality ctx _ _ _) l r t = Equality ctx l r t
    retNewJ (Reduct ctx _ _ _) l r t = Reduct ctx l r t
    retNewJ _ _ _ _ = error "retNewJ is in error"

--------------------------------------------------------------------
-- Adds vars to Ctx as it checks
-- Also fixes ctx terms
checkCtx :: MetaCtx -> TypedCtx -> SortCheckM (Ctx, TypedCtx)
checkCtx mCtx = checkCtxVarsHelper mCtx [] []
  where
    checkCtxVarsHelper :: MetaCtx -> Ctx -> TypedCtx -> TypedCtx -> SortCheckM (Ctx, TypedCtx)
    checkCtxVarsHelper _ ctx tctx [] = return (ctx, reverse tctx)
    checkCtxVarsHelper mCtx ctx tctx ((vname, tm):xs) = do
      (tm', tySort) <- checkTerm mCtx ctx tm
      checkTySort tySort tm'
      let tctx' = (vname, tm') : tctx

      -- check if it's in metas we have it fixed
      -- !!(this is here and not just
      --         case on "checkTerm mCtx ctx (Var vname)" -- same lookup is inside there!
      --         cause I forgot how destructure it)
      case lookupName (AST.mName . fst) vname mCtx of
        Right _ -> do
          (tm'', srt) <- checkTerm mCtx ctx (Var vname)
          checkTmSort srt tm''
          checkCtxVarsHelper mCtx ctx tctx' xs
      -- ELSE it's a variable
        Left _ -> do
          ctx' <- checkCtxShadowing ctx [vname]
          checkCtxVarsHelper mCtx ctx' tctx' xs






---
