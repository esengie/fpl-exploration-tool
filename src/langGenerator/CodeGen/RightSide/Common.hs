{-# LANGUAGE TemplateHaskell #-}

module CodeGen.RightSide.Common
  where

import Control.Monad.State
import Control.Monad.Except (throwError, lift)
import Control.Lens
import Language.Haskell.Exts.Simple

import qualified Data.Map as Map

import AST hiding (Var, name, Name)
import qualified AST (Term(Var), Name)

import CodeGen.Common hiding (count)

-- T def - need forall
-- f() def - need funsyms

data Q = Q {
  _count :: Int,
  _foralls :: Map.Map MetaVar Sort,
  -- metaVar as in forall x.T -> termExp
  _metas :: Map.Map MetaVar [(Ctx, Exp)],
  _doStmts :: [Stmt], -- this will be concatted

  -- we define some metavars on the right of :, others we need to check
  _juds  :: Juds,
  -- various counters - the outer monad will have to use this
  _toGen :: ToGen,
  -- need this to get the kinds of funsyms
  _funsyms :: Map.Map AST.Name FunctionalSymbol
}

data Juds = Juds {
  _metaTyDefs :: [(MetaVar, Judgement)], -- some var will be added to the metas map (|- g : T)
  _notDefsTy :: [(Term, Judgement)], -- here it will not, so v_i <- infer ...  (|- g : exp(T, G))
  _otherJuds :: [Judgement]  -- |- g def
}

makeLenses ''Juds
makeLenses ''Q

type BldRM = StateT Q (ErrorM)

initJuds :: Juds
initJuds = Juds [] [] []

runBM :: (Map.Map AST.Name FunctionalSymbol) -> BldRM a -> ErrorM a
runBM fss mon = evalStateT mon (Q 0 Map.empty Map.empty [] initJuds initGen fss)

fresh :: BldRM VName
fresh = do
  i <- gets _count
  count += 1
  return (vars !! i)

updateMap :: MetaVar -> v -> Map.Map MetaVar [(Ctx,v)] -> Map.Map MetaVar [(Ctx,v)]
updateMap k v m = case Map.lookup k m of
  Nothing -> Map.insert k [(mContext k,v)] m
  (Just vs) -> Map.insert k ((mContext k,v):vs) m

appendExp :: Exp -> BldRM ()
appendExp ex = appendStmt (Qualifier ex)

appendStmt :: Stmt -> BldRM ()
appendStmt st = doStmts %= (++ [st])

labelJudgement :: Judgement -> BldRM ()
labelJudgement jud =
  case jType jud of
    Nothing -> juds.otherJuds %= (jud :)
    Just (Meta mv) -> juds.metaTyDefs %= ((mv, jud) :)
    Just tm -> juds.notDefsTy %= ((tm, jud) :)


---
