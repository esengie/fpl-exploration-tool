{-# LANGUAGE TemplateHaskell #-}

module AST.Judgement(
  Judgement(..),
  isEqJudgement,
  isRedJudgement,
  showCtx,
  jContext
  )
  where

import Data.List(intercalate)
import Control.Lens

import AST.Term

data Judgement =
  Statement {
  _jContext   :: [(VarName, Term)]
, jTerm :: Term
, jType :: Maybe Term    -- def as maybe
} |
  Equality {
  _jContext   :: [(VarName, Term)]
, jLeft  :: Term
, jRight  :: Term
, jType :: Maybe Term -- equality t1 = t2 : Maybe t3
} |
  Reduct {
  _jContext   :: [(VarName, Term)]
, jLeft  :: Term
, jRight  :: Term
, jType :: Maybe Term
} deriving (Eq)

makeLenses ''Judgement


instance Show Judgement where
  show (Statement ctx tm Nothing) = concat [
    showCtx showVnTm ctx,
    "|- ", show tm, " def"]
  show (Statement ctx tm (Just ty)) = concat [
      showCtx showVnTm ctx,
      "|- ", show tm, ": ", show ty]
  show a@Equality{} = showEqRed a " = "
  show a@Reduct{} = showEqRed a " => "


showCtx :: (a -> String) -> [a] -> String
showCtx f lst = intercalate ", " (map f lst)

showVnTm :: (VarName, Term) -> String
showVnTm (a, b) = a ++ " :" ++ show b

showEqRed :: Judgement -> String -> String
showEqRed a@Statement{} _ = show a
showEqRed a eq = case jType a of
  Nothing -> concat [
      showCtx showVnTm (a^.jContext),
      "|- ", show (jLeft a), eq, show (jRight a)]
  Just ty -> concat [
      showCtx showVnTm (a^.jContext),
      "|- ", show (jLeft a), eq, show (jRight a), ": ", show ty]

isEqJudgement :: Judgement -> Bool
isEqJudgement Equality{} = True
isEqJudgement _ = False

isRedJudgement :: Judgement -> Bool
isRedJudgement Reduct{} = True
isRedJudgement _ = False







--
