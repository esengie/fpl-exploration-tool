module AST.Judgement(
  Judgement(..),
  isEqJudgement,
  isRedJudgement,
  showCtx
  )
  where

import Data.List(intercalate)

import AST.Term

data Judgement =
  Statement {
  jContext   :: [(VarName, Term)]
, jTerm :: Term
, jType :: Maybe Term    -- def as maybe
} |
  Equality {
  jContext   :: [(VarName, Term)]
, jLeft  :: Term
, jRight  :: Term
, jType :: Maybe Term -- equality t1 = t2 : Maybe t3
} |
  Reduct {
  jContext   :: [(VarName, Term)]
, jLeft  :: Term
, jRight  :: Term
, jType :: Maybe Term
} deriving (Eq)

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
      showCtx showVnTm (jContext a),
      "|- ", show (jLeft a), eq, show (jRight a)]
  Just ty -> concat [
      showCtx showVnTm (jContext a),
      "|- ", show (jLeft a), eq, show (jRight a), ": ", show ty]

isEqJudgement :: Judgement -> Bool
isEqJudgement Equality{} = True
isEqJudgement _ = False

isRedJudgement :: Judgement -> Bool
isRedJudgement Reduct{} = True
isRedJudgement _ = False







--
