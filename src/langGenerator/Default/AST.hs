
module AST (
  Term(..),
  Type(..),
  Brujin(..)
  ) where

type Brujin = String

data Type = Pi Type Type | Bool

data Term = Var Brujin | False | True
  | Lam Type Term
  | App Term Term
  | If Term Term Term













--
