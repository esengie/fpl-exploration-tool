
module AST (
  Term(..),
  Brujin(..)
  ) where

type Brujin = String

data Term = Var Brujin | Bool | False | True |
  Pi Term ([Term], Term) | App Term Term | If Term Term Term

bool : ty
false : tm
true : tm
pi : (ty , 0) * (ty , 1) -> ty  -- sigma : (ty , 0) * (ty , 1) -> ty
lam : (ty , 0) * (tm , 1) -> tm
app : (tm , 0) * (tm , 0) -> tm













--
