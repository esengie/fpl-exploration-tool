module PStruct
  where


type PError = String
type Name = String
type ErrorM = Either PError

type CtxP = [Name]

data TermP = VarP Name
           | Fun  Name [(CtxP, TermP)]
           | AppP Name [TermP]
  deriving (Show, Eq)

data Decl = Decl {
  nm   :: Name,
  ctx  :: [(Name, TermP)],
  term :: TermP
} deriving (Show, Eq)


---
