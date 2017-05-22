{-# LANGUAGE TemplateHaskell, ExistentialQuantification #-}

import Data.Foldable

import SimpleBound
import PStruct
import Lang
import Grammar



convert :: TermP -> Term String
convert (VarP n) = Var n
convert (Fun "Pi" [([], a), ([v], b)]) = Pi (convert a) (abstract v $ convert b)
convert (Fun "Lam" [([], a), ([v], b)]) = Lam (convert a) (abstract v $ convert b)
convert (Fun "App" [([], a), ([], b), ([v], c)]) = App (convert a) (convert b) (abstract v $ convert c)
convert (Fun "Bool" []) = Bool
convert _ = error "Parse error"


-- spec x = convert . term $ runParse x !! 0











---
