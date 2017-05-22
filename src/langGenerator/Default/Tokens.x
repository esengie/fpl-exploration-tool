{
module Tokens where
}

%wrapper "monad"

$digit = 0-9
$alpha = [a-zA-Z]

tokens :-
  $white+                         ;
  "--".*                          ;
  "="                             { tok TEq   }
  ":"                             { tok TCol  }
  ";"                             { tok TSemi }
  "."                             { tok TDot  }
  ","                             { tok TComma}
  "("                             { tok TLP   }
  ")"                             { tok TRP   }
  [A-Z] [$alpha $digit \_ \'\-]*  {  tok' TCtor      }
  [a-z] [$alpha $digit \_ \'\-]*  {  tok' TVar       }
{

tok' :: (String -> Tok) -> AlexAction Token
tok' f (p,_,_,s) i = return $ Token p (f (take i s))

tok :: Tok -> AlexAction Token
tok = tok' . const

data Tok
  = TEq
  | TCol
  | TSemi
  | TDot
  | TComma
  | TLP
  | TRP
  | TCtor String
  | TVar String
  | TokEOF
  deriving (Eq,Show)

data Token = Token AlexPosn Tok
  deriving (Show)

token_posn :: Token -> AlexPosn
token_posn (Token p _) = p

alexEOF :: Alex Token
alexEOF = do
  (p, _, _, _) <- alexGetInput
  return $ Token p TokEOF

lexwrap :: (Token -> Alex a) -> Alex a
lexwrap = (alexMonadScan >>=)

}
