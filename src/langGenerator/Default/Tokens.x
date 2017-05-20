{
module Tokens where
}

%wrapper "posn"

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
  [A-Z] [$alpha $digit \_ \'\-]*  {  tok_str TCtor      }
  [a-z] [$alpha $digit \_ \'\-]*  {  tok_str TVar       }
{

tok' f p s = Token p (f s)
tok x = tok' (\s -> x)
tok_str x = tok' (\s -> x s)

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
  deriving (Eq,Show)

data Token = Token AlexPosn Tok
  deriving (Show)

token_posn :: Token -> AlexPosn
token_posn (Token p _) = p

scanTokens = alexScanTokens

}
