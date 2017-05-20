{
module Grammar where
import Tokens
}

%name parseGram
%tokentype { Token  }
%error { parseError }

%token
    var     { Token _ (TVar $$)  }
    ctor    { Token _ (TCtor $$) }
    '='     { Token _ TEq   }
    '.'     { Token _ TDot  }
    ','     { Token _ TComma}
    ':'     { Token _ TCol  }
    ';'     { Token _ TSemi }
    '('     { Token _ TLP   }
    ')'     { Token _ TRP   }

%%

Decls           :   Decl                           { [$1]          }
                |   Decl ';' Decls                 { $1 : $3       }

Decl            :   var '=' Term                   { Decl $1 [] $3 }
                |   var '(' Ctx ')' '=' Term       { Decl $1 $3 $6 }

Term            :   var                            { Var $1        }
                |   ctor                           { Fun $1 []     }
                |   ctor '(' CommaSepCtTms ')'     { Fun $1 $3     }

TypedVar        :   var ':' Term                   { ($1, $3)      }

Ctx             :   TypedVar                       { [$1]          }
                |   TypedVar ',' Ctx               { $1 : $3       }

CombTerm        :   Term                           {  ([], $1)     }
                |   InnerTerm                      {  $1           }

InnerTerm       :   var '.' Term                   { ([$1], $3)    }
                |   '(' SpaceSepNames ')' '.' Term { ($2, $5)      }

CommaSepCtTms   :   CombTerm                       { [$1]          }
                |   CombTerm ',' CommaSepCtTms     { $1 : $3       }

SpaceSepNames   :   var                            { [$1]          }
                |   var SpaceSepNames              { $1 : $2       }


{

runTC :: String -> [Decl]
runTC = parseGram . scanTokens

parseError :: [Token] -> a
parseError tks = error ("Parse error at " ++ lcn ++ "\n")
	where
	lcn = case tks of
    		  [] -> "end of file"
    		  tk:_ -> "line " ++ show l ++ ", column " ++ show c
    			where
    			   AlexPn _ l c = token_posn tk

type Ctx = [String]

data Term = Var String
          | Fun String [(Ctx, Term)]
  deriving (Show, Eq)

data Decl = Decl String [(String, Term)] Term
  deriving (Show, Eq)

}
