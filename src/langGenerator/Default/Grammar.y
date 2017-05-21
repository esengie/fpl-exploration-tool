{
module Grammar (runParse) where

import Control.Monad.Except (throwError)
import Tokens
import PStruct
}

%name parseGram
%tokentype { Token  }
%monad { Alex }
%lexer { lexwrap } { Token _ TokEOF }
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

Term            :   var                            { VarP $1       }
                |   ctor                           { Fun $1 []     }
                |   var  '(' CommaSepTms ')'       { AppP $1 $3    }
                |   ctor '(' CommaSepCtTms ')'     { Fun $1 $3     }

TypedVar        :   var ':' Term                   { ($1, $3)      }

Ctx             :   TypedVar                       { [$1]          }
                |   TypedVar ',' Ctx               { $1 : $3       }

CombTerm        :   Term                           {  ([], $1)     }
                |   InnerTerm                      {  $1           }

InnerTerm       :   var '.' Term                   { ([$1], $3)    }
                |   '(' SpaceSepNames ')' '.' Term { ($2, $5)      }

CommaSepTms     :   Term                           { [$1]          }
                |   Term ',' CommaSepTms           { $1 : $3       }

CommaSepCtTms   :   CombTerm                       { [$1]          }
                |   CombTerm ',' CommaSepCtTms     { $1 : $3       }

SpaceSepNames   :   var                            { [$1]          }
                |   var SpaceSepNames              { $1 : $2       }

{

runParse :: String -> ErrorM [Decl]
runParse s = runAlex s parseGram

parseError :: Token -> Alex a
parseError tk = alexError ("Parse error at " ++ lcn ++ "\n")
	where
	lcn = case tk of
    		  Token _ TokEOF -> "end of file"
    		  _ -> "line " ++ show l ++ ", column " ++ show c
    			where
    			   AlexPn _ l c = token_posn tk

-- checkLevels :: TermP -> ErrorM () -- checks same level var redefs

}
