{
module Parser(parseExp) where

import AST
import Lexer

}

%name parse
%tokentype { Token }
%monad { Alex }
%lexer { lexwrap } { Token _ TEOF }
-- Without this we get a type error
%error { happyError }

%token
      int             { Token _ (TInt $$)   }
      ident           { Token _ (TIdent $$) }
      depSortBeg      { Token _ TDepS       }
      simpleSortBeg   { Token _ TSimpleS    }
      funSymBeg          { Token _ TFunSyms    }
      axBeg           { Token _ TAxioms     }
      V               { Token _ TForall     }
      '='             { Token _ TEq         }
      ':'             { Token _ TColon      }
      '|-'            { Token _ TTurnstile  }
      '|---'          { Token _ TJudgement  }
      ','             { Token _ TComma      }
      '.'             { Token _ TDot        }
      '->'            { Token _ TArrow      }
      '*'             { Token _ TTimes      }
      '('             { Token _ TLParen     }
      ')'             { Token _ TRParen     }
      '['             { Token _ TLSubst     }
      ']'             { Token _ TRSubst     }
      ':='            { Token _ TSubst      }
      '\t'            { Token _ TIndent     }
      '/t'            { Token _ TDedent     }
      '\n'            { Token _ TNewLine    } -- currently not used in the parsing stage

%%

LangSpec     : DepSorts SimpleSorts FunSyms AxiomsAll { LangSpec $1 $2 $3 $4 }
             | SimpleSorts DepSorts FunSyms AxiomsAll { LangSpec $2 $1 $3 $4 }

SimpleSorts  : simpleSortBeg ':' '\t' SortNames '/t'     { $4 }
DepSorts     : depSortBeg ':' '\t' SortNames '/t'        { $4 }
SortNames    : ident                        { [$1] }
             | ident ',' SortNames             { $1 : $3 }

FunSyms      : funSymBeg ':' '\t' FunSymsH '/t'     { $4 }
FunSymsH     : FunSym                       { [$1] }
             | FunSym FunSymsH              { $1 : $2 }

FunSym       : ident ':' SortsLeft '->' ident { FunSym $3 $5 }
SortsLeft    : SortLeft                      { [$1] }
             | SortLeft '*' SortsLeft            { $1 : $3 }
SortLeft     : ident                         { SimpleSort $1 }
             | '(' ident ',' int ')'         { DepSort $2 $4 }

AxiomsAll    : axBeg ':' '\t' Axioms '/t' { $4 }
Axioms       : Axiom                         { [$1] }
             | Axiom Axioms                  { $1 : $2 }
Axiom        : ident                         { Axiom $1 [] [] [] }


{

lexwrap :: (Token -> Alex a) -> Alex a
lexwrap = (alexMonadScan' >>=)

happyError :: Token -> Alex a
happyError (Token p t) =
  alexError' p ("parse error at token '" ++ unLex t ++ "'")

parseExp :: FilePath -> String -> Either String LangSpec
parseExp fp st = runAlex' fp st parse

}
