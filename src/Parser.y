{
module Parser(parseLang) where

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
      funSymBeg       { Token _ TFunSyms    }
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
  --  '\n'            { Token _ TNewLine    } -- currently not used in the parsing stage

%%

LangSpec        :   DepSorts SimpleSorts FunSyms AxiomsAll    { LangSpec $1 $2 $3 $4 }
                |   SimpleSorts DepSorts FunSyms AxiomsAll    { LangSpec $2 $1 $3 $4 }

SimpleSorts     :   simpleSortBeg ':' '\t' CommaSepNames '/t' { $4 }
DepSorts        :   depSortBeg ':' '\t' CommaSepNames '/t'    { $4 }
CommaSepNames   :   ident                                     { [$1] }
                |   ident ',' CommaSepNames                   { $1 : $3 }

FunSyms         :   funSymBeg ':' '\t' FunSymsH '/t'          { $4 }
FunSymsH        :   FunSym                                    { [$1] }
                |   FunSym FunSymsH                           { $1 : $2 }

FunSym          :   ident ':' SortsLeft '->' ident            { FunSym $1 $3 $5 }
SortsLeft       :   SortLeft                                  { [$1] }
                |   SortLeft '*' SortsLeft                    { $1 : $3 }
SortLeft        :   ident                                     { SimpleSort $1 }
                |   '(' ident ',' int ')'                     { DepSort $2 $4 }

AxiomsAll       :   axBeg ':' '\t' Axioms '/t'                { $4 }
Axioms          :   Axiom                                     { [$1] }
                |   Axiom Axioms                              { $1 : $2 }
Axiom           :   ident '=' '\t' Forall '\t'
                          Premise '|---' Judgement '/t' '/t'  { Axiom $1 $4 $6 $8 }

Forall          :   V ForallVars                              { $2 }
ForallVars      :   ForallVar                                 { [$1] }
                |   ForallVar ',' ForallVars                  { $1 : $3 }
ForallVar       :   VarName ':' ident                         { ($1 , $3) }
VarName         :   ident                                     { SimpleVar $1 }
                |   ident '.' ident                           { DepVar [$1] $3 }
                |   '(' SpaceSepNames ')' '.' ident           { DepVar $2 $5 }

SpaceSepNames   :   ident                                     { [$1] }
                |   ident SpaceSepNames                       { $1 : $2 }

Premise         :   Judgement                                 { [$1] }
                |   Judgement ',' Premise                     { $1 : $3 }

Judgement       :   '|-' Term ':' Term                        { Statement [] $2 $4 }
                |   '|-' Term '=' Term ':' Term               { Equality [] $2 $4 $6 }
                |   Context '|-' Term ':' Term                { Statement $1 $3 $5 }
                |   Context '|-' Term '=' Term ':' Term       { Equality $1 $3 $5 $7 }

Context         :   ident ':' Term                            { [($1, $3)] }
                |   ident ':' Term ',' Context                { ($1, $3) : $5 }

Term            :   VarName                                   { Var $1 }
                |   ident '(' CommaSepTerms ')'               { FunApp $1 $3 }
                |   Term '[' ident ':=' Term ']'              { Subst $1 $3 $5 }

CommaSepTerms   :   Term                                      { [$1] }
                |   Term ',' CommaSepTerms                    { $1 : $3 }


{

lexwrap :: (Token -> Alex a) -> Alex a
lexwrap = (alexMonadScan' >>=)

happyError :: Token -> Alex a
happyError (Token p t) =
  alexError' p ("parse error at token '" ++ unLex t ++ "'")

parseLang :: FilePath -> String -> Either String LangSpec
parseLang fp code = runAlex' fp code parse

}
