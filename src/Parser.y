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

FunSym          :   ident ':' SortsLeft '->' ident            { FunSym $1 $3 (SimpleSort $5) }  -- hacky
SortsLeft       :   SortLeft                                  { [$1] }
                |   SortLeft '*' SortsLeft                    { $1 : $3 }
SortLeft        :   ident                                     { SimpleSort $1 }
                |   '(' ident ',' int ')'                     { DepSort $2 $4 }

AxiomsAll       :   axBeg ':' '\t' Axioms '/t'                { $4 }
Axioms          :   Axiom                                     { [$1] }
                |   Axiom Axioms                              { $1 : $2 }
Axiom           :   ident '=' '\t' Forall '\t'
                      Premise '|---' JudgementNoEq '/t' '/t'  { Axiom $1 $4 $6 $8 }

Forall          :   V ForallVars                              { $2 }
ForallVars      :   ForallVar                                 { [$1] }
                |   ForallVar ',' ForallVars                  { $1 : $3 }
ForallVar       :   VarName ':' ident                         { ($1 , SimpleSort $3) }  -- hacky
VarName         :   ident                                     { MetaVar [] $1 }
                |   ident '.' ident                           { MetaVar [$1] $3 }
                |   '(' SpaceSepNames ')' '.' ident           { MetaVar $2 $5 }

SpaceSepNames   :   ident                                     { [$1] }
                |   ident SpaceSepNames                       { $1 : $2 }

Premise         :   JudgementWithEq                           { [$1] }
                |   JudgementWithEq ',' Premise               { $1 : $3 }

JudgementNoEq   :   '|-' Term ':' Term                        { Statement [] $2 $4 }
                |   Context '|-' Term ':' Term                { Statement $1 $3 $5 }


JudgementWithEq :   '|-' Term ':' Term                        { Statement [] $2 $4 }
                |   '|-' Term '=' Term ':' Term               { Equality [] $2 $4 $6 }
                |   Context '|-' Term ':' Term                { Statement $1 $3 $5 }
                |   Context '|-' Term '=' Term ':' Term       { Equality $1 $3 $5 $7 }

Context         :   ident ':' Term                            { [($1, $3)] }
                |   ident ':' Term ',' Context                { ($1, $3) : $5 }


---      neeed [] much tighter than others + no (a b). stuff on the upper levels!
Term            :   ident                                     { Var $1 }
                |   ident '(' CommaSepTerms ')'               { FunApp $1 $3 }
                |   Term '[' ident ':=' Term ']'              { Subst $1 $3 $5 }

CombTerm        :   Term                                      {  $1  }
                |   InnerTerm                                 {  $1  }

InnerTerm       :   ident '.' Term                            { TermInCtx [$1] $3}
                |   '(' SpaceSepNames ')' '.' Term            { TermInCtx $2 $5}

CommaSepTerms   :   CombTerm                                  { [$1] }
                |   CombTerm ',' CommaSepTerms                { $1 : $3 }


{

lexwrap :: (Token -> Alex a) -> Alex a
lexwrap = (alexMonadScan' >>=)

happyError :: Token -> Alex a
happyError (Token p t) =
  alexError' p ("parse error at token '" ++ unLex t ++ "'")

parseLang :: FilePath -> String -> Either String LangSpec
parseLang fp code = runAlex' fp code parse

}
