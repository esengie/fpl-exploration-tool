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
      int             { Token _ (TInt $$) }
      DepSortBeg      { Token _ TDepS }
      SimpleSortBeg   { Token _ TSimpleS }
      FunBeg          { Token _ TFunSyms }
      AxBeg           { Token _ TAxioms }
      V               { Token _ TForall }
      ident           { Token _ (TIdent $$) }
      '='             { Token _ TEq }
      ':'             { Token _ TColon }
      '|-'            { Token _ TTurnstile }
      '|---'          { Token _ TJudgement }
      ','             { Token _ TComma }
      '.'             { Token _ TDot }
      '->'            { Token _ TArrow }
      '*'             { Token _ TTimes }
      '('             { Token _ TLParen }
      ')'             { Token _ TRParen }
      '['             { Token _ TLSubst }
      ']'             { Token _ TRSubst }
      ':='            { Token _ TSubst }
      '\t'            { Token _ TIndent }
      '/t'            { Token _ TDedent }
      '\n'            { Token _ TNewLine } -- currently not used in the parsing stage

%%

LangSpec : DepSorts NotDepSorts FunSyms Axioms { LangSpec $1 $2 $3 $4 }
         | NotDepSorts DepSorts FunSyms Axioms { LangSpec $2 $1 $3 $4 }

DepSorts :  '+' Term           { Plus $1 $3 }
         | Exp1 '-' Term           { Minus $1 $3 }
         | Term                    { Term $1 }

NotDepSorts  : Term '*' Factor         { Times $1 $3 }
      | Term '/' Factor         { Div $1 $3 }
      | Factor                  { Factor $1 }

Factor
      : int                     { Int $1 }
      | var                     { Var $1 }
      | '(' Exp ')'             { Brack $2 }

{

lexwrap :: (Token -> Alex a) -> Alex a
lexwrap = (alexMonadScan' >>=)

happyError :: Token -> Alex a
happyError (Token p t) =
  alexError' p ("parse error at token '" ++ unLex t ++ "'")

parseExp :: FilePath -> String -> Either String Exp
parseExp = runAlex' parse

}
