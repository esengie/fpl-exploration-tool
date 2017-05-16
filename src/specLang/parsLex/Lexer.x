{
-- Lexer reads tokens, add indents where needed and removes all (!) the empty newlines -- maybe this is bad,
-- but otherwise <UNTAB> <UNTAB> <TAB> <TAB> is breaking my parser. (Should've read up on happy and alex more, but eh)

module Lexer(
    Token(..)
  , AlexPosn(..)
  , TokenClass(..)
  , unLex
  , Alex(..)
  , runAlex'
  , alexMonadScan'
  , alexError'
  , mainLex
  ) where
import Prelude hiding (lex)
import Control.Monad ( liftM, forever, when )

import Debug.Trace
import Data.Char
import Data.List

}

%wrapper "monadUserState"

$digit = 0-9
$alpha = [A-Za-z]
@indent = "  " | \t

tokens :-
  "--".*                                ; -- kill comments
  \n (@indent)*                         { startWhite        }
  $digit+                               { lex (TInt . read) }
  "Unstable"                            { lex' TUnstab      }
  "DependentSorts"                      { lex' TDepS        }
  "SimpleSorts"                         { lex' TSimpleS     }
  "FunctionalSymbols"                   { lex' TFunSyms     }
  "Axioms"                              { lex' TAxioms      }
  "Reductions"                          { lex' TReds        }
  "forall"                              { lex' TForall      }
  "def"                                 { lex' TDef         }
  $alpha [$alpha $digit \_ \'\-]*       { lex  TIdent       }
  "="                                   { lex' TEq          }
  "=>"                                  { lex' TReduce      }
  ":"                                   { lex' TColon       }
  "|-"                                  { lex' TTurnstile   }
  "|--" "-"*                            { lex' TJudgement   }
  ","                                   { lex' TComma       }
  "."                                   { lex' TDot         }
  "->"                                  { lex' TArrow       }
  "*"                                   { lex' TTimes       }
  "("                                   { lex' TLParen      }
  ")"                                   { lex' TRParen      }
  "["                                   { lex' TLSubst      }
  "]"                                   { lex' TRSubst      }
  ":="                                  { lex' TSubst       }
  [\ \t\f\v]+                           ;
  \n (@indent)* "--".*                  ; -- kill comments some more

{

data AlexUserState = AlexUserState {
    filePath :: FilePath,
    indentStack::[Int],
    pendingTokens::[Token] }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState "<unknown>" [1] []

-- The token type, consisting of the source code position and a token class.
data Token = Token AlexPosn TokenClass
  deriving ( Show )

data TokenClass
  = TInt Int
  | TUnstab
  | TDepS
  | TSimpleS
  | TFunSyms
  | TAxioms
  | TReds
  | TForall
  | TDef
  | TIdent String
  | TEq
  | TColon
  | TTurnstile
  | TReduce
  | TJudgement
  | TComma
  | TDot
  | TArrow
  | TTimes
  | TLParen
  | TRParen
  | TLSubst
  | TRSubst
  | TSubst
  | TEOF
  | TIndent
  | TDedent
  | TNewLine
  deriving ( Show )

startWhite :: AlexInput -> Int -> Alex Token
startWhite (p,_, _, _) n = do
     indentSt@(cur:_) <- getIndentStack
     when (n>cur) $ do
        setIndentStack (n : indentSt)
        setPendingTokens [Token p TIndent]
     when (n<cur) $ do
        let (pre,post@(top:_)) = span (> n) indentSt
        if top == n then do
           setIndentStack post
           setPendingTokens (map (const (Token p TDedent)) pre)
        else
           alexError' p "Indents don't match"
     return (Token p TNewLine)

-- Need this apparently
alexEOF :: Alex Token
alexEOF = do
  (p,_,_,_) <- alexGetInput
  return $ Token p TEOF

-- Unfortunately, we have to extract the matching bit of string
-- ourselves...
lex :: (String -> TokenClass) -> AlexAction Token
lex f = \(p,_,_,s) i -> return $ Token p (f (take i s))

-- For constructing tokens that do not depend on
-- the input
lex' :: TokenClass -> AlexAction Token
lex' = lex . const

-- We rewrite alexMonadScan' to delegate to alexError' when lexing fails
-- (the default implementation just returns an error message).
alexMonadScan' :: Alex Token
alexMonadScan' = do
    inp <- alexGetInput
    sc <- alexGetStartCode
    pendTok <- getPendingTokens
    case pendTok of
      -- Indents, Dedents and TEOF only
      t:ts -> do
        setPendingTokens ts
        return t
      [] -> case alexScan inp sc of
        AlexEOF -> do
          a@(p,_,_,_) <- alexGetInput
          rval <- startWhite a 1
          pt <- getPendingTokens
          setPendingTokens (pt ++ [Token p TEOF])
          case rval of
            Token _ TNewLine -> alexMonadScan'
            _ -> return rval
        AlexError (p, _, _, s) ->
          alexError' p ("lexical error at character '" ++ take 1 s ++ "'")
        AlexSkip inp' _ -> do
          alexSetInput inp'
          alexMonadScan'
        AlexToken inp' n act -> do
          alexSetInput inp'
          tmp <- act inp n
          case tmp of
            Token _ TNewLine -> alexMonadScan'
            Token _ _ -> act inp n

-- Signal an error, including a commonly accepted source code position.
alexError' :: AlexPosn -> String -> Alex a
alexError' (AlexPn _ l c) msg = do
  fp <- getFilePath
  alexError (fp ++ ":" ++ show l ++ ":" ++ show c ++ ": " ++ msg)

-- A variant of runAlex, keeping track of the path of the file we are lexing.
runAlex' :: FilePath -> String -> Alex a -> Either String a
runAlex' fp input a = runAlex processedInput (setFilePath fp >> a)
  where processedInput = intercalate "\n" $ map (\l -> if (not $ all isSpace l) then l else "--") (lines input)

readtoks:: Alex [Token]
readtoks = do
            t<-alexMonadScan'
            case t of
              (Token _ TEOF) -> return [t]
              _ -> do
                rest<- readtoks
                return (t:rest)

detok (Token _ d) = d

printHelper (Left s) = [s]
printHelper (Right r) = map (unLex . detok) r

tokenize::String-> Either String [Token]
tokenize s = runAlex' "sad" s readtoks

mainLex :: String -> [String]
mainLex input = printHelper (tokenize input)

-- For nice parser error messages.
unLex :: TokenClass -> String
unLex (TInt i) = show i
unLex TUnstab = "!Unstable"
unLex TDepS = "!DepS"
unLex TSimpleS = "!SimpleS"
unLex TFunSyms = "!FunsSyms"
unLex TAxioms = "!Axioms"
unLex TReds   = "!Reductions"
unLex TForall = "FORALL"
unLex TDef = "def"
unLex (TIdent s) = s
unLex TEq = "="
unLex TColon = ":"
unLex TTurnstile = "|-"
unLex TReduce = "=>"
unLex TJudgement = "|---"
unLex TComma = ","
unLex TDot = "."
unLex TArrow = "->"
unLex TTimes = "*"
unLex TLParen = "("
unLex TRParen = ")"
unLex TLSubst = "["
unLex TRSubst = "]"
unLex TSubst = ":="
unLex TEOF = "<EOF>"
unLex TIndent = "<TAB>"
unLex TDedent = "<UNTAB>"
unLex TNewLine = "<NEWLINE>"

getFilePath :: Alex FilePath
getFilePath = liftM filePath alexGetUserState

getIndentStack :: Alex [Int]
getIndentStack = liftM indentStack alexGetUserState

getPendingTokens :: Alex [Token]
getPendingTokens = liftM pendingTokens alexGetUserState

setFilePath :: FilePath -> Alex ()
setFilePath f = do
  u <- alexGetUserState
  alexSetUserState $ AlexUserState f (indentStack u) (pendingTokens u)

setIndentStack :: [Int] -> Alex ()
setIndentStack i = do
  u <- alexGetUserState
  alexSetUserState $ AlexUserState (filePath u) i (pendingTokens u)

setPendingTokens :: [Token] -> Alex ()
setPendingTokens i = do
  u <- alexGetUserState
  alexSetUserState $ AlexUserState (filePath u) (indentStack u) i

}
