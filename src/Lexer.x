-- No need to fix positions cause we don't care anyway
--
{
module Lexer
  ( Token(..)
  , AlexPosn(..)
  , TokenClass(..)
  , unLex
  , Alex(..)
  , runAlex'
  , alexMonadScan'
  , alexError'
  , maien
  ) where
import Prelude hiding (lex)
import Control.Monad ( liftM, forever, when )
import Debug.Trace
}

%wrapper "monadUserState"

$digit = 0-9
$alpha = [A-Za-z]
@indent = "  " | \t

tokens :-
  "--".*                                ; -- kill comments
  \n (@indent)*                         { startWhite        }
  $digit+                               { lex (TInt . read) }
  $alpha [$alpha $digit \_ \']*         { lex  TIdent       }
  "="                                   { lex' TEq          }
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
  | TIdent String
  | TEq
  | TColon
  | TTurnstile
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

skip' :: AlexAction Token
skip' = \(p,_,_,s) i -> skip s (i-1)

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
      t:ts -> do
        setPendingTokens ts
        return t
      [] -> case alexScan inp sc of
        AlexEOF -> do
          a@(p,_,_,_) <- alexGetInput
          rval <- startWhite a 1
          pt <- getPendingTokens
          setPendingTokens (pt ++ [Token p TEOF])
          return rval
        AlexError (p, _, _, s) ->
          alexError' p ("lexical error at character '" ++ take 1 s ++ "'")
        AlexSkip inp' _ -> do
          alexSetInput inp'
          alexMonadScan'
        AlexToken inp' n act -> do
          alexSetInput inp'
          act (ignorePendingBytes inp) n

-- Signal an error, including a commonly accepted source code position.
alexError' :: AlexPosn -> String -> Alex a
alexError' (AlexPn _ l c) msg = do
  fp <- getFilePath
  alexError (fp ++ ":" ++ show l ++ ":" ++ show c ++ ": " ++ msg)

-- A variant of runAlex, keeping track of the path of the file we are lexing.
runAlex' :: FilePath -> String -> Alex a -> Either String a
runAlex' fp input a = runAlex input (setFilePath fp >> a)

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
tokenize s =
         (runAlex' "sad" s readtoks)

maien :: String -> [String]
maien input = printHelper (tokenize input)

-- For nice parser error messages.
unLex :: TokenClass -> String
unLex (TInt i) = show i
unLex (TIdent s) = s
unLex TEq = "="
unLex TColon = ":"
unLex TTurnstile = "|-"
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
