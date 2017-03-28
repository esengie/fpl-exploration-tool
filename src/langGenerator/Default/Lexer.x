{

module Lexer(
    Token(..)
  , AlexPosn(..)
  , TokenClass(..)
  , unLex
  , Alex(..)
  , runAlex'
  , alexMonadScan
  , mainLex
  ) where

import Prelude hiding (lex)
import Data.List (intercalate)
import Data.Char (isSpace)
import Control.Monad ( liftM, forever, when )

}

%wrapper "monadUserState"

$digit = 0-9
$alpha = [A-Za-z]

tokens :-
  "--" .*                               ; -- kill comments
  $digit+                               { lex (TInt . read) }
  $alpha [$alpha $digit \_ \' \-]*      { lex  TIdent       }
  "."                                   { lex' TDot         }
  ","                                   { lex' TComma       }
  "("                                   { lex' TLParen      }
  ")"                                   { lex' TRParen      }
  ";"                                   { lex' TSemi        }
  $white+                               ;

{

-- The token type, consisting of the source code position and a token class.
data Token = Token AlexPosn TokenClass
  deriving ( Show )

data TokenClass
  = TInt Int
  | TIdent String
  | TDot
  | TSemi
  | TComma
  | TLParen
  | TRParen
  | TEOF
  deriving ( Show )

data AlexUserState = AlexUserState {
    filePath :: FilePath
  }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState "<unknown>"

alexEOF :: Alex Token
alexEOF = do
  (p, _, _, _) <- alexGetInput
  return $ Token p TEOF

-- Unfortunately, we have to extract the matching bit of string
-- ourselves...
lex :: (String -> TokenClass) -> AlexAction Token
lex f = \(p,_,_,s) i -> return $ Token p (f (take i s))

-- For constructing tokens that do not depend on
-- the input
lex' :: TokenClass -> AlexAction Token
lex' = lex . const

-- A variant of runAlex, keeping track of the path of the file we are lexing.
runAlex' :: FilePath -> String -> Alex a -> Either String a
runAlex' fp input a = runAlex input (setFilePath fp >> a)

processInput input = intercalate "\n" $ filter (\l -> not $ all isSpace l)
                                     (lines input)

kkk = do
  f <- readFile "examples/codeOfGenLangs/depTypedLC.lc"
  putStr f

readtoks:: Alex [Token]
readtoks = do
            t <- alexMonadScan
            case t of
              (Token _ TEOF) -> return [t]
              _ -> do
                rest <- readtoks
                return (t:rest)

detok (Token _ d) = d

printHelper (Left s) = [s]
printHelper (Right r) = map (unLex . detok) r

tokenize::String-> Either String [Token]
tokenize s = runAlex' "sad" s readtoks

mainLex' :: String -> [String]
mainLex' input = printHelper (tokenize input)

mainLex :: FilePath -> IO ()
mainLex file = do
  str <- readFile file
  putStr . concat $ mainLex' file


-- For nice parser error messages.
unLex :: TokenClass -> String
unLex (TInt i) = show i
unLex (TIdent s) = s
unLex TComma = ","
unLex TDot = "."
unLex TLParen = "("
unLex TRParen = ")"
unLex TSemi = ";\n"
unLex TEOF = "<EOF>"

getFilePath :: Alex FilePath
getFilePath = liftM filePath alexGetUserState

setFilePath :: FilePath -> Alex ()
setFilePath f = do
  u <- alexGetUserState
  alexSetUserState $ AlexUserState f

}
