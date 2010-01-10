{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Text.ParserCombinators.ZParse.Lexical
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Lexical stuff ...
--
--------------------------------------------------------------------------------

module Text.ParserCombinators.ZParse.Lexical
  ( 
    CharParserT
  , runCharParserT
  
  , withinLine 

  -- Character parsers
  , satisfy
  , anyChar
  , char
  , string
  , oneOf
  , noneOf

  -- Character classification (vis Data.Char)
  , control
  , space
  , lower
  , upper
  , alpha
  , alphaNum
  , printable
  , digit
  , octDigit
  , hexDigit

  , newline
  , tab
  , spaces

  ) where

import Text.ParserCombinators.ZParse.ParseMonad
import Text.ParserCombinators.ZParse.ParseError
import Text.ParserCombinators.ZParse.SourcePosition
import Text.ParserCombinators.ZParse.Utils

import Control.Applicative
import Data.Char

type LexicalInput = [Char]


type CharParserT m a = ParserT LexicalState m a

data LexicalState = LexicalState {
        ls_file_name :: Maybe String,
        ls_src_pos   :: SrcPos,
        ls_err_stk   :: ErrorStack,                   
        ls_input     :: LexicalInput
      }
  deriving (Show)

initialLexicalState :: Maybe String -> String -> LexicalState
initialLexicalState opt_filename input = LexicalState 
    { ls_file_name = opt_filename
    , ls_src_pos   = initialSrcPos
    , ls_err_stk   = []          
    , ls_input     = input
    }

instance HasInput LexicalState where
  type InputStream LexicalState = String
  getInput = ls_input
  setInput xs st = st { ls_input = xs } 

instance HasSrcPos LexicalState where
  getSrcPos = ls_src_pos
  setSrcPos pos s = s { ls_src_pos = pos }


instance HasErrorStack LexicalState where
  getErrorStack =  ls_err_stk
  setErrorStack err s = s { ls_err_stk = err }

-- Return rest-of-input together with parse success so we can run 
-- different parsers successively on a file.
--
runCharParserT :: Monad m 
               => CharParserT m a
               -> Maybe String 
               -> String
               -> m (Either ParseFailure a)
runCharParserT p opt_filename input = runParserT p successK failureK state_0
  where state_0 = initialLexicalState opt_filename input
 

successK :: Monad m => Sk LexicalState (m (Either ParseFailure a)) a
successK = \ans _fk _st -> return $ Right ans

failureK :: Monad m => Fk LexicalState (m (Either ParseFailure a))
failureK st = return $ Left $ failureMessage st


nextState :: (SrcPos -> SrcPos) -> LexicalInput -> LexicalState -> LexicalState
nextState f xs = subst (\s i -> s { ls_input = xs, ls_src_pos = f i }) 
                       ls_src_pos


pushError :: ParseFailure -> LexicalState -> LexicalState
pushError err = subst (\s stk -> s { ls_err_stk = putFailure err stk }) 
                      ls_err_stk

--------------------------------------------------------------------------------
-- control parsers


withinLine :: Monad m => CharParserT m String -> CharParserT m String
withinLine p = ParserT $ \sk fk st -> 
    let (line,rest) = break1 (=='\n') (ls_input st) in
    runParserT p successK failureK (st { ls_input=line }) >>=
      either (\msg -> fk (pushError msg st))
             (\ans -> sk ans fk (nextState incrLine rest st))
  where 
    break1 c cs = let (xs,rest) = break c cs in 
                  case rest of
                    (_:ys) -> (xs,ys)
                    []     -> (xs,[])
                    

--------------------------------------------------------------------------------
-- char parsers

satisfy :: (Char -> Bool) -> CharParserT m Char
satisfy test = ParserT $ \sk fk st -> 
    case getInput st of
      (x:xs) | test x -> sk x fk (setInput xs st)
      _               -> fk st

anyChar :: CharParserT m Char
anyChar = ParserT $ \sk fk st -> 
    case getInput st of
      (x:xs) -> sk x fk (nextState (nextPosChar x) xs st)
      []     -> fk st



string :: String -> CharParserT m String
string sym = ParserT $ \sk fk st -> 
    case stripPrefix sym (getInput st) of
      Just rest -> sk sym fk (nextState (nextPosString sym) rest st)
      Nothing   -> fk st
  where
    stripPrefix []     xs            = Just xs
    stripPrefix (a:as) (x:xs) | a==x = stripPrefix as xs
    stripPrefix _      _             = Nothing


char            :: Char -> CharParserT m Char
char c          = satisfy (==c)

oneOf           :: [Char] -> CharParserT m Char
oneOf cs        = satisfy (`elem` cs)

noneOf          :: [Char] -> CharParserT m Char
noneOf cs       = satisfy (liftA not (`elem` cs))

--------------------------------------------------------------------------------
-- vis-a-vis Data.Char

control         :: CharParserT m Char
control         = satisfy isControl

space           :: CharParserT m Char
space           = satisfy isSpace

lower           :: CharParserT m Char
lower           = satisfy isLower

upper           :: CharParserT m Char
upper           = satisfy isUpper

alpha           :: CharParserT m Char
alpha           = satisfy isAlpha

alphaNum        :: CharParserT m Char
alphaNum        = satisfy isAlphaNum

-- vis isPrint 
-- (name altered to avoid clash with the Prelude) 
--
printable      :: CharParserT m Char
printable      = satisfy isPrint

digit          :: CharParserT m Char
digit          = satisfy isDigit

octDigit       :: CharParserT m Char
octDigit       = satisfy isOctDigit

hexDigit       :: CharParserT m Char
hexDigit       = satisfy isHexDigit

newline        :: CharParserT m Char
newline        = satisfy (=='\n')

tab            :: CharParserT m Char
tab            = satisfy (=='\t')

spaces         :: CharParserT m String
spaces         = many space


  


