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
  , anyChar
  , token
  ) where

import Text.ParserCombinators.ZParse.ParseMonad
import Text.ParserCombinators.ZParse.ParseError
import Text.ParserCombinators.ZParse.SourcePosition


type CharParserT m a = ParserT LexicalState m a

data LexicalState = LexicalState {
        ls_file_name :: Maybe String,
        ls_src_pos   :: SrcPos,
        ls_err_stk   :: ErrorStack,                   
        ls_input     :: String
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



runCharParserT :: Monad m 
               => ParserT LexicalState m r -> String -> m (Either ParseFailure r)
runCharParserT ma str = 
   runParserT ma (\sk _ _ -> return $ Right sk)
                 (\st     -> return $ Left $ failureMessage st)
                 state_0
  where
    state_0 = initialLexicalState Nothing str


anyChar :: CharParserT m Char
anyChar = ParserT $ \sk fk st -> 
    case getInput st of
      (x:xs) -> sk x fk (setInput xs st)
      []     -> fk st


token :: String -> CharParserT m String
token sym = ParserT $ \sk fk st -> 
    case stripPrefix sym (getInput st) of
      Just rest -> sk sym fk (setInput rest st)
      Nothing   -> fk st
  where
    stripPrefix []     xs            = Just xs
    stripPrefix (a:as) (x:xs) | a==x = stripPrefix as xs
    stripPrefix _      _             = Nothing







  


