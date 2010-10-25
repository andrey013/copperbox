{-# LANGUAGE RankNTypes                 #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.ToyFontMetrics.TokenParser
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- White-space and comment handling.
--
-- Acknowledgment - this is a smaller version Parsec\'s Token 
-- module. Credit and thanks for the technique is owed to Daan 
-- Leijen. 
-- 
--------------------------------------------------------------------------------

module Graphics.ToyFontMetrics.TokenParser
  (
    TokenParsers(..)
  , makeTokenParsers

  ) where

import Graphics.ToyFontMetrics.ParserCombinators

import Control.Applicative

data TokenParsers  = TokenParsers
      { lexeme     :: forall a. CharParser a -> CharParser a
      , symbol     :: String -> CharParser String
      , whiteSpace :: CharParser ()
      }


makeTokenParsers :: LexerDef -> TokenParsers
makeTokenParsers lexer_def = 
    TokenParsers 
      { lexeme     = plexeme
      , symbol     = psymbol
      , whiteSpace = pwhiteSpace 
      }
  where
    plexeme p = p <* pwhiteSpace

    psymbol ss = plexeme $ string ss

    pwhiteSpace 
      | no_line && no_span = skipMany whiteChar
      | no_line            = skipMany (whiteChar <|> spanComment)
      | no_span            = skipMany (whiteChar <|> lineComment)
      | otherwise          = skipMany (whiteChar <|> lineComment 
                                                 <|> spanComment)

    whiteChar = skipOne $ oneOf $ whitespace_chars lexer_def

    lineComment = string (comment_line lexer_def) *> 
                  manyTill anyChar (char '\n')    *> return () 

    spanComment = startComment *> manyTill anyChar endComment *> return ()

    startComment = string (comment_start lexer_def)
    endComment   = string (comment_end   lexer_def)
    no_line      = null   (comment_line  lexer_def)
    no_span      = null   (comment_start lexer_def)