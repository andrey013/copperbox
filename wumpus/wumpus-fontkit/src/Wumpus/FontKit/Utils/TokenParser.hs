{-# LANGUAGE RankNTypes                 #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.FontKit.Utils.TokenParser
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

module Wumpus.FontKit.Utils.TokenParser
  (
    TokenParsers(..)
  , makeTokenParsers

  , natural

  ) where

import Wumpus.FontKit.Utils.ParserCombinators

import Control.Applicative

data TokenParsers  = TokenParsers
      { lexeme     :: forall a. CharParser a -> CharParser a
      , symbol     :: String -> CharParser String
      , whiteSpace :: CharParser ()
      }

-- This formulation seems preferable to a jamming the LexerDef 
-- into a reader monad and carrying it around in the parser. 
-- 
-- With a /parameterized module/ parser definitions e.g. 
-- whiteSpace are static so should be more efficient.
--
-- This module could do some rexporting lifting though...
--

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

    whiteChar     = skipOne $ oneOf $ whitespace_chars lexer_def

    lineComment   = string (comment_line lexer_def) *> 
                    manyTill anyChar endLine        *> return () 

    spanComment   = startComment *> manyTill anyChar endComment *> return ()

    endLine       = skipOne (char '\n') <|> eof
    
    startComment  = string (comment_start lexer_def)
    endComment    = string (comment_end   lexer_def)
    no_line       = null   (comment_line  lexer_def)
    no_span       = null   (comment_start lexer_def)



-- Extra token parsers can be defined without needing to add fields
-- to the TokenParsers data type...
--

natural :: TokenParsers -> CharParser Int
natural cfg = lexeme cfg natural_

natural_ :: CharParser Int
natural_ = liftA read (many1 digit)
