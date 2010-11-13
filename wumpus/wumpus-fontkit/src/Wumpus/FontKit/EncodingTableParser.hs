{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.FontKit.EncodingTableParser
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Parser for the Adobe glyphlist file which matches PostScript
-- glyph names to Unicode Scalar values. 
-- 
--------------------------------------------------------------------------------

module Wumpus.FontKit.EncodingTableParser
  ( 
    EncodingTable
  , encodingTable
  , octcode
  , deccode
  , hexcode

  )
  where

import Wumpus.Basic.Utils.ParserCombinators
import qualified Wumpus.Basic.Utils.TokenParsers as P


import Control.Applicative

import Data.Char

type EncodingTable  = [(String,Int)]

encodingTable :: CharParser Int -> CharParser EncodingTable
encodingTable p = 
    whiteSpace *> manyTill1 (glyphDesc p) eof <?> "general failure..."


glyphDesc :: CharParser Int -> CharParser (String,Int)
glyphDesc p = (,) <$> (glyphName <* semi) <*> (p <* lnewline)

glyphName :: CharParser String
glyphName = (many1 $ satisfy isAlphaNum) <?> "glyphName"

semi :: CharParser Char
semi = char ';'

octcode :: CharParser Int
octcode = lexeme P.octBase

deccode :: CharParser Int
deccode = fromIntegral <$> lexeme P.integer

hexcode :: CharParser Int
hexcode = lexeme P.hexBase

lnewline :: CharParser ()
lnewline = skipOne (lexeme newline) 

--------------------------------------------------------------------------------

-- no newline in whitespace


-- Newline is not white-space. Space and tab are, although it
-- looks like the glyphlist does not use whitespace anyway.  
--
lp :: P.LexemeParser
lp = P.commentLineLexemeParser "#" [' ', '\t']


lexeme          :: CharParser a -> CharParser a
lexeme          = P.lexeme lp

whiteSpace      :: CharParser ()
whiteSpace      = P.whiteSpace lp


