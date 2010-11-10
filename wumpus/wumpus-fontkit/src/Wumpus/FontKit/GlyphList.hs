{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.FontKit.GlyphList
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

module Wumpus.FontKit.GlyphList
  where


import Wumpus.Basic.Utils.ParserCombinators
import qualified Wumpus.Basic.Utils.TokenParsers as P


import Control.Applicative

import Data.Char

type UCode = [Int]

afmFile :: CharParser [(String,UCode)]
afmFile = whiteSpace *> manyTill1 glyphDesc eof <?> "general failure..."


glyphDesc :: CharParser (String,UCode)
glyphDesc = (,) <$> (glyphName <* semi) <*> (ucode <* lnewline)

glyphName :: CharParser String
glyphName = (many1 $ satisfy isAlphaNum) <?> "glyphName"

semi :: CharParser Char
semi = char ';'

ucode :: CharParser UCode
ucode = many1 hexInt

hexInt :: CharParser Int
hexInt = lexeme P.hexBase

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

symbol          :: String -> CharParser String
symbol          = lexeme . string

whiteSpace      :: CharParser ()
whiteSpace      = P.whiteSpace lp


integer         :: CharParser Integer
integer         = lexeme P.integer

int             :: CharParser Int
int             = fromIntegral <$> integer

double          :: CharParser Double
double          = lexeme P.double
