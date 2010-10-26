{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.ToyFontMetrics.Parser
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- AFM file parser.
-- 
--------------------------------------------------------------------------------

module Graphics.ToyFontMetrics.Parser
  where


import Graphics.ToyFontMetrics.Datatypes
import Graphics.ToyFontMetrics.ParserCombinators
import qualified Graphics.ToyFontMetrics.TokenParser as P

import Control.Applicative
import Control.Applicative.Permutation


afmFile :: CharParser AfmFile
afmFile = AfmFile <$> afmHeader <*> metricProps

afmHeader :: CharParser AfmHeader
afmHeader = runPerms $ 
   AfmHeader <$> atom         versionNumber
             <*> optAtom 0    metricsSets
             <*> atom         fontName
             <*> atom       (record "FullName" whiteString)
             <*> atom       (record "FamilyName" whiteString)


metricProps :: CharParser MetricProps
metricProps = pure MetricProps

record :: String -> CharParser a -> CharParser a
record name p = symbol name *> p <* newlineOrEOF

versionNumber   :: CharParser String
versionNumber   = record "StartFontMetrics" $ many1 (digit <|> char '.')

metricsSets     :: CharParser Int
metricsSets     = record "MetricsSets" $ lexeme natural

fontName        :: CharParser String
fontName        = record "FontName" whiteString



keyword :: String -> CharParser ()
keyword ss = skipOne $ symbol ss


newlineOrEOF :: CharParser ()
newlineOrEOF = skipOne newline <|> eof


newline :: CharParser Char
newline = lexeme $ char '\n'

whiteString :: CharParser String
whiteString = many (noneOf ['\n'])

--------------------------------------------------------------------------------

-- no newline in whitespace

afm_lexer :: LexerDef
afm_lexer = emptyDef { whitespace_chars = "\t "
                     , comment_line     = "Comment" }

tp :: P.TokenParsers
tp = P.makeTokenParsers afm_lexer


lexeme          :: CharParser a -> CharParser a
lexeme          = P.lexeme tp

symbol          :: String -> CharParser String
symbol          = P.symbol tp

whiteSpace      :: CharParser ()
whiteSpace      = P.whiteSpace tp
