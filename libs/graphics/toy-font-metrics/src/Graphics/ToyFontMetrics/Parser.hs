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


record :: String -> CharParser a -> CharParser a
record name p = symbol name *> p <* newlineOrEOF

versionNumber :: Parser Char String
versionNumber = record "StartFontMetrics" $ many1 (digit <|> char '.')


keyword :: String -> CharParser ()
keyword ss = skipOne $ symbol ss


newlineOrEOF :: CharParser ()
newlineOrEOF = skipOne newline <|> eof


newline :: CharParser Char
newline = lexeme $ char '\n'


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
