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

import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language

versionNumber :: Parser String
versionNumber = symbol "StartFontMetrics" >> many1 (digit <|> char '.')



-------------------------------------------------------------------------------
-- Tokens
-------------------------------------------------------------------------------

lexer             :: P.TokenParser st
lexer             = P.makeTokenParser lexerDef

lexerDef = emptyDef
  { commentLine  = "Comment"                                 
  }

whiteSpace        = P.whiteSpace lexer 
reserved          = P.reserved lexer
parens            = P.parens lexer

symbol            :: String -> CharParser st String
symbol            = P.symbol lexer