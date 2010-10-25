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

import Control.Applicative

-- Type error mixing a parser and a lexer / char parser...

versionNumber :: Parser Char String
versionNumber = keyword "StartFontMetrics" >> many1 (digit <|> char '.')




keyword :: String -> Parser Char ()
keyword ss = lexeme $ mapM symbol ss >> return ()


