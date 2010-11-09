{-# LANGUAGE RankNTypes                 #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Utils.TokenParsers
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Generally you should expect to import this module qualified 
-- and define versions to consume trailing white-space.
--
-- > integer  :: CharParser Int
-- > integer  = P.lexeme P.integer
-- 
--------------------------------------------------------------------------------

module Wumpus.Basic.Utils.TokenParsers
  (

    LexemeParser
  , spaceLexemeParser
  , spaceCharLexemeParser
  , commentLexemeParser
  , commentLineLexemeParser
  , commentMultiLexemeParser

  , lexeme
  , whiteSpace

  , octalBase
  , haskOctal
  
  , natural
  , double

  ) where

import Wumpus.Basic.Utils.ParserCombinators

import Control.Applicative

newtype LexemeParser = LexemeParser { getLexemeParser :: CharParser () }

spaceLexemeParser :: LexemeParser
spaceLexemeParser = LexemeParser go 
  where
    go = () <$ many space

spaceCharLexemeParser :: [Char] -> LexemeParser
spaceCharLexemeParser cs = LexemeParser go 
  where
    go = skipMany (oneOf cs)

commentLexemeParser :: String -> (String,String) -> [Char] -> LexemeParser
commentLexemeParser line (start,end) cs = LexemeParser go
  where
    go        = skipMany (whiteChar <|> lineComment line
                                    <|> spanComment start end)
    whiteChar = skipOne (oneOf cs)


commentLineLexemeParser :: String -> [Char] -> LexemeParser
commentLineLexemeParser start cs = LexemeParser go
  where
    go        = skipMany (whiteChar <|> lineComment start)
    whiteChar = skipOne (oneOf cs)

commentMultiLexemeParser :: String -> String -> [Char] -> LexemeParser
commentMultiLexemeParser start end cs = LexemeParser go
  where
    go        = skipMany (whiteChar <|> spanComment start end)
    whiteChar = skipOne (oneOf cs)

lineComment :: String -> CharParser ()
lineComment start  = 
    skipOne (string start *> manyTill anyChar endLine)

spanComment :: String -> String -> CharParser ()
spanComment start end = 
    string start *> manyTill anyChar (string end) *> return ()


endLine :: CharParser ()
endLine = skipOne (char '\n') <|> eof


lexeme :: LexemeParser -> CharParser a -> CharParser a
lexeme trail p = p <* getLexemeParser trail

whiteSpace :: LexemeParser -> CharParser ()
whiteSpace = getLexemeParser

--------------------------------------------------------------------------------




octalBase :: CharParser Int
octalBase = (\cs -> read $ '0':'o':cs) <$> many1 octDigit

haskOctal :: CharParser Int
haskOctal = (string "0o" <|> string "0O") *> octalBase


-- integer :: CharParser Int
-- integer  

natural :: CharParser Int
natural = liftA read (many1 digit)

double :: CharParser Double
double = (\signf intpart fracpart -> signf $ intpart + fracpart)
                  <$> psign <*> onatural <*> ofrac 
  where
    psign     = option id (negate <$ char '-')
    onatural  = option 0  (fromIntegral <$> natural)
    ofrac     = option 0  ((\xs -> read $ '.':xs) <$> (char '.' *> (many1 digit)))
