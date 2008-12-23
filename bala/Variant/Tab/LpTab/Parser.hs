{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  LpTab.Parser
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Parser
--
--------------------------------------------------------------------------------



module LpTab.Parser where

import LpTab.Datatypes

import Control.Applicative hiding (many, optional, (<|>), empty )
import Control.Monad
import Data.Either (lefts) 
import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language (emptyDef)

-- | An Applicative instance for Parsec. 
instance Applicative (GenParser tok st) where
  pure = return
  (<*>) = ap

tabParse :: FilePath -> IO (Either ParseError Ans)
tabParse path = return fn `ap` readFile path where
    fn text = runParser tabparser () path (preprocess text)

preprocess :: String -> String
preprocess = unlines . map (++ line_end) . lines  

line_end :: String
line_end = "/@"

type Ans = [TabSystem]

tabparser :: Parser Ans
tabparser = lefts <$> many (eitherParse (try tabsystem) toLineEnd)

tabsystem :: Parser TabSystem
tabsystem = TabSystem <$> many1 tabline


tabline :: Parser TabLine
tabline = whiteSpace *> many1 tabLexeme <* toLineEnd
 
      
tabLexeme :: Parser TabLexeme
tabLexeme = choice [barmark, hiaton, fretnum]
  where
    barmark   = BarMarkT    <$  char '|'
    hiaton    = HiatonT     <$  char '-'
    fretnum   = FretNumberT <$> positiveInt




--------------------------------------------------------------------------------
-- Lexical analysis

baseLex           :: P.TokenParser st
baseLex           = P.makeTokenParser emptyDef

whiteSpace        :: CharParser st ()
whiteSpace        = P.whiteSpace baseLex



nonzeroInt :: CharParser st Int
nonzeroInt = (\x xs -> read $ x:xs) <$> oneOf "123456789" <*> many digit

 
-- Parsec's integer/int parser would consume dash '-' as a negative sign   
positiveInt :: CharParser st Int
positiveInt = read <$> many1 digit

toLineEnd :: CharParser st ()
toLineEnd = whiteSpace <* lineEnd 
    
lineEnd :: CharParser st Char
lineEnd = string line_end *> newline


--------------------------------------------------------------------------------
-- Utility parsers

eitherParse :: CharParser st a -> CharParser st b -> CharParser st (Either a b)
eitherParse a b = choice [ Left <$> a, Right <$> b]

 

