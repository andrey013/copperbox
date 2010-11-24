{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.FontLoader.AfmV2
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- AFM file parser for Version 2.0.
--
-- Note - AFM Version 2.0 used by GhostScript and Version 3.0+
-- have numerous differences. 
-- 
--------------------------------------------------------------------------------

module Wumpus.Basic.FontLoader.AfmV2Parser
  ( 

     parseAfmV2File
  
  ) where

import Wumpus.Basic.FontLoader.Base
import Wumpus.Basic.Utils.ParserCombinators
import qualified Wumpus.Basic.Utils.TokenParsers as P

import Wumpus.Core                              -- package: wumpus-core

import Control.Applicative

import Data.Char
import qualified Data.Map               as Map



--------------------------------------------------------------------------------
-- parser

parseAfmV2File :: FilePath -> IO (Either ParseError AfmFile)
parseAfmV2File filepath = runParserEither afmFile <$> readFile filepath


afmFile :: CharParser AfmFile
afmFile = 
    (\info xs -> AfmFile (encodingScheme info) 
                         (fontBBox info) 
                         (capHeight info)
                         xs ) 
      <$> (versionNumber    *> globalInfo) 
      <*> (startCharMetrics *> many charMetrics)



globalInfo :: CharParser GlobalInfo
globalInfo = (foldr (\(k,v) a -> Map.insert k v a) Map.empty) 
               <$> manyTill (record <* lexeme newline) (peek startCharMetrics)



textQuery :: String -> GlobalInfo -> Maybe String
textQuery = Map.lookup
 
runQuery :: String -> CharParser a -> GlobalInfo -> Maybe a
runQuery field_name p table = 
    Map.lookup field_name table >>= extr . runParser p
  where
    extr (Okay a _) = Just a
    extr _          = Nothing



-- | Strictly speaking a fontBBox is measured in integer units.
--
fontBBox            :: GlobalInfo -> Maybe AfmBoundingBox
fontBBox            = runQuery "FontBBox" charBBox


encodingScheme      :: GlobalInfo -> Maybe String
encodingScheme      = textQuery "EncodingScheme"


capHeight           :: GlobalInfo -> Maybe AfmUnit
capHeight           = runQuery "CapHeight" number


charMetrics :: CharParser AfmGlyphMetrics
charMetrics = AfmGlyphMetrics <$>
        metric "C" (-1) cint
    <*> widthVector
    <*> metric "N" "" name1
    <*  charBBox
    <*  many (symbol "L" *> ligature_body <* semi)
    <*  newlineOrEOF
  where
    ligature_body = ((,) <$> name <*> name)
    
widthVector :: CharParser (Vec2 AfmUnit)
widthVector =  (symbol "WX" *> ((\w -> vec w 0) <$> number) <* semi)
           <|> (symbol "W"  *> (vec <$> number <*> number)  <* semi)

charBBox :: CharParser AfmBoundingBox
charBBox = symbol "B" *> go <* semi
  where
    go = (\llx lly urx ury -> boundingBox (P2 llx lly) (P2 urx ury))
           <$> number <*> number <*> number <*> number

metric :: String -> a -> CharParser a -> CharParser a
metric iden dfault p = option dfault go
  where
    go = symbol iden *> p <* semi



record :: CharParser (AfmKey,String)
record = (,) <$> keyName <*> whiteString -- <* newlineOrEOF

versionNumber :: CharParser String
versionNumber = 
    symbol "StartFontMetrics" *> many1 (digit <|> char '.') <* newlineOrEOF


startCharMetrics :: CharParser Int
startCharMetrics = 
    symbol "StartCharMetrics" *> int <* newlineOrEOF



keyName :: CharParser AfmKey
keyName = lexeme (many1 $ satisfy isAlphaNum) 




newlineOrEOF :: CharParser ()
newlineOrEOF = skipOne (lexeme newline) <|> eof

name :: CharParser String
name = lexeme $ many (noneOf ";\n")

name1 :: CharParser String
name1 = lexeme $ many (noneOf "; \t\n")



semi :: CharParser Char
semi = lexeme $ char ';'

whiteString :: CharParser String
whiteString = many1 (noneOf ['\n'])

number :: CharParser AfmUnit
number = liftA realToFrac double




cint :: CharParser Int
cint = hexInt <|> octInt <|> int


hexInt :: CharParser Int
hexInt = lexeme $ between (char '<') (char '>') P.hexBase


octInt :: CharParser Int
octInt = lexeme $ char '\\' *> P.octBase



--------------------------------------------------------------------------------

-- no newline in whitespace


lp :: P.LexemeParser
lp = P.commentLineLexemeParser "Comment" [' ', '\t']


lexeme          :: CharParser a -> CharParser a
lexeme          = P.lexeme lp

symbol          :: String -> CharParser String
symbol          = lexeme . string

-- whiteSpace      :: CharParser ()
-- whiteSpace      = P.whiteSpace lp


integer         :: CharParser Integer
integer         = lexeme P.integer

int             :: CharParser Int
int             = fromIntegral <$> integer

double          :: CharParser Double
double          = lexeme P.double
