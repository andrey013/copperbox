{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.FontKit.AfmV2Parser
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- AFM file parser.
--
-- Note - AFM Version 2.0 used by GhostScript and Version 3.0+
-- have numerous differences. 
-- 
--------------------------------------------------------------------------------

module Wumpus.FontKit.AfmV2Parser
  where


import Wumpus.FontKit.AfmV2Datatypes
import Wumpus.Basic.Utils.ParserCombinators
import qualified Wumpus.Basic.Utils.TokenParsers as P

import Wumpus.Core                              -- package: wumpus-core
import Wumpus.Basic.Text.Datatypes              -- package: wumpus-basic

import Control.Applicative

import Data.Char
import qualified Data.Map as Map

afmFile :: CharParser AfmFile
afmFile = AfmFile <$> 
    versionNumber <*> globalInfo <*> startCharMetrics <*> many characterMetrics

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



fontName            :: GlobalInfo -> Maybe String
fontName            = textQuery "FontName" 

fullName            :: GlobalInfo -> Maybe String
fullName            = textQuery "FullName"

familyName          :: GlobalInfo -> Maybe String
familyName          = textQuery "FamilyName"

weight              :: GlobalInfo -> Maybe String
weight              = textQuery "Weight"

italicAngle         :: GlobalInfo -> Maybe Radian
italicAngle         = runQuery "ItalicAngle" degree

isFixedPitch        :: GlobalInfo -> Maybe Bool
isFixedPitch        = runQuery "IsFixedPitch" bool

-- | Strictly speaking a fontBBox is measured in integer units.
--
fontBBox            :: GlobalInfo -> Maybe CharBBox
fontBBox            = runQuery "FontBBox" charBBox

underlinePosition   :: GlobalInfo -> Maybe AfmUnit
underlinePosition   = runQuery "UnderlinePosition" number

underlineThickness  :: GlobalInfo -> Maybe AfmUnit
underlineThickness  = runQuery "UnderlineThickness" number

version             :: GlobalInfo -> Maybe String
version             = textQuery "Version"

notice              :: GlobalInfo -> Maybe String
notice              = textQuery "Notice"

encodingScheme      :: GlobalInfo -> Maybe String
encodingScheme      = textQuery "EncodingScheme"

capHeight           :: GlobalInfo -> Maybe AfmUnit
capHeight           = runQuery "CapHeight" number

xHeight             :: GlobalInfo -> Maybe AfmUnit
xHeight             = runQuery "XHeight" number

ascender            :: GlobalInfo -> Maybe AfmUnit
ascender            = runQuery "Ascender" number

descender           :: GlobalInfo -> Maybe AfmUnit
descender           = runQuery "Descender" number



characterMetrics :: CharParser CharacterMetrics
characterMetrics = CharacterMetrics <$>
        metric "C" (-1) cint
    <*> widthVector
    <*> metric "N" "" name1
    <*> charBBox
    <*  many (symbol "L" *> ligature_body <* semi)
    <*  newlineOrEOF
  where
    ligature_body = ((,) <$> name <*> name)
    
widthVector :: CharParser WidthVector
widthVector =  (symbol "WX" *> ((\w -> vec w 0) <$> number) <* semi)
           <|> (symbol "W"  *> (vec <$> number <*> number)  <* semi)

charBBox :: CharParser CharBBox
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



keyword :: String -> CharParser ()
keyword ss = skipOne $ symbol ss


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

degree :: CharParser Radian
degree = liftA d2r double

number :: CharParser AfmUnit
number = liftA realToFrac double


cint :: CharParser Int
cint = hexInt <|> octInt <|> int


hexInt :: CharParser Int
hexInt = lexeme $ between (char '<') (char '>') P.hexBase

octInt :: CharParser Int
octInt = lexeme $ char '\\' *> P.octBase


bool :: CharParser Bool
bool =  False <$ symbol "false"
    <|> True  <$ symbol "true"

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
