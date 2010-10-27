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
import Wumpus.FontKit.Utils.ParserCombinators
import qualified Wumpus.FontKit.Utils.TokenParser as P

import Wumpus.Core                              -- package: wumpus-core

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

italicAngle         :: GlobalInfo -> Maybe Double
italicAngle         = runQuery "ItalicAngle" number

isFixedPitch        :: GlobalInfo -> Maybe Bool
isFixedPitch        = runQuery "IsFixedPitch" bool

fontBBox            :: GlobalInfo -> Maybe FontBBox
fontBBox            = runQuery "FontBBox" go
  where
    go = (\llx lly urx ury -> bbox (P2 llx lly) (P2 urx ury)) 
           <$> int <*> int <*> int <*> int

underlinePosition   :: GlobalInfo -> Maybe Double
underlinePosition   = runQuery "UnderlinePosition" number

underlineThickness  :: GlobalInfo -> Maybe Double
underlineThickness  = runQuery "UnderlineThickness" number

version             :: GlobalInfo -> Maybe String
version             = textQuery "Version"

notice              :: GlobalInfo -> Maybe String
notice              = textQuery "Notice"

encodingScheme      :: GlobalInfo -> Maybe String
encodingScheme      = textQuery "EncodingScheme"

capHeight           :: GlobalInfo -> Maybe Double
capHeight           = runQuery "CapHeight" number

xHeight             :: GlobalInfo -> Maybe Double
xHeight             = runQuery "XHeight" number

ascender            :: GlobalInfo -> Maybe Double
ascender            = runQuery "Ascender" number

descender           :: GlobalInfo -> Maybe Double
descender           = runQuery "Descender" number



characterMetrics :: CharParser CharacterMetrics
characterMetrics = CharacterMetrics <$>
        metric "C" (-1) int
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
    go = (\llx lly urx ury -> bbox (P2 llx lly) (P2 urx ury))
           <$> number <*> number <*> number <*> number

metric :: String -> a -> CharParser a -> CharParser a
metric name dfault p = option dfault go
  where
    go = symbol name *> p <* semi


{-

characterMetrics :: CharParser CharacterMetrics
characterMetrics = runPerms $ CharacterMetrics <$>
        optAtom (-1) (metric "C"  int)
    <*> atom         widthVector
    <*> atom         (metric "N"                     name1)
    <*> atom         (metric "B"                     bbox)
    <*> optAtom []   (many $ metric "L" ((,) <$> name <*> name))
  where
    widthVector =  (metric "WX" ((\w -> (w,0)) <$> lexeme number))
               <|> (metric "W"  ((,) <$> lexeme number <*> lexeme number))

    bbox        = (,,,) <$> lexeme number <*> lexeme number <*> lexeme number
                                          <*> lexeme number

-}

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

number :: CharParser Double
number = lexeme go 
  where
    go        = (\signf intpart fracpart -> signf $ intpart + fracpart)
                  <$> psign <*> onatural <*> ofrac
    psign     = option id (negate <$ char '-')
    onatural  = option 0  (fromIntegral <$> natural)
    ofrac     = option 0  ((\xs -> read $ '.':xs) <$> (char '.' *> (many1 digit)))

int :: CharParser Int
int = lexeme go
  where
    go    = ($) <$> psign <*> natural
    psign = option id (negate <$ char '-')


bool :: CharParser Bool
bool =  False <$ symbol "false"
    <|> True  <$ symbol "true"

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
