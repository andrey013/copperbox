{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.ToyFontMetrics.ParserV2
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

module Graphics.ToyFontMetrics.ParserV2
  where


import Graphics.ToyFontMetrics.DatatypesV2
import Graphics.ToyFontMetrics.ParserCombinators
import qualified Graphics.ToyFontMetrics.TokenParser as P

import Control.Applicative
import Control.Applicative.Permutation


afmFile :: CharParser AfmFile
afmFile = AfmFile <$> globalInfo <*> metricProps

globalInfo :: CharParser GlobalInfo
globalInfo = runPerms $ GlobalInfo <$> 
        atom            versionNumber
    <*> atom            (record "FontName"            whiteString)
    <*> optAtom ""      (record "FullName"            whiteString)
    <*> optAtom ""      (record "FamilyName"          whiteString)
    <*> optAtom ""      (record "Weight"              whiteString)
    <*> optAtom 0       (record "ItalicAngle"         number)
    <*> optAtom False   (record "IsFixedPitch"        bool)
    <*> atom            fontBBox
    <*> optAtom 0       (record "UnderlinePosition"   $ lexeme number)
    <*> optAtom 0       (record "UnderlineThickness"  $ lexeme number)
    <*> optAtom ""      (record "Version"             whiteString)
    <*> optAtom ""      (record "Notice"              whiteString)
    <*> optAtom ""      (record "EncodingScheme"      whiteString)
    <*> optAtom 0       (record "CapHeight"           $ lexeme number)
    <*> optAtom 0       (record "XHeight"             $ lexeme number)
    <*> optAtom 0       (record "Ascender"            $ lexeme number)
    <*> optAtom 0       (record "Descender"           $ lexeme number) 
  where
    versionNumber   = record "StartFontMetrics" $ many1 (digit <|> char '.')
    fontBBox        = record "FontBBox" $ (,,,) 
                        <$> lexeme int <*> lexeme int <*> lexeme int 
                                       <*> lexeme int


characterMetrics :: CharParser CharacterMetrics
characterMetrics = runPerms $ CharacterMetrics <$>
        optAtom (-1) (metric "C"                     $ lexeme int)
    <*> atom         widthVector
    <*> atom         (metric "N"                     name)
    <*> atom         (metric "B"                     bbox)
    <*> optAtom []   (many $ metric "L" ((,) <$> name <*> name))
  where
    widthVector =  (metric "WX" ((\w -> (w,0)) <$> lexeme number))
               <|> (metric "W"  ((,) <$> lexeme number <*> lexeme number))

    bbox        = (,,,) <$> lexeme number <*> lexeme number <*> lexeme number
                                          <*> lexeme number

metricProps :: CharParser MetricProps
metricProps = pure MetricProps

record :: String -> CharParser a -> CharParser a
record name p = symbol name *> p <* newlineOrEOF

metric :: String -> CharParser a -> CharParser a
metric name p = symbol name *> p <* semi



metricsSets     :: CharParser Int
metricsSets     = record "MetricsSets" $ lexeme natural



keyword :: String -> CharParser ()
keyword ss = skipOne $ symbol ss


newlineOrEOF :: CharParser ()
newlineOrEOF = skipOne newline <|> eof

name :: CharParser String
name = lexeme $ many (noneOf ";\n")

newline :: CharParser Char
newline = lexeme $ char '\n'

semi :: CharParser Char
semi = lexeme $ char ';'

whiteString :: CharParser String
whiteString = many (noneOf ['\n'])

number :: CharParser Double
number = (\signf intpart fracpart -> signf $ intpart + fracpart)
          <$> psign <*> onatural <*> ofrac
  where
    psign    = option id (negate <$ char '-')
    onatural = option 0  (fromIntegral <$> natural)
    ofrac    = option 0  ((\xs -> read $ '.':xs) <$> (char '.' *> (many1 digit)))

int :: CharParser Int
int = ($) <$> psign <*> natural
  where
    psign    = option id (negate <$ char '-')


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
