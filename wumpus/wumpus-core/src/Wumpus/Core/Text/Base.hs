{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Core.Text.Base
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Extended character code handling.
-- 
-- Wumpus uses an escaping style derived from SVG to embed 
-- character codes and PostScript glyph names in regular strings. 
--
-- > "regular ascii text &#38; more ascii text"
--  
-- i.e. character codes are delimited by @&\#@ on the 
-- left and @;@ on the right.
--
-- Glyph names are delimited by @&@ on the 
-- left and @;@ on the right.
--
-- > "regular ascii text &ampersand; more ascii text"
--
-- Note that glyph names \*\* should always \*\* correspond to
-- PostScript glyph names not SVG / HTML glyph names.
--
-- In Wumpus both character names and character codes can
-- be embedded in strings - (e.g. @ &\egrave; or &\#232; @).
--
-- Character codes can be also be expressed as octal or 
-- hexadecimal:
--
-- > myst&#0o350;re
--
-- > myst&#0xE8;re
--
-- In the generated PostScript, Wumpus uses the character name, 
-- e.g.:  
--
-- > (myst) show /egrave glyphshow (re) show
-- 
-- The generated SVG uses the numeric code, e.g.: 
--
-- > myst&#232;re
--
--------------------------------------------------------------------------------


module Wumpus.Core.Text.Base
  ( 

    EncodedText(..)    
  , EncodedChar(..)
  , EncodingVector

  , encodeString
  , textLength  

  ) where

import Wumpus.Core.Utils.FormatCombinators

import Data.Char
import qualified Data.IntMap as IntMap


-- | Internal string representation for Wumpus-Core.
-- 
-- 'EncodedText' is a list of characters, where each character 
-- may be either a regular character, an integer representing a 
-- Unicode code-point or a PostScript glyph name.
-- 
newtype EncodedText = EncodedText { getEncodedText :: [EncodedChar] }
  deriving (Eq,Show)


-- | Internal character representation for Wumpus-Core.
-- 
-- An 'EncodedChar' may be either a regular character, an integer
-- representing a Unicode code-point or a PostScript glyph
-- name.
-- 
data EncodedChar = CharLiteral Char
                 | CharEscInt  Int
                 | CharEscName String
  deriving (Eq,Show)


-- | 'EncodingVecor' - a map from code point to PostScript glyph
-- name.
-- 
type EncodingVector = IntMap.IntMap String


--------------------------------------------------------------------------------

instance Format EncodedText where
  format = hcat . map format . getEncodedText


instance Format EncodedChar where
  format (CharLiteral c) = char c
  format (CharEscInt i)  = text "&#" <> int i  <> semicolon
  format (CharEscName s) = text "&" <> text s <> semicolon


--------------------------------------------------------------------------------




-- | 'encodeString' input is regular text and escaped glyph names 
-- or decimal character codes. Escaping in the input string should 
-- follow the SVG convention - the escape sequence starts with 
-- @&#@ (ampersand hash) and end with @;@ (semicolon).
--
-- Escaped characters are output to PostScript as their respective
-- glyph names:
--
-- > /egrave glyphshow
--
-- Escaped chararacters are output to SVG as an escaped decimal, 
-- e.g.:
--
-- > &#232;
--
encodeString :: String -> EncodedText
encodeString = EncodedText . lexer


-- | Get the character count of an 'EncodedText' string.
--
textLength :: EncodedText -> Int
textLength = length . getEncodedText where 


 
lexer :: String -> [EncodedChar]
lexer []            = []
lexer ('&':'#':cs)  = escNumStart cs
lexer ('&':cs)      = escName cs
lexer (c:cs)        = CharLiteral c : lexer cs

-- Input is malformed if this reaches the @rest@ case.
-- 
escNumStart :: String -> [EncodedChar]
escNumStart ('0':'o':cs)           = escOct cs
escNumStart ('0':'O':cs)           = escOct cs
escNumStart ('0':'x':cs)           = escHex cs
escNumStart ('0':'X':cs)           = escHex cs
escNumStart (c:cs) | isDigit c     = escDec (digitToInt c) cs
escNumStart rest                   = chompToSemi rest      

escName :: String -> [EncodedChar]
escName (c:cs)                     = let (ss,rest) = span isAlphaNum cs 
                                     in specialEscape (c:ss) : chompToSemi rest
escName []                         = [] 


-- | One digit consumed already...
--
escDec :: Int -> String -> [EncodedChar]
escDec n (c:cs) | isDigit c = escDec (n*10 + digitToInt c) cs
escDec n cs     | n > 0     = CharEscInt n : chompToSemi cs
                | otherwise = chompToSemi cs

escHex :: String -> [EncodedChar]
escHex = step 0
  where
    step n (c:cs) | isHexDigit c = step (n*16 + digitToInt c) cs
    step n cs     | n > 0        = CharEscInt n : chompToSemi cs
                  | otherwise    = chompToSemi cs 


escOct :: String -> [EncodedChar]
escOct = step 0
  where
    step n (c:cs) | isHexDigit c = step (n*8 + digitToInt c) cs
    step n cs     | n > 0        = CharEscInt n : chompToSemi cs
                  | otherwise    = chompToSemi cs 



-- The last two conditions both indicate ill-formed input, but it
-- is /best/ if the lexer does not throw errors.
-- 
chompToSemi :: String -> [EncodedChar]
chompToSemi (';':cs) = lexer cs
chompToSemi (_:cs)   = chompToSemi cs           
chompToSemi []       = []


-- | Special processing for @amp@ because it is so common.
--
-- Are there other cases to add to this function?
--
specialEscape :: String -> EncodedChar
specialEscape ['a','m','p'] = CharEscName "ampersand"
specialEscape cs            = CharEscName cs
