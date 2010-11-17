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
-- Extended character handling.
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



type EncodingVector = IntMap.IntMap String


--------------------------------------------------------------------------------

instance Format EncodedText where
  format = hcat . map format . getEncodedText


instance Format EncodedChar where
  format (CharLiteral c) = char c
  format (CharEscInt i)  = text "&#" <> int i  <> semicolon
  format (CharEscName s) = text "&#" <> text s <> semicolon


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



-- Note - the lexer reads number spans with isDigit, so reads 
-- decimals only.
-- 
lexer :: String -> [EncodedChar]
lexer []            = []
lexer ('&':'#':cs)  = escStart cs
lexer (c:cs)        = CharLiteral c : lexer cs

escStart :: String -> [EncodedChar]
escStart ('0':'o':cs)           = escOct cs
escStart ('0':'O':cs)           = escOct cs
escStart ('0':'x':cs)           = escHex cs
escStart ('0':'X':cs)           = escHex cs
escStart (c:cs) | isDigit c     = escDec (digitToInt c) cs
escStart (c:cs)                 = let (ss,rest) = span isAlphaNum cs 
                                  in CharEscName (c:ss) : chompToSemi rest
escStart []                     = [] 

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

