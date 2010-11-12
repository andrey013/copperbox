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

  , textLength  
  , lexLabel

  ) where

import Wumpus.Core.Utils.FormatCombinators

import Data.Char
import qualified Data.IntMap as IntMap

newtype EncodedText = EncodedText { getEncodedText :: [EncodedChar] }
  deriving (Eq,Show)


-- | For KernLabels Wumpus needs a Char version of TextChunk.
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


textLength :: EncodedText -> Int
textLength = length . getEncodedText where 


-- | 'lexLabel' input is regular text and escaped glyph names or
-- decimal character codes. Escaping follows the SVG convention,
-- start with @&#@ (ampersand hash) end with @;@ (semicolon).
--
-- Special chars are output to PostScript as:
--
-- > /egrave glyphshow
--
-- Special chars are output to SVG as an escaped decimal, e.g.:
--
-- > &#232;
--
-- Note, HTML entity names do not seem to be supported in SVG,
-- @ &egrave; @ does not work in FireFox or Chrome.
--
lexLabel :: String -> EncodedText
lexLabel = EncodedText . lexer

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

