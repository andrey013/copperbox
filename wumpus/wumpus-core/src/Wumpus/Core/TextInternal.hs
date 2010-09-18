{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Core.TextInternal
-- Copyright   :  (c) Stephen Tetley 2009-2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Extended character handling.
-- 
--------------------------------------------------------------------------------

module Wumpus.Core.TextInternal
  ( 

    EncodedText(..)    
  , TextChunk(..)
  , EncodedChar(..)

  , textLength
  , lookupByCharCode  
  , lookupByGlyphName
  , getSvgFallback
  , getPsFallback
  
  , lexLabel

  ) where

import Wumpus.Core.FormatCombinators
import Wumpus.Core.TextEncoder

import Data.Char
import qualified Data.Map as Map

newtype EncodedText = EncodedText { getEncodedText :: [TextChunk] }
  deriving (Eq,Show)

-- | Wumpus supports both escaped names e.g. @egrave@ and escaped
-- (numeric decimal) character codes in the input string for a 
-- TextLabel.
-- 
data TextChunk = TextSpan    String
               | TextEscInt  Int
               | TextEscName GlyphName
  deriving (Eq,Show)

-- | For KernLabels Wumpus needs a Char version of TextChunk.
--
data EncodedChar = CharLiteral Char
                 | CharEscInt  Int
                 | CharEscName GlyphName
  deriving (Eq,Show)


--------------------------------------------------------------------------------

instance Format EncodedText where
  format = hcat . map format . getEncodedText

instance Format TextChunk where
  format (TextSpan s)    = text s
  format (TextEscInt i)  = text "&#" <> int i  <> semicolon
  format (TextEscName s) = text "&#" <> text s <> semicolon

instance Format EncodedChar where
  format (CharLiteral c) = char c
  format (CharEscInt i)  = text "&#" <> int i  <> semicolon
  format (CharEscName s) = text "&#" <> text s <> semicolon


--------------------------------------------------------------------------------


textLength :: EncodedText -> Int
textLength = foldr add 0 . getEncodedText where 
    add (TextSpan s) n = n + length s
    add _            n = n + 1


lookupByCharCode :: FontEncoderName -> CharCode -> TextEncoder -> Maybe GlyphName
lookupByCharCode name i enc = 
    Map.lookup name (font_encoder_map enc) >>= \a -> (ps_lookup a) i

lookupByGlyphName :: FontEncoderName -> GlyphName -> TextEncoder -> Maybe CharCode
lookupByGlyphName name i enc = 
    Map.lookup name (font_encoder_map enc) >>= \a -> (svg_lookup a) i


getSvgFallback :: FontEncoderName -> TextEncoder -> CharCode
getSvgFallback name enc = case Map.lookup name (font_encoder_map enc) of
   Just fe -> svg_fallback fe 
   Nothing -> 0o040                     -- wild guess

getPsFallback :: FontEncoderName -> TextEncoder -> GlyphName
getPsFallback name enc = case Map.lookup name (font_encoder_map enc) of
   Just fe -> ps_fallback fe 
   Nothing -> "space"                    -- wild guess


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
lexer :: String -> [TextChunk]
lexer []            = []

lexer ('&':'#':xs)  = esc xs
  where
    esc (c:cs) | isDigit c = let (s,cs') = span isDigit cs 
                             in  intval (c:s) cs'
               | otherwise = let (s,cs') = span isAlpha cs 
                             in TextEscName (c:s) : optsemi cs'
    esc []                 = []

    optsemi (';':cs)   = lexer cs      -- let ill-formed go through
    optsemi cs         = lexer cs

    intval [] rest  = optsemi rest
    intval cs rest  = TextEscInt (read cs) : optsemi rest

lexer (x:xs)        = let (s,xs') = span (/= '&') xs 
                      in TextSpan (x:s) : lexer xs'
