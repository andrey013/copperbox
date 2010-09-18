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

  , getDefaultFontEncoder
  , getFontEncoder

  , textLength
  , lookupByCharCode  
  , lookupByGlyphName
  
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


-- | Get the default font encoder in a TextEncoder.
--
-- This function throws a runtime error on failure.
-- 
getDefaultFontEncoder :: TextEncoder -> FontEncoder
getDefaultFontEncoder enc = 
    case Map.lookup (default_encoder_name enc) (font_encoder_map enc) of
       Nothing -> error "TextEncoder error - cannot find the default font."
       Just a  -> a


-- | Lookup a font encoder in a TextEncoder.
--
-- This function throws a runtime error on failure.
-- 
getFontEncoder :: FontEncoderName -> TextEncoder -> FontEncoder
getFontEncoder name enc = case Map.lookup name (font_encoder_map enc) of
    Nothing -> error err_msg
    Just a  -> a
  where
    err_msg = "TextEncoder error - cannot find the encoder: " 
              ++ show name ++ "."

textLength :: EncodedText -> Int
textLength = foldr add 0 . getEncodedText where 
    add (TextSpan s) n = n + length s
    add _            n = n + 1


lookupByCharCode :: CharCode -> FontEncoder -> Maybe GlyphName
lookupByCharCode i enc = (ps_lookup enc) i

lookupByGlyphName :: GlyphName -> FontEncoder -> Maybe CharCode
lookupByGlyphName i enc = (svg_lookup enc) i


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
