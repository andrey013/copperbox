{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Fresh.TextInternal
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Fresh.
-- 
--------------------------------------------------------------------------------

module Wumpus.Fresh.TextInternal
  ( 

    EncodedText(..)    
  , TextChunk(..)

  , textLength
  , lookupByCharCode  
  , lookupByGlyphName

  , lexLabel

  ) where

import Wumpus.Fresh.FormatCombinators
import Wumpus.Fresh.TextEncoder

import Data.Char

newtype EncodedText = EncodedText { getEncodedText :: [TextChunk] }
  deriving (Eq,Show)


data TextChunk = SText  String
               | EscInt Int
               | EscStr GlyphName
  deriving (Eq,Show)


--------------------------------------------------------------------------------

instance Format EncodedText where
  format = hcat . map format . getEncodedText

instance Format TextChunk where
  format (SText s)   = text s
  format (EscInt i)  = text "&#" <> int i  <> semicolon
  format (EscStr s)  = text "&#" <> text s <> semicolon

--------------------------------------------------------------------------------

textLength :: EncodedText -> Int
textLength = foldr add 0 . getEncodedText where 
    add (SText s) n = n + length s
    add _         n = n + 1


lookupByCharCode :: CharCode -> TextEncoder -> Maybe GlyphName
lookupByCharCode i enc = (ps_lookup enc) i

lookupByGlyphName :: GlyphName -> TextEncoder -> Maybe CharCode
lookupByGlyphName i enc = (svg_lookup enc) i


-- | Output to PostScript as @ /egrave glyphshow @

-- Output to SVG as an escaped decimal, e.g. @ &#232; @
--
-- Note, HTML entity names do not seem to be supported in SVG,
-- @ &egrave; @ does not work in FireFox or Chrome.


lexLabel :: String -> EncodedText
lexLabel = EncodedText . lexer

lexer :: String -> [TextChunk]
lexer []            = []

lexer ('&':'#':xs)  = esc xs
  where
    esc (c:cs) | isDigit c = let (s,cs') = span isDigit cs 
                             in  intval (c:s) cs'
               | otherwise = let (s,cs') = span isAlpha cs 
                             in EscStr (c:s) : optsemi cs'
    esc []                 = []

    optsemi (';':cs)   = lexer cs      -- let ill-formed go through
    optsemi cs         = lexer cs

    intval [] rest  = optsemi rest
    intval cs rest  = EscInt (read cs) : optsemi rest

lexer (x:xs)        = let (s,xs') = span (/= '&') xs 
                      in SText (x:s) : lexer xs'
