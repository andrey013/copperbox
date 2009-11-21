{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Core.TextEncoding
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  experimental
-- Portability :  GHC only
--
-- Extended character handling...
-- 
--------------------------------------------------------------------------------

module Wumpus.Core.TextEncoding
  ( 
    GlyphName
  , PostScriptLookup
  , SVGLookup
  , TextEncoder(..)

  , EncodedText(..)    
  , TextChunk(..)

  , textLength

  , lexLabel

  ) where

import Text.PrettyPrint.Leijen hiding ( SText )

import Data.Char

type GlyphName = String

type PostScriptLookup = Int -> Maybe GlyphName
type SVGLookup        = GlyphName -> Maybe Int

data TextEncoder = TextEncoder  {
                       ps_lookup         :: PostScriptLookup,
                       svg_lookup        :: SVGLookup,
                       svg_encoding_name :: String
                     }


newtype EncodedText = EncodedText { getEncodedText :: [TextChunk] }
  deriving (Eq,Show)


data TextChunk = SText  String
               | EscInt Int
               | EscStr GlyphName
  deriving (Eq,Show)


--------------------------------------------------------------------------------

instance Pretty EncodedText where
  pretty = hcat . map pretty . getEncodedText

instance Pretty TextChunk where
  pretty (SText s)   = string s
  pretty (EscInt i)  = text "&#" <> int i  <> semi
  pretty (EscStr s)  = text "&#" <> text s <> semi

--------------------------------------------------------------------------------

textLength :: EncodedText -> Int
textLength = foldr add 0 . getEncodedText where 
    add (SText s) n = n + length s
    add _         n = n + 1


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
