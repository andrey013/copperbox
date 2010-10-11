{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Core.Text.Encoder
-- Copyright   :  (c) Stephen Tetley 2009-2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Extended character code handling.
-- 
-- Wumpus uses SVG style escaping to embed character codes or 
-- names in regular strings:
--
-- > "regular ascii text &#egrave; more ascii text"
--  
-- i.e. character names and codes are delimited by @&\#@ on the 
-- left and @;@ on the right.
--
-- In Wumpus both character names and character codes can
-- be embedded in strings - (e.g. @ &\#egrave; or &\#232; @).
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
-- To accommodate both, Wumpus defines a TextEncoder record which
-- provides a two-way mapping between character codes and glyph 
-- names for a character set.
--
--------------------------------------------------------------------------------

module Wumpus.Core.Text.Encoder
  ( 
    GlyphName
  , CharCode
  , PostScriptLookup
  , SVGLookup

  , FontEncoderName(..)
  , TextEncoder(..)
  , FontEncoder(..)

  ) where

import Data.Map
import Data.Word

type GlyphName        = String
type CharCode         = Int 

type PostScriptLookup = CharCode -> Maybe GlyphName
type SVGLookup        = GlyphName -> Maybe CharCode



-- | Font encoder name - a newtype wrapped /number/.
-- 
-- Ideally this would be an enumerated type, but it has to be 
-- open - new encoders need to be added, so an enum is out of the
-- question.
--
-- A String would be good, but would have slow lookup when used 
-- as a key. Dealing with multiple encoders was added late to 
-- Wumpus-Core - it is necessary, but taking a performace hit 
-- because of it is chagrin. So instead /uniquely/ asssigned
-- numbers are used.
--
-- Numbers below 10000 are reserved for Wumpus, though it is 
-- unlikely to need more than a handful. Numbers above are free 
-- to use (clearly clashes are possible, but probably unlikely).
-- 
-- Wumpus-Core assigns the following, other Wumpus libraries may 
-- assign more:
--
-- > 0 - Latin1 (for Helvetica, Times Roman, Courier...)
--
-- > 1 - Symbol Font
-- 
newtype FontEncoderName = FontEncoderName { getFontEncoderName :: Word16 }
  deriving (Eq,Ord)

instance Show FontEncoderName where
  show = step . getFontEncoderName
    where
      step 0 = "Latin1"
      step 1 = "Symbol-Font"
      step n = show n

-- | 'TextEncoder'
--
-- An /instance/ needs: 
--
-- * A map of FontEncoderNames to FontEncoders.
--
-- * The name of the encoding - this is printed in the xml 
-- prologue of the SVG file as the @encoding@ attribute. Latin 
-- 1\'s official name is seemingly \"ISO-8859-1\". 
-- 
-- * The name of the default encoder - this should naturally be 
-- in the Font Encoder map.
-- 
-- 
data TextEncoder = TextEncoder
      { svg_encoding_name       :: String
      , font_encoder_map        :: Map FontEncoderName FontEncoder
      }

-- | 'FontEncoder'.
--
-- * The functions for looking up codes by glyph-name and 
-- glyph-name by code. 
-- 
-- * Fallback glyph-names and char codes in case lookup fails.
-- 
-- "Wumpus.Core.TextLatin1" defines an implementation for Latin 1.
--
data FontEncoder = FontEncoder  
      { ps_lookup           :: PostScriptLookup
      , svg_lookup          :: SVGLookup
      , ps_fallback         :: GlyphName
      , svg_fallback        :: CharCode
      }
                     

-- no show instance as a TextEncoder contains functions.


