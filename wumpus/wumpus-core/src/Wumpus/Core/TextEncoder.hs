{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Core.TextEncoder
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

module Wumpus.Core.TextEncoder
  ( 
    GlyphName
  , CharCode
  , PostScriptLookup
  , SVGLookup
  , TextEncoder(..)

  ) where



type GlyphName        = String
type CharCode         = Int 

type PostScriptLookup = CharCode -> Maybe GlyphName
type SVGLookup        = GlyphName -> Maybe CharCode


-- | A TextEncoder
--
-- An /instance/ needs: 
--
-- * The functions for looking up codes by glyph-name and 
-- glyph-name by code. 
-- 
-- * The name of the encoding - this is printed in the xml 
-- prologue of the SVG file as the @encoding@ attribute. Latin 
-- 1\'s official name is \"ISO-8859-1\". 
-- 
-- * Fallback glyph-names and char codes in case lookup fails.
-- 
-- "Wumpus.Core.TextLatin1" defines an implementation for Latin 1.
--
data TextEncoder = TextEncoder  {
                       ps_lookup         :: PostScriptLookup,
                       svg_lookup        :: SVGLookup,
                       svg_encoding_name :: String,
                       ps_fallback       :: GlyphName,
                       svg_fallback      :: CharCode
                     }
                     

-- no show instance as a TextEncoder contains functions.