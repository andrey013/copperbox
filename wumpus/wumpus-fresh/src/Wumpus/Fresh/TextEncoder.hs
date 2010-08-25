{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Fresh.TextEncoder
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Fresh text encoder
--
--------------------------------------------------------------------------------

module Wumpus.Fresh.TextEncoder
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