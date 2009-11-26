{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Core.TextEncoder
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC with TypeFamilies and more
--
-- Extended character handling...
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


import Data.Char

type GlyphName = String
type CharCode  = Int 

type PostScriptLookup = CharCode -> Maybe GlyphName
type SVGLookup        = GlyphName -> Maybe CharCode

data TextEncoder = TextEncoder  {
                       ps_lookup         :: PostScriptLookup,
                       svg_lookup        :: SVGLookup,
                       svg_encoding_name :: String,
                       ps_fallback       :: GlyphName,
                       svg_fallback      :: CharCode
                     }
                     

