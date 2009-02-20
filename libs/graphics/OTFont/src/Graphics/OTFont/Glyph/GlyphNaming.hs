{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.OTFont.Glyph.GlyphNaming
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- ..
--
--------------------------------------------------------------------------------

module Graphics.OTFont.Glyph.GlyphNaming where

import Graphics.OTFont.Datatypes
import Graphics.OTFont.Glyph.GlyphNames258
import Graphics.OTFont.Table.Post

import Data.Array.Unboxed ( bounds, (!) )
import qualified Data.IntMap as IntMap
import Data.List ( foldl' )

type GlyphNameMap = IntMap.IntMap String

buildGlyphNameMap :: PostTable -> GlyphNameMap
buildGlyphNameMap = maybe buildNameMap_v1 sk . subtableData
  where 
    sk (_,j,k) = buildNameMap_v2 j k

subtableData :: PostTable -> Maybe (UShort, USequence UShort, StringSequence)
subtableData = fn . post_subtable where
    fn (Version2_0 a b c)   = Just (a,b,c)
    fn _                    = Nothing
     

buildNameMap_v1 :: GlyphNameMap
buildNameMap_v1 = glyph_mac_names_258

buildNameMap_v2 :: USequence UShort -> StringSequence -> GlyphNameMap
buildNameMap_v2 arr ss = foldl' fn IntMap.empty ixs 
  where
    ixs       = let (a,b) = bounds arr in [a..b]
    mac_names = glyph_mac_names_258 
    at        = IntMap.findWithDefault ""
    
    fn :: GlyphNameMap -> Int -> GlyphNameMap
    fn m i  = let ix = fromIntegral $ arr!i in 
              case ix <= 257 of 
                  True  -> IntMap.insert i (at ix mac_names) m
                  False -> IntMap.insert i (at (ix - 258) ss) m 
    
    
    
    
    