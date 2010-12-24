{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.System.FontLoader.GhostScript
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Font loader / import shim for GhostScript glyph metrics.
--
-- Use this loader if you have GhostScript installed and you want 
-- to use the (AFM v2.0) metrics that are distributed with 
-- GhostScript.
-- 
--------------------------------------------------------------------------------

module Wumpus.Basic.System.FontLoader.GhostScript
  ( 

    module Wumpus.Basic.System.FontLoader.Internal.Base
  , module Wumpus.Basic.System.FontLoader.Internal.AfmV2Parser  

  , loadGSMetrics
  , gsFontLoader
  

  
  ) where

import Wumpus.Basic.Kernel
import Wumpus.Basic.System.FontLoader.Internal.AfmV2Parser
import Wumpus.Basic.System.FontLoader.Internal.Base
import Wumpus.Basic.System.FontLoader.Internal.GSFontMap

import Wumpus.Core                              -- package: wumpus-core

import Data.Maybe




newtype GSFontLoader = GSLoader { getGSFontLoader :: FontLoaderAlg AfmUnit }



-- loadFont :: GSLoader -> FontName -> IO (Either FontLoadErr FontCalcs)


loadGSMetrics :: FilePath -> [FontName] -> IO GlyphMetrics
loadGSMetrics font_dir_path ns = 
    loadGlyphMetrics (getGSFontLoader $ gsFontLoader font_dir_path) ns
    

gsFontLoader :: FilePath -> GSFontLoader
gsFontLoader font_dir_path = 
    GSLoader $ FontLoaderAlg
      { unit_scale_fun      = afmUnitScale
      , path_to_font_dir    = font_dir_path
      , file_name_locator   = buildName
      , font_parser         = parseAfmV2File
      , post_process        = buildFontProps bbox (V2 600 0) 1000
      }
  where
    buildName :: FontName -> FilePath
    buildName font = fromMaybe font $ gsMetricsFile core14_alias_table font

    bbox           = BBox (P2 (-23) (-250)) (P2 715 805)


