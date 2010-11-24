{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.FontLoader.AfmV2
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- AFM file parser for Version 2.0.
--
-- Note - AFM Version 2.0 used by GhostScript and Version 3.0+
-- have numerous differences. 
-- 
--------------------------------------------------------------------------------

module Wumpus.Basic.FontLoader.AfmV2
  ( 

    module Wumpus.Basic.FontLoader.Base
  , module Wumpus.Basic.FontLoader.AfmV2Parser  

  , ghostScriptFontLoader
  

  
  ) where

import Wumpus.Basic.FontLoader.AfmV2Parser
import Wumpus.Basic.FontLoader.Base
import Wumpus.Basic.FontLoader.GSFontMap
import Wumpus.Basic.Graphic

import Wumpus.Core                              -- package: wumpus-core

import Data.Maybe




-- | This is the default loader...
-- 
ghostScriptFontLoader :: FilePath -> FontLoader AfmUnit
ghostScriptFontLoader font_dir_path = FontLoader
      { unit_scale_fun      = afmUnitScale
      , path_to_font_dir    = font_dir_path
      , file_name_locator   = buildName
      , font_parser         = parseAfmV2File
      , post_process        = buildGlyphMetricsTable bbox (V2 600 0) 1000
      }
  where
    buildName :: FontName -> FilePath
    buildName font = fromMaybe font $ gsMetricsFile core14_alias_table font

    bbox           = BBox (P2 (-23) (-250)) (P2 715 805)


