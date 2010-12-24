{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.System.FontLoader.Afm
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Font loader / import shim for the Adobe \"Core 14\" glyph 
-- metrics.
--
-- Use this loader if you have the Adode glyph metrics set
-- (AFM v4.1). This metrics set is avaiable from the Adobe 
-- website.
--
--------------------------------------------------------------------------------

module Wumpus.Basic.System.FontLoader.Afm
  ( 

    module Wumpus.Basic.System.FontLoader.Internal.Base
  , module Wumpus.Basic.System.FontLoader.Internal.AfmV4Dot1Parser  

  , loadAfmMetrics
  , afmV4Dot1Loader
  

  
  ) where

import Wumpus.Basic.Kernel
import Wumpus.Basic.System.FontLoader.Internal.AfmV4Dot1Parser
import Wumpus.Basic.System.FontLoader.Internal.Base

import Wumpus.Core                              -- package: wumpus-core


-- The file names of the Afm fonts match there PostScript names,
-- the only difference is the addition of a @.afm@ extension.
--


loadAfmMetrics :: FilePath -> [FontName] -> IO GlyphMetrics
loadAfmMetrics font_dir_path ns = 
    loadGlyphMetrics (afmV4Dot1Loader font_dir_path) ns


afmV4Dot1Loader :: FilePath -> FontLoaderAlg AfmUnit
afmV4Dot1Loader font_dir_path = 
    FontLoaderAlg
      { unit_scale_fun      = afmUnitScale
      , path_to_font_dir    = font_dir_path
      , file_name_locator   = buildName
      , font_parser         = parseAfmV4Dot1File
      , post_process        = buildFontProps bbox (V2 600 0) 1000
      }
  where
    buildName :: FontName -> FilePath
    buildName font = font ++ ".afm"

    bbox           = BBox (P2 (-23) (-250)) (P2 715 805)


