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
  

  
  ) where

import Wumpus.Basic.Kernel
import Wumpus.Basic.System.FontLoader.Internal.AfmV4Dot1Parser
import Wumpus.Basic.System.FontLoader.Internal.Base

import Wumpus.Core                              -- package: wumpus-core

import Data.Either

-- The file names of the Afm fonts match there PostScript names,
-- the only difference is the addition of a @.afm@ extension.
--

-- TODO need loaders that write log to StdOut...

loadAfmMetrics :: FilePath -> [FontName] -> IO GlyphMetrics
loadAfmMetrics font_dir_path ns = do
    calcs <- fmap rights $ mapM mstep ns
    return $ foldr insertFont emptyGlyphMetrics calcs
  where
    mstep = evalFontLoadIO . afmLoadFontCalcs font_dir_path



afmLoadFontCalcs :: FilePath -> FontName -> FontLoadIO FontCalcs
afmLoadFontCalcs font_dir_path name = do
    path        <- checkFontPath font_dir_path (name ++ ".afm")
    ans         <- runParserFLIO path afmV4Dot1Parser
    props       <- buildAfmFontProps  afm_mono_defaults_4_1 ans
    return $ FontCalcs name (buildMetricsOps afmUnitScale props)



-- | These are values extracted from Courier in the core 14 fonts.
--
afm_mono_defaults_4_1 :: MonospaceDefaults AfmUnit
afm_mono_defaults_4_1 = 
    MonospaceDefaults { default_letter_bbox  = bbox
                      , default_cap_height   = 562
                      , default_char_width   = V2 600 0
                      }
  where
    bbox = BBox (P2 (-23) (-250)) (P2 715 805)