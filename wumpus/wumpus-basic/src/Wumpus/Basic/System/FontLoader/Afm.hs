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
    loadAfmMetrics
  
  ) where

import Wumpus.Basic.Kernel
import Wumpus.Basic.System.FontLoader.Base.AfmV4Dot1Parser
import Wumpus.Basic.System.FontLoader.Base.Datatypes
import Wumpus.Basic.System.FontLoader.Base.FontLoadMonad


import Wumpus.Core                              -- package: wumpus-core

import Control.Monad
import Data.Monoid

-- The file names of the Afm fonts match there PostScript names,
-- the only difference is the addition of a @.afm@ extension.
--

-- | 'loadAfmMetrics' : 
-- @ path_to_afm_fonts -> [font_name] -> IO (metrics, messages) @ 
-- 
-- Load the supplied list of fonts. 
-- 
-- Note - if a font fails to load a message is written to the 
-- log and monospaced /fallback metrics/ are used.
--
loadAfmMetrics :: FilePath -> [FontName] -> IO (GlyphMetrics, [String])
loadAfmMetrics font_dir_path ns = 
    liftM post $ runFontLoadIO $ sequenceAll $ map mkFun ns
  where
    mkFun = afmLoadFontCalcs font_dir_path  
 
    post (Left err,ss) = (mempty, ss ++ [err])      -- unreachable...
    post (Right xs,ss) = (foldr insertFont mempty xs, ss)




afmLoadFontCalcs :: FilePath -> FontName -> FontLoadIO FontMetricsOps
afmLoadFontCalcs font_dir_path name = do
    logLoadMsg  $ "Loading " ++ name
    path        <- checkFontPath font_dir_path (name ++ ".afm")
    ans         <- runParserFLIO path afmV4Dot1Parser
    props       <- buildAfmFontProps  afm_mono_defaults_4_1 ans
    return $ FontMetricsOps name (buildMetricsOps afmUnitScale props)



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