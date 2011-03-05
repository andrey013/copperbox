{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.System.FontLoader.GhostScript
-- Copyright   :  (c) Stephen Tetley 2010-2011
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

    loadGSFontMetrics

  , loadGSFont1 
   
  ) where

import Wumpus.Basic.Kernel
import Wumpus.Basic.System.FontLoader.Base.AfmV2Parser
import Wumpus.Basic.System.FontLoader.Base.Datatypes
import Wumpus.Basic.System.FontLoader.Base.FontLoadMonad
import Wumpus.Basic.System.FontLoader.Base.GSFontMap

import Wumpus.Core                              -- package: wumpus-core

import Control.Monad
import Data.Monoid


-- | 'loadGSFontMetrics' : 
-- @ path_to_gs_fonts * [font_name] -> IO FontLoadResult @ 
-- 
-- Load the supplied list of fonts. 
-- 
-- Note - if a font fails to load a message is written to the 
-- log and monospaced /fallback metrics/ are used.
--
loadGSFontMetrics :: FilePath -> [FontName] -> IO FontLoadResult
loadGSFontMetrics font_dir_path ns = 
    liftM post $ runFontLoadIO $ sequenceAll $ map mkFun ns
  where
    mkFun = gsLoadFontMetrics font_dir_path ghostscript_fontmap_8_54 

    post (Left err,msgs) = let errs = fontLoadMsg err `mappend` msgs
                           in FontLoadResult mempty errs 
    post (Right xs,msgs) = let body = foldr fn mempty xs
                           in FontLoadResult body msgs

    fn (name,metrics) table = insertFont name metrics table


-- | 'loadGSFont1' : 
-- @ path_to_gs_fonts * font_name -> IO FontLoadResult @ 
-- 
-- Load a single GhostScript font. 
-- 
-- Note - if the font fails to load a message is written to the 
-- log and monospaced /fallback metrics/ are used.
--
loadGSFont1 :: FilePath -> FontName -> IO FontLoadResult
loadGSFont1 font_dir_path name = 
   liftM post $ runFontLoadIO $ 
      gsLoadFontMetrics font_dir_path ghostscript_fontmap_8_54 name
  where
    post (Left err,msgs)    = let errs = fontLoadMsg err `mappend` msgs
                              in FontLoadResult mempty errs 
    post (Right (a,b),msgs) = let body = insertFont a b mempty
                              in FontLoadResult body msgs



gsLoadFontMetrics :: FilePath -> GSFontMap -> FontName 
                  -> FontLoadIO (FontName,FontMetrics)
gsLoadFontMetrics font_dir_path fm name = do
    tellLoadMsg  $ "Loading " ++ name
    font_file   <- resolveFontFile fm name 
    path        <- checkFontPath font_dir_path font_file
    ans         <- runParserFLIO path afmV2Parser
    props       <- buildAfmFontProps  ghostscript_mono_defaults_8_54 ans
    return (name, buildMetricsOps afmValue props)


resolveFontFile :: GSFontMap -> FontName -> FontLoadIO FilePath
resolveFontFile fm name = maybe errk return $ gsMetricsFile fm name
  where
    errk = loadError $ "Could note resolve GhostScript alias for " ++ name


-- | These are values extracted from the file @n022003l.afm@
-- which is the font @NimbusMonL-Regu@, GhostScript\'s eqivalent 
-- font for the core 14 font Courier.
--
ghostscript_mono_defaults_8_54 :: MonospaceDefaults AfmUnit
ghostscript_mono_defaults_8_54 = 
    MonospaceDefaults { default_letter_bbox  = bbox
                      , default_cap_height   = 563
                      , default_descender    = (-186)
                      , default_char_width   = V2 600 0
                      }
  where
    bbox = BBox (P2 (-46) (-273)) (P2 650 820)