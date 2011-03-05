{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.System.FontLoader.Afm
-- Copyright   :  (c) Stephen Tetley 2010-2011
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

    loadAfmFontMetrics

  , loadAfmFont1  

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

-- | 'loadAfmFontMetrics' : 
-- @ path_to_afm_fonts * [font_name] -> IO FontLoadResult @ 
-- 
-- Load the supplied list of fonts. 
-- 
-- Note - if a font fails to load a message is written to the 
-- log and monospaced /fallback metrics/ are used.
--
loadAfmFontMetrics :: FilePath -> [FontName] -> IO FontLoadResult
loadAfmFontMetrics font_dir_path ns = 
    liftM post $ runFontLoadIO $ sequenceAll $ map mkFun ns
  where
    mkFun                = afmLoadFontMetrics font_dir_path  
 
    post (Left err,msgs) = let errs = fontLoadMsg err `mappend` msgs
                           in FontLoadResult mempty errs
    post (Right xs,msgs) = let body = foldr fn mempty xs
                           in FontLoadResult body msgs
    
    fn (name,metrics) table = insertFont name metrics table


-- TODO - need a one font version...


-- | 'loadAfmFont1' : 
-- @ path_to_afm_fonts * font_name -> IO FontLoadResult @ 
-- 
-- Load a single AFM font. 
-- 
-- Note - if the font fails to load a message is written to the 
-- log and monospaced /fallback metrics/ are used.
--
loadAfmFont1 :: FilePath -> FontName -> IO FontLoadResult
loadAfmFont1 font_dir_path name =
    liftM post $ runFontLoadIO $ afmLoadFontMetrics font_dir_path name
  where
    post (Left err,msgs)    = let errs = fontLoadMsg err `mappend` msgs
                              in FontLoadResult mempty errs
    post (Right (a,b),msgs) = let body = insertFont a b mempty
                              in FontLoadResult body msgs
    


afmLoadFontMetrics :: FilePath -> FontName -> FontLoadIO (FontName,FontMetrics)
afmLoadFontMetrics font_dir_path name = do
    tellLoadMsg  $ "Loading " ++ name
    path        <- checkFontPath font_dir_path (name ++ ".afm")
    ans         <- runParserFLIO path afmV4Dot1Parser
    props       <- buildAfmFontProps  afm_mono_defaults_4_1 ans
    return (name, buildMetricsOps afmValue props)



-- | These are values extracted from Courier in the core 14 fonts.
--
afm_mono_defaults_4_1 :: MonospaceDefaults AfmUnit
afm_mono_defaults_4_1 = 
    MonospaceDefaults { default_letter_bbox  = bbox
                      , default_cap_height   = 562
                      , default_descender    = (-157)
                      , default_char_width   = V2 600 0
                      }
  where
    bbox = BBox (P2 (-23) (-250)) (P2 715 805)