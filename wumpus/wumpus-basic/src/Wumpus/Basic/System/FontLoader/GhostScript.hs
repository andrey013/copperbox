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
  

  
  ) where

import Wumpus.Basic.Kernel
import Wumpus.Basic.System.FontLoader.Internal.AfmV2Parser
import Wumpus.Basic.System.FontLoader.Internal.Base
import Wumpus.Basic.System.FontLoader.Internal.GSFontMap

import Wumpus.Core                              -- package: wumpus-core

import Data.Either





-- TODO need loaders that write log to StdOut...


loadGSMetrics :: FilePath -> [FontName] -> IO GlyphMetrics
loadGSMetrics font_dir_path ns = do 
    calcs <- fmap rights $ mapM mstep ns
    return $ foldr insertFont emptyGlyphMetrics calcs
  where
    mstep font_name = evalFontLoadIO $ 
        gsLoadFontCalcs font_dir_path ghostscript_fontmap_8_54 font_name



gsLoadFontCalcs :: FilePath -> GSFontMap -> FontName -> FontLoadIO FontCalcs
gsLoadFontCalcs font_dir_path fm name = do
    font_file   <- resolveFontFile fm name 
    path        <- checkFontPath font_dir_path font_file
    ans         <- runParserFLIO path afmV2Parser
    props       <- buildAfmFontProps  ghostscript_mono_defaults_8_54 ans
    return $ FontCalcs name (buildMetricsOps afmUnitScale props)


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
                      , default_char_width   = V2 600 0
                      }
  where
    bbox = BBox (P2 (-46) (-273)) (P2 650 820)