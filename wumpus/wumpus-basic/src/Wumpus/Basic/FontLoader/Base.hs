{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExistentialQuantification  #-}
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
-- AFM file parser.
--
-- Note - AFM Version 2.0 used by GhostScript and Version 3.0+
-- have numerous differences. 
-- 
--------------------------------------------------------------------------------

module Wumpus.Basic.FontLoader.Base
  (

  -- * Afm Unit
    AfmUnit
  , afmValue
  , afmUnitScale
  
  -- * Glyph metrics

  , PSCharCode
  , PSEncodingScheme
  , AfmBoundingBox

  , AfmKey
  , GlobalInfo
  , AfmFile(..)
  , AfmGlyphMetrics(..)

  -- * Font loading
  , FontLoadErr
  , FontLoadResult
  , FontLoader(..)
  , loadFont

  , BaseGlyphMetrics
  , loadBaseGlyphMetrics

  , buildGlyphMetricsTable

  ) where

import Wumpus.Basic.Graphic


import Wumpus.Core                              -- package: wumpus-core
import Wumpus.Core.Text.GlyphIndices

import Data.Foldable ( foldrM )
import qualified Data.IntMap            as IntMap
import qualified Data.Map as Map
import Data.Maybe

import System.Directory
import System.FilePath



-- | Wrapped Double representing 1\/1000 of the scale factor
-- (Point size) of a font. AFM files encode all measurements 
-- as these units. 
-- 
newtype AfmUnit = AfmUnit { getAfmUnit :: Double } 
  deriving (Eq,Ord,Num,Floating,Fractional,Real,RealFrac,RealFloat)

instance Show AfmUnit where
  showsPrec p d = showsPrec p (getAfmUnit d)


-- | Compute the size of a measurement in Afm units scaled by the
-- point size of the font.
--
afmValue :: FromPtSize u => AfmUnit -> PtSize -> u
afmValue u pt = fromPtSize $ (realToFrac $ getAfmUnit u) * (pt / 1000)

afmUnitScale :: AfmUnit -> PtSize 
afmUnitScale u = (realToFrac $ getAfmUnit u / 1000)


--------------------------------------------------------------------------------

-- | Afm files index glyphs by /PostScript character code/. This 
-- is not the same as Unicode, ASCII...
--
-- It is expected to be determined by @EncodingScheme@ in the
-- Global Font Information Section.
--
type PSCharCode         = Int

type PSEncodingScheme   = String

type AfmBoundingBox     = BoundingBox AfmUnit

type AfmKey         = String
type GlobalInfo     = Map.Map AfmKey String



-- | Wumpus needs a very small subset of AFM files, common to both
-- version 2.0 and version 4.1.
--
data AfmFile = AfmFile 
      { afm_encoding        :: Maybe String
      , afm_font_bbox       :: Maybe AfmBoundingBox
      , afm_cap_height      :: Maybe AfmUnit
      , afm_glyph_metrics   :: [AfmGlyphMetrics]
      }
  deriving (Show) 


data AfmGlyphMetrics = AfmGlyphMetrics
      { afm_char_code       :: !PSCharCode
      , afm_width_vector    :: !(Vec2 AfmUnit)
      , afm_char_name       :: !String
      }
  deriving (Eq,Show)



-- Maybe the CharMetricsTable should be scaled to Wumpus units as 
-- the last part of the parsing process...
--
-- No it shouldn\'t - this would disallow drawings in centimeters
-- 

type FontLoadErr        = String
type FontLoadResult cu  = Either FontLoadErr (GlyphMetricsTable cu)

data FontLoader cu = forall interim. FontLoader 
      { unit_scale_fun      :: cu -> PtSize
      , path_to_font_dir    :: FilePath
      , file_name_locator   :: FontName -> FilePath
      , font_parser         :: FilePath -> IO (Either String interim)
      , post_process        :: interim -> GlyphMetricsTable cu
      }


loadFont :: FontLoader cu -> FontName -> IO (FontLoadResult cu)
loadFont loader font_name = 
    locateStep loader font_name >>= \ans -> case ans of
      Nothing        -> return $ Left $ "Cannot find font " ++ font_name
      Just full_path -> parseStep loader full_path

locateStep :: FontLoader cu -> FontName -> IO (Maybe FilePath)
locateStep loader font_name = 
    doesFileExist full_path >>= \check -> 
    if check then return $ Just full_path
             else return $ Nothing
  where
    full_path = normalise $ path_to_font_dir loader 
                             </> file_name_locator loader font_name


parseStep :: FontLoader cu -> FilePath -> IO (FontLoadResult cu)
parseStep (FontLoader _ _ _ parser post) valid_path = 
    fmap (either Left (Right . post)) $ parser valid_path

--------------------------------------------------------------------------------



loadBaseGlyphMetrics :: FontLoader u -> [FontName] -> IO BaseGlyphMetrics
loadBaseGlyphMetrics loader xs = foldrM fn Map.empty xs
  where
    fn font_name acc = loadFont loader font_name >>= \ans -> 
                       case ans of
                         Left err -> reportBaseError font_name err >> return acc
                         Right table -> return $ 
                             Map.insert font_name (tableToGM table) acc

    tableToGM = buildMetrics (unit_scale_fun loader)  
    
reportBaseError :: FontName -> FontLoadErr -> IO ()
reportBaseError font_name err = do 
    putStrLn $ "The font " ++ font_name ++ " failed to load, with the error:"
    putStrLn $ err




buildGlyphMetricsTable :: BoundingBox AfmUnit 
                       -> Vec2 AfmUnit 
                       -> AfmUnit
                       -> AfmFile 
                       -> GlyphMetricsTable AfmUnit
buildGlyphMetricsTable bbox dflt_vec dflt_cap_height afm = 
    GlyphMetricsTable 
      { glyph_bounding_box    = bbox
      , glyph_default_adv_vec = dflt_vec
      , glyph_adv_vecs        = makeAdvVecs $ afm_glyph_metrics afm
      , glyph_cap_height      = fromMaybe dflt_cap_height $ afm_cap_height afm
      }  


makeAdvVecs :: [AfmGlyphMetrics] -> IntMap.IntMap (Vec2 AfmUnit)
makeAdvVecs  = foldr fn IntMap.empty
  where
    fn (AfmGlyphMetrics _ v ss) table = case Map.lookup ss ps_glyph_indices of
        Nothing -> table
        Just i  -> IntMap.insert i v table

