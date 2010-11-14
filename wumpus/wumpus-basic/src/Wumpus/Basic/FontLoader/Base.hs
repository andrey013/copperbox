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
  
  -- * Glyph metrics

  , PSCharCode
  , PSEncodingScheme
  , AfmBoundingBox
  , AfmGlyphMetrics(..)

  -- * Font loading
  , FontName
  , FontLoadErr
  , FontLoadResult
  , FontLoader(..)
  , loadFont

  , BaseGlyphMetrics
  , loadBaseGlyphMetrics

  ) where

import Wumpus.Basic.Text.Datatypes              


import Wumpus.Core                              -- package: wumpus-core

import Data.Foldable ( foldrM )
import qualified Data.Map as Map
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



-- | Afm files index glyphs by /PostScript character code/.
-- This is not the same as Unicode, ASCII...
--
-- It is expected to be determined by @EncodingScheme@ in the
-- Global Font Information Section.
--
type PSCharCode         = Int

type PSEncodingScheme   = String

type AfmBoundingBox     = BoundingBox AfmUnit

data AfmGlyphMetrics = AfmGlyphMetrics
      { afm_char_code       :: !PSCharCode
      , afm_width_vector    :: !(Vec2 AfmUnit)
      , afm_char_name       :: !String
      }
  deriving (Eq,Show)

-- Maybe the CharMetricsTable should be scaled to Wumpus units as 
-- the last part of the parsing process...

type FontName           = String
type FontLoadErr        = String
type FontLoadResult cu  = Either FontLoadErr (GlyphMetricsTable cu)

data FontLoader u = forall interim. FontLoader 
      { path_to_font_dir    :: FilePath
      , file_name_locator   :: FontName -> FilePath
      , font_parser         :: FilePath -> IO (Either String interim)
      , post_process        :: interim -> GlyphMetricsTable u
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
parseStep (FontLoader _ _ parser post) valid_path = 
    fmap (either Left (Right . post)) $ parser valid_path

--------------------------------------------------------------------------------

type BaseGlyphMetrics u = Map.Map FontName (GlyphMetricsTable u)


loadBaseGlyphMetrics :: FontLoader u -> [FontName] -> IO (BaseGlyphMetrics u)
loadBaseGlyphMetrics loader xs = foldrM fn Map.empty xs
  where
    fn font_name acc = loadFont loader font_name >>= \ans -> 
                       case ans of
                         Left err -> reportBaseError font_name err >> return acc
                         Right table -> return $ Map.insert font_name table acc


reportBaseError :: FontName -> FontLoadErr -> IO ()
reportBaseError font_name err = do 
    putStrLn $ "The font " ++ font_name ++ " failed to load, with the error:"
    putStrLn $ err

