{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.System.FontLoader.Internal.AfmV2
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

module Wumpus.Basic.System.FontLoader.Internal.Base
  (
    FontLoadErr
  , FontLoadIO
  , runFontLoadIO
  , evalFontLoadIO
  , loadError
  , logLoadMsg
  , promoteIO
  , promoteEither
  , runParserFLIO

  -- * Afm Unit
  , AfmUnit
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

  , MonospaceDefaults(..)

  -- * Font loading

  , buildAfmFontProps
  , checkFontPath
  
  ) where

import Wumpus.Basic.Kernel


import Wumpus.Core                              -- package: wumpus-core
import Wumpus.Core.Text.GlyphIndices
import Wumpus.Basic.Utils.HList
import Wumpus.Basic.Utils.ParserCombinators


import Control.Monad
import qualified Data.IntMap            as IntMap
import qualified Data.Map as Map
import Data.Monoid
import System.Directory
import System.FilePath



--------------------------------------------------------------------------------
-- FontLoadIO monad - IO plus Error



type FontLoadErr        = String

newtype FontLoadLog     = FontLoadLog { getFontLoadLog :: H String }

instance Monoid FontLoadLog where
  mempty        = FontLoadLog $ emptyH
  a `mappend` b = FontLoadLog $ getFontLoadLog a `appendH` getFontLoadLog b



newtype FontLoadIO a = FontLoadIO { 
          getFontLoadIO :: IO (Either FontLoadErr a, FontLoadLog ) }

instance Functor FontLoadIO where
  fmap f ma = FontLoadIO $ getFontLoadIO ma >>= \(a,w) -> return (fmap f a, w)
 
instance Monad FontLoadIO where
  return a = FontLoadIO $ return (Right a, mempty)
  m >>= k  = FontLoadIO $ getFontLoadIO m >>= fn 
              where
                fn (Left err, w) = return (Left err, w)
                fn (Right a, w1) = getFontLoadIO (k a) >>= \(b,w2) -> 
                                   return (b, w1 `mappend` w2)

runFontLoadIO :: FontLoadIO a -> IO (Either FontLoadErr a,[String])
runFontLoadIO ma = liftM post $ getFontLoadIO ma 
  where
    post (ans,w) = (ans, toListH $ getFontLoadLog w)


evalFontLoadIO :: FontLoadIO a -> IO (Either FontLoadErr a)
evalFontLoadIO ma = liftM post $ getFontLoadIO ma
  where
    post (ans,_) = ans


loadError :: FontLoadErr -> FontLoadIO a
loadError msg = FontLoadIO $ return $ (Left msg, mempty)

logLoadMsg :: String -> FontLoadIO ()
logLoadMsg msg = FontLoadIO $ return $ (Right (), FontLoadLog $ wrapH msg ) 


-- | aka liftIO
promoteIO :: IO a -> FontLoadIO a
promoteIO ma = FontLoadIO $ ma >>= \a -> return (Right a, mempty)

promoteEither :: Either FontLoadErr a -> FontLoadIO a
promoteEither = either loadError return 

runParserFLIO :: FilePath -> Parser Char a -> FontLoadIO a
runParserFLIO filepath p = 
   promoteIO (readFile filepath) >>= promoteEither . runParserEither p






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
-- Note - Bounding Box is mandatory for AFM versions 3.0 and 4.1
-- 
-- Cap Height is optional in AFM versions 3.0 and 4.1. As Wumpus 
-- uses cap height in calculations, glyph metrics must be build 
-- with an arbitrary value if it is not present.
--
-- Encoding Scheme is optional in AFM files.
--
data AfmFile = AfmFile 
      { afm_encoding        :: Maybe String
      , afm_letter_bbox     :: Maybe AfmBoundingBox
      , afm_cap_height      :: Maybe AfmUnit
      , afm_glyph_metrics   :: [AfmGlyphMetrics]
      }
  deriving (Show) 

-- Note BBox is a required field for version 4.1.


data AfmGlyphMetrics = AfmGlyphMetrics
      { afm_char_code       :: !PSCharCode
      , afm_width_vector    :: !(Vec2 AfmUnit)
      , afm_char_name       :: !String
      }
  deriving (Eq,Show)


-- | Monospace defaults are used if the font loader fails to 
-- extract the necessary fields.
-- 
-- The values are taken from the font correpsonding to Courier 
-- in the distributed font files.
--
data MonospaceDefaults cu = MonospaceDefaults 
      { default_letter_bbox  :: BoundingBox cu
      , default_cap_height   :: cu
      , default_char_width   :: Vec2 cu
      }
  deriving (Eq,Show)




-- | Afm files do not have a default advance vec so use the 
-- monospace default.
-- 
-- Afm files hopefully have @CapHeight@ and @FontBBox@ properties
-- in the header. Use the monospace default only if they are 
-- missing.
-- 
buildAfmFontProps :: MonospaceDefaults AfmUnit 
                  -> AfmFile 
                  -> FontLoadIO (FontProps AfmUnit)
buildAfmFontProps defaults afm = do 
    cap_height <- extractCapHeight defaults afm
    bbox       <- extractFontBBox  defaults afm 
    return $ FontProps 
               { fp_bounding_box    = bbox
               , fp_default_adv_vec = default_char_width defaults
               , fp_adv_vecs        = char_widths
               , fp_cap_height      = cap_height
               }  
  where
    char_widths = foldr fn IntMap.empty $ afm_glyph_metrics afm
 
    fn (AfmGlyphMetrics _ v ss) table = case Map.lookup ss ps_glyph_indices of
                                          Nothing -> table
                                          Just i  -> IntMap.insert i v table


extractCapHeight :: MonospaceDefaults AfmUnit -> AfmFile -> FontLoadIO AfmUnit
extractCapHeight defaults afm = maybe errk return $ afm_cap_height afm
  where
    errk = logLoadMsg "WARNING - Could not extract CapHeight" >> 
           return (default_cap_height defaults)


extractFontBBox :: MonospaceDefaults AfmUnit -> AfmFile 
                -> FontLoadIO (BoundingBox AfmUnit)
extractFontBBox defaults afm = maybe errk return $ afm_letter_bbox afm
  where
    errk = logLoadMsg "WARNING - Could not extract CapHeight" >> 
           return (default_letter_bbox defaults)



checkFontPath :: FilePath -> FilePath -> FontLoadIO FilePath
checkFontPath path_root font_file_name = 
    let full_path = normalise (path_root </> font_file_name)
    in do { check <- promoteIO (doesFileExist full_path)
          ; if check then return full_path
                     else loadError $ "Could not resolve path: " ++ full_path
          }
