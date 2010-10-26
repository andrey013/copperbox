{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.FontKit.AfmV2Datatypes
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- AFM concrete syntax for Version 2.0.
-- 
--------------------------------------------------------------------------------

module Wumpus.FontKit.AfmV2Datatypes
  ( 
    AfmNumber
  , AfmName
  , AfmKey
  , AfmFile(..)
  , GlobalInfo


  ) where


import Data.Map

-- Note - for robustness there is some merit in building a simple
-- map between field names and strings representing unparsed 
-- answers.
--
-- This can handle permutations and extra fields defined by 
-- /users/.
--


type AfmNumber  = Double
type AfmKey     = String
type AfmName    = String

data AfmFile = AfmFile
      { global_info     :: GlobalInfo
--      , metric_props    :: MetricProps
      }
  deriving (Eq,Show)




type GlobalInfo = Map AfmKey String


{-

-- | @font_name@ is the name used by PostScript\'s @findfont@
-- command. Note, though that GhostScript aliases the font names
-- of the /standard/ fonts (Times, Helvetica, Courier and Symbol).
--
--
data GlobalInfo = GlobalInfo
      { afm_version             :: String
      , font_name               :: String
      , full_name               :: String
      , family_name             :: String
      , weight                  :: String
      , italic_angle            :: AfmNumber
      , is_fixed_pitch          :: Bool
      , font_bbox               :: (Int,Int,Int,Int)
      , underline_pos           :: AfmNumber
      , underline_thick         :: AfmNumber
      , version                 :: String
      , notice                  :: String
      , encoding_scheme         :: String
      , cap_height              :: AfmNumber
      , x_height                :: AfmNumber
      , ascender                :: AfmNumber
      , descender               :: AfmNumber
      }
  deriving (Eq,Show)

-- | Note the /unit/ is 1\/1000 of a point size.
--
data MetricProps = MetricProps
  deriving (Eq,Show)

data CharacterMetrics = CharacterMetrics
      { char_code               :: Int
      , width_vector            :: (AfmNumber, AfmNumber)
      , char_name               :: AfmName
      , char_bbox               :: (AfmNumber, AfmNumber, AfmNumber, AfmNumber)
      , ligature_sequence       :: [(AfmName, AfmName)]
      }
  deriving (Eq,Show)

-}