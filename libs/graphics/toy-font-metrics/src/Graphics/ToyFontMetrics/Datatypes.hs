{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.ToyFontMetrics.Datatypes
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- AFM concrete syntax.
-- 
--------------------------------------------------------------------------------

module Graphics.ToyFontMetrics.Datatypes
  ( 
    AfmFile(..)
  , AfmHeader(..)
  , MetricProps(..)
  ) where

data AfmFile = AfmFile
      { header          :: AfmHeader
      , metric_props    :: MetricProps
      }
  deriving (Eq,Show)

-- | @font_name@ is the name used by PostScript\'s @findfont@
-- command. Note, though that GhostScript aliases the font names
-- of the /standard/ fonts (Times, Helvetica, Courier and Symbol).
--
--
data AfmHeader = AfmHeader
      { afm_version     :: String
      , metrics_sets    :: Int     
      , font_name       :: String -- This is used by PostScript\'s @findfont@
      , full_name       :: String
      , family_name     :: String
      }
  deriving (Eq,Show)

-- | Note the /unit/ is 1\/1000 of a point size.
--
data MetricProps = MetricProps
  deriving (Eq,Show)