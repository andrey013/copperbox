{-# LANGUAGE RankNTypes                 #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Kernel.Base.FontMetrics
-- Copyright   :  (c) Stephen Tetley 2010-2011
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Data types representing font metrics.
-- 
--------------------------------------------------------------------------------

module Wumpus.Basic.Kernel.Base.FontMetrics
  ( 

    FontName
  , CodePoint
  , CharWidthLookup

  , FontMetrics(..)

  , FontTable
  , emptyFontTable
  , lookupFont
  , insertFont

  , FontLoadMsg
  , FontLoadLog
  , fontLoadMsg

  , FontLoadResult(..)
  , printLoadErrors

  , monospace_metrics

  
  ) where

import Wumpus.Basic.Utils.HList

import Wumpus.Core                              -- package: wumpus-core

import qualified Data.Map      as Map
import Data.Monoid



type FontName = String

-- | A Unicode code-point.
--
type CodePoint = Int




-- | A lookup function from code point to /width vector/.
--
-- Note - in PostScript terminology a width vector is not obliged
-- to be left-to-right (writing direction 0). It could be 
-- top-to-bottom (writing direction 1).
--
type CharWidthLookup u = CodePoint -> Vec2 u



-- | 'FontMetrics' store a subset of the properties available in 
-- a font file - enough to calculate accurate bounding boxes and
-- positions for text.
--
-- > Bounding box representing the maximum glyph area.
-- > Width vectors for each character.
-- > Cap height
-- > Descender depth.
--
-- Because Wumpus always needs font metrics respective to the 
-- current point size, the actual fields are all functions.
--
data FontMetrics = FontMetrics
    { get_bounding_box :: forall u. PtSize u => PsPoint -> BoundingBox u 
    , get_cw_table     :: forall u. PtSize u => PsPoint -> CharWidthLookup u
    , get_cap_height   :: forall u. PtSize u => PsPoint -> u
    , get_descender    :: forall u. PtSize u => PsPoint -> u
    }


-- | A map between a font name and the respective FontMetrics.
--
newtype FontTable = FontTable { 
          getFontTable :: Map.Map FontName FontMetrics }



instance Monoid FontTable where
  mempty        = emptyFontTable
  a `mappend` b = FontTable $ getFontTable a `mappend` getFontTable b


emptyFontTable :: FontTable
emptyFontTable = FontTable $ Map.empty


-- | 'FontLoadMsg' - type synonym for String.
--
type FontLoadMsg        = String

-- | 'FontLoadLog' is a Hughes list of Strings, so it supports 
-- efficient append.
--
newtype FontLoadLog     = FontLoadLog { getFontLoadLog :: H FontLoadMsg }


instance Monoid FontLoadLog where
  mempty        = FontLoadLog $ emptyH
  a `mappend` b = FontLoadLog $ getFontLoadLog a `appendH` getFontLoadLog b


fontLoadMsg :: String -> FontLoadLog 
fontLoadMsg = FontLoadLog . wrapH


-- Need a synonym for @FontLoading@...
data FontLoadResult = FontLoadResult
      { loaded_font_table    :: FontTable
      , loader_errors        :: FontLoadLog
      }


-- | Print the loader errors from the 'FontLoadResult' to std-out.
--
printLoadErrors :: FontLoadResult -> IO ()
printLoadErrors = mapM_ putStrLn . toListH . getFontLoadLog . loader_errors

--------------------------------------------------------------------------------


-- | 'lookupFont' : @ name * font_table -> Maybe FontMetrics @ 
-- 
-- Lookup a font in the font_table.
-- 
lookupFont :: FontName -> FontTable -> Maybe FontMetrics
lookupFont name = Map.lookup name . getFontTable

-- | 'insertFont' : @ name * font_metrics * font_table -> FontTable @ 
-- 
-- Insert a named font into the font_table.
-- 
insertFont :: FontName -> FontMetrics -> FontTable -> FontTable
insertFont name ops = 
    FontTable . Map.insert name ops . getFontTable

-- | This ignores the Char code lookup and just returns the 
-- default advance vector.
--
monospace_metrics :: FontMetrics
monospace_metrics = FontMetrics
    { get_bounding_box  = \sz -> BBox (lowerLeft sz) (upperRight sz)
    , get_cw_table      = \sz _ -> hvec (upscale sz width_vec) 
    , get_cap_height    = \sz -> upscale sz cap_height
    , get_descender     = \sz -> upscale sz descender
    }
  where
    llx             = (-23)  / 1000
    lly             = (-250) / 1000
    urx             = 715    / 1000
    ury             = 805    / 1000
    width_vec       = 600    / 1000
    cap_height      = 562    / 1000
    descender       = (-157) / 1000

    upscale sz d    = fromPsPoint $ sz * d
    lowerLeft sz    = P2 (upscale sz llx) (upscale sz lly) 
    upperRight sz   = P2 (upscale sz urx) (upscale sz ury) 


