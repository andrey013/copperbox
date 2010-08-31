{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Core.Colour
-- Copyright   :  (c) Stephen Tetley 2009-2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Core colour.
--
--------------------------------------------------------------------------------


module Wumpus.Core.Colour
  ( 

    RGB255(..)

  -- * Predefined colours
  , black
  , white
  , red
  , green
  , blue
    
  ) where

import Wumpus.Core.FormatCombinators

import Data.Word

-- | Colours levels are in the range [0..255]
-- 
-- Note - this is the format used by SVG, whereas PostScript uses 
-- [0..1]. 
--
-- It is more efficient to prefer SVG here.
--
data RGB255 = RGB255 !Word8 !Word8 !Word8
  deriving (Eq,Ord,Show)


--------------------------------------------------------------------------------
-- instances

instance Format RGB255 where
  format (RGB255 0   0   0)    = text "*black*"
  format (RGB255 255 255 255)  = text "*white*"
  format (RGB255 r   g   b)    = integral r <> comma <> integral g 
                                            <> comma <> integral b


--------------------------------------------------------------------------------

-- Some colours

-- There will be name clashes with the X11Colours / SVGColours.

-- | Black - 0, 0, 0.
--
black :: RGB255
black = RGB255 0 0 0

-- | White - 255, 255, 255.
--
white :: RGB255
white = RGB255 255 255 255

-- | Red - 255, 0, 0.
--
red :: RGB255
red = RGB255 255 0 0

-- | Green - 0, 255, 0.
--
green :: RGB255 
green = RGB255 0 255 0

-- | Blue - 0, 0, 255.
--
blue :: RGB255
blue = RGB255 0 0 255
