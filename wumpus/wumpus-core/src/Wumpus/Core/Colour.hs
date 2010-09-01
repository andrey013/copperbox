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
-- Colour represented as RGB with each component in the range 
-- [0..255].
-- 
-- Note - the predifined colours are hidden when importing the
-- /top-level/ module @Wumpus.Core@, import this module directly
-- to use them.
--
--------------------------------------------------------------------------------


module Wumpus.Core.Colour
  ( 

  -- * RGB colour type  
    RGBi(..)

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
data RGBi = RGBi !Word8 !Word8 !Word8
  deriving (Eq,Ord,Show)


--------------------------------------------------------------------------------
-- instances

instance Format RGBi where
  format (RGBi 0   0   0)    = text "*black*"
  format (RGBi 255 255 255)  = text "*white*"
  format (RGBi r   g   b)    = integral r <> comma <> integral g 
                                            <> comma <> integral b


--------------------------------------------------------------------------------


-- Some colours

-- There will be name clashes with the X11Colours / SVGColours.

-- | Black - 0, 0, 0.
--
black :: RGBi
black = RGBi 0 0 0

-- | White - 255, 255, 255.
--
white :: RGBi
white = RGBi 255 255 255

-- | Red - 255, 0, 0.
--
red :: RGBi
red = RGBi 255 0 0

-- | Green - 0, 255, 0.
--
green :: RGBi 
green = RGBi 0 255 0

-- | Blue - 0, 0, 255.
--
blue :: RGBi
blue = RGBi 0 0 255
