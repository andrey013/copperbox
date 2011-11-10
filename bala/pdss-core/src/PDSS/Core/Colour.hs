{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  PDSS.Core.Colour
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Colour represented as RGB with each component in the range 
-- [0..255].
-- 
--
--------------------------------------------------------------------------------


module PDSS.Core.Colour
  ( 

  -- * RGB colour type  
    RGBi(..)

  , rgbValue

  -- * Predefined colours
  , black
  , white
  , red
  , green
  , blue
  , yellow
  , cyan
  , magenta
    
  ) where

import PDSS.Core.Utils.FormatCombinators

import Data.Bits
import Data.Word

-- | Colours levels are in the range [0..255] - 8 bit colour - 
-- although PD only supports 6 bit colour [0..63].
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


-- | Note - Hans-Christoph Steiner\'s guide to the PD file format
-- is wrong for colour.
--
-- Colour is 18-bit.
-- See the thread in the PD-Dev mailing list:
--
-- > 2010-02 - pd file format: color settings
--
-- And the message from zac hilbert here:
-- 
-- http://puredata.hurleur.com/sujet-335-puredata-patchfile-format
--
rgbValue :: RGBi -> Int
rgbValue (RGBi r g b) = sub1 $ fromIntegral rgb * (-1)
  where
    r'   :: Word32
    r'   = (lowRes r) `shiftL` 12
    g'   = (lowRes g) `shiftL` 6
    b'   = (lowRes b)
    rgb  = r' + g' + b'

    sub1 = (subtract 1)

 

-- | Go from 8 bit to 6 bit - range [0..255] changes to [0..63].
--
lowRes :: Word8 -> Word32
lowRes i = floor $ (63.0 :: Double) * ((fromIntegral i) / 255.0)




--------------------------------------------------------------------------------


-- Some colours

-- There will be name clashes with the X11Colours / SVGColours.

-- | Black - 0, 0, 0.
--
black           :: RGBi
black           = RGBi 0 0 0

-- | White - 255, 255, 255.
--
white           :: RGBi
white           = RGBi 255 255 255

-- | Red - 255, 0, 0.
--
red             :: RGBi
red             = RGBi 255 0 0

-- | Green - 0, 255, 0.
--
green           :: RGBi 
green           = RGBi 0 255 0

-- | Blue - 0, 0, 255.
--
blue            :: RGBi
blue            = RGBi 0 0 255

-- | Yellow - 255, 255, 0.
--
yellow          :: RGBi
yellow          = RGBi 255 255 0

-- | Cyan - 0, 255, 255.
--
cyan            :: RGBi
cyan            = RGBi 0 255 255

-- | Magenta - 255, 0, 255.
--
magenta         :: RGBi
magenta         = RGBi 255 0 255
