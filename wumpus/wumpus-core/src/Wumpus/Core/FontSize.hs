{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Core.FontSize
-- Copyright   :  (c) Stephen Tetley 2009-2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC with TypeFamilies and more
--
-- Font size calculation for Label\'s and their bounding boxes.
-- 
-- Calculations are based on metrics derived from Courier at 
-- 48pt. As Courier is a monospaced font, bounding boxes 
-- calculated for other font families will usually have longer 
-- width than is necessary for the printed text. 
-- 
-- This is a deficiency of Wumpus, and limits its text handling
-- capabilities (for example, text cannot be automatically 
-- centered). However, alternatives would need access to font 
-- metrics - this would require a font loader and add 
-- significant implementation complexity.
-- 
--------------------------------------------------------------------------------

module Wumpus.Core.FontSize
  ( 
  
  -- * Type synonyms
    FontSize
  , CharCount

  -- * Courier metrics at 48 point
  , courier48_width
  , courier48_height
  , courier48_numeral_height
  , courier48_xheight
  , courier48_descender_depth
  , courier48_ascender_height
  , courier48_spacer_width


  -- * Metrics calculation
  , widthAt48pt
  , textWidth
  , textHeight
  , numeralHeight
  , xcharHeight
  , descenderDepth
  , textBounds

  ) where

import Wumpus.Core.BoundingBox
import Wumpus.Core.Geometry
import Wumpus.Core.PtSize

import Data.AffineSpace                 -- package: vector-space



type CharCount = Int
type FontSize = Int

-- NOTE - I\'ve largely tried to follow the terminoly from 
-- Edward Tufte\'s /Visual Explantions/, page 99.
--


-- | The width of a letter in Courier at 48 pt.
--
-- The value is not entirely accurate but it is satisfactory.
--
-- > width = 26.0 
--
courier48_width :: PtSize
courier48_width = 26.0

-- | The point size of a character in Courier at 48 pt.
--
-- \*\* Naturally the height is 48.0 \*\*.
--
courier48_height :: PtSize 
courier48_height = 48.0



-- | The height of a numeral without accents, ascenders or 
-- descenders in Courier at 48 pt.
--
-- Note - the height of a numeral in Courier is slightly 
-- larger than a upper-case letter.
--
-- > numeral_height = 30.0 
--
courier48_numeral_height :: PtSize 
courier48_numeral_height = 30.0

-- | The height of the body of a lower-case letter 
--  (typically the letter  \'x\') in Courier at 48 pt. 
--
-- This is also known as the \"body height\".
--
-- > xheight = 20.0 
-- 
courier48_xheight :: PtSize
courier48_xheight = 20.0


-- | The depth of a descender in Courier at 48 pt.
-- 
-- > descender_depth = 9.0
-- 
courier48_descender_depth :: PtSize
courier48_descender_depth = 9.0

-- | The depth of an ascender in Courier at 48 pt.
-- 
-- > ascender_height = 10.0
-- 
-- Note - for Courier point size is not the sum of
-- descender, ascender and xheight and lower-case letters with
-- ascenders are slightly taller than upper-case letters:
--
-- > descender_depth + xheight + ascender_height /= point_size
--
-- > xheight + ascender_height /= cap_height
--
-- > xheight + ascender_height == numeral_height
--
courier48_ascender_height :: PtSize 
courier48_ascender_height = 10.0


-- | The spacing between letters printed directly with 
-- PostScript\'s show command for Courier at 48 pt.
--
-- The value is not entirely accurate but it is satisfactory
-- for bounding box calculations.
--
-- > spacer_width = 3.0
--
courier48_spacer_width :: PtSize
courier48_spacer_width = 3.0


-- | Width of the supplied string when printed at 48pt.
--
widthAt48pt :: CharCount -> PtSize
widthAt48pt n = courier48_width * len + courier48_spacer_width * len_sub
  where
    len      = fromIntegral n
    len_sub  = len - 1.0

-- | Text width at @sz@ point size of the string @s@. All
-- characters are counted literally - special chars may cause
-- problems (this a current deficiency of Wumpus).
--
textWidth :: FontSize -> CharCount -> PtSize
textWidth sz n = (fromIntegral sz)/48 * widthAt48pt n


-- | Text height is just identity/double-coercion of the Point size.
-- i.e. @18 == 18.0@. The /size/ of a font is the maximum height:
--
textHeight :: FontSize -> PtSize
textHeight = fromIntegral

-- | Approximate the height of a numeral using metrics derived 
-- from the Courier monospaced font.
--
numeralHeight :: FontSize -> PtSize
numeralHeight sz = textHeight sz * (courier48_numeral_height / courier48_height)

-- | Approximate the height of the lower-case char \'x\' using 
-- metrics derived from the Courier monospaced font.
--
xcharHeight :: FontSize -> PtSize
xcharHeight sz = textHeight sz * (courier48_xheight / courier48_height)


-- | Approximate the descender depth for font size @sz@ using
-- metrics derived from the Courier monospaced font.
-- 
descenderDepth :: FontSize -> PtSize
descenderDepth sz = (fromIntegral sz) / 48 * courier48_descender_depth

-- | Find the bounding box for the character count at the 
-- supplied font-size.
-- 
-- The supplied point represents the bottom left corner of the 
-- a regular upper-case letter (that is without descenders).
-- The bounding box will always be /dropped/ to accommodate 
-- ascenders - no interpretation of the string takes place to 
-- see if it actually contains ascenders or descenders.
--  
-- The metrics used are derived from Courier - a monospaced font.
-- For variable width fonts the calculated bounding box will 
-- usually be too long.
--
textBounds :: (Fractional u, Ord u, FromPtSize u) 
           => FontSize -> Point2 u -> CharCount -> BoundingBox u
textBounds sz body_bl n = bbox bl tr where
    h           = fromPtSize $ textHeight sz
    w           = fromPtSize $ textWidth  sz n
    dd          = fromPtSize $ descenderDepth sz
    bl          = body_bl .-^ V2 0 dd 
    tr          = bl .+^ V2 w h
  