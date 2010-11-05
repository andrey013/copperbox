{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Core.FontSize
-- Copyright   :  (c) Stephen Tetley 2009-2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Font size calculation for Label\'s and their bounding boxes.
-- 
-- Calculations are based on metrics derived from Courier at 
-- 48pt. As Courier is a monospaced font, applying these metrics
-- to other font families will usually produce over-estimates
-- (bounding boxes will be longer than the true visual length
-- of the text).
-- 
-- This is a deficiency of Wumpus, and limits its text handling
-- capabilities - for example, text cannot be reliably centered 
-- or right aligned as its true length is not known. However, more 
-- powerful alternatives would need access to the metrics embedded 
-- within font files. This would require a font loader and add 
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
  , charWidth
  , spacerWidth
  , textWidth
  , textHeight
  , numeralHeight
  , xcharHeight
  , descenderDepth
  , textBounds
  , textBoundsEnc       -- hidden by toplevel Wumpus.Core
  , charCount

  ) where

import Wumpus.Core.BoundingBox
import Wumpus.Core.Geometry
import Wumpus.Core.PtSize
import Wumpus.Core.Text.TextInternal

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


-- | Width of the supplied string when printed at 48pt (i.e. n 
-- chars + (n-1) spacers).
--
-- Use 'charCount' to calculate the @CharCount@.
--
widthAt48pt :: CharCount -> PtSize
widthAt48pt n = (courier48_width * len) + (courier48_spacer_width * len_sub)
  where
    len      = fromIntegral n
    len_sub  = len - 1.0

-- | Approximate the width of a monospace character using 
-- metrics derived from the Courier font.
--
charWidth :: FontSize -> PtSize
charWidth sz = (fromIntegral sz)/48 * courier48_width


-- | Approximate the width of a spacer between monospace 
-- characters using metrics derived from the Courier font.
--
spacerWidth :: FontSize -> PtSize
spacerWidth sz = (fromIntegral sz)/48 * courier48_spacer_width



-- | Text width at @sz@ point size of the string @s@. All
-- characters are counted literally - special chars may cause
-- problems (this a current deficiency of Wumpus).
--
textWidth :: FontSize -> CharCount -> PtSize
textWidth _  n | n <= 0 = 0
textWidth sz n          = (fromIntegral sz)/48 * widthAt48pt n


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

-- | 'textBounds' : @ font_size * baseline_left * text -> BBox@
--
-- Find the bounding box for the character count at the 
-- supplied font-size.
-- 
-- The supplied point represents the baseline left corner of the 
-- a regular upper-case letter (that is without descenders).
-- The bounding box will always be /dropped/ to accommodate 
-- ascenders - no interpretation of the string takes place to 
-- see if it actually contains ascenders or descenders.
--  
-- The metrics used are derived from Courier - a monospaced font.
-- For variable width fonts the calculated bounding box will 
-- usually be too long.
--
textBounds :: (Num u, Ord u, FromPtSize u) 
           => FontSize -> Point2 u -> String -> BoundingBox u
textBounds sz pt ss = textBoundsBody sz pt (charCount ss) 


-- | 'textBoundsEnc' : @ font_size * baseline_left * encoded_text -> BBox@
-- 
--  Version of textBounds for EncodedText.
-- 
-- Note this function is hidded by the top-level module 
-- @Wumpus.Core@.
--
textBoundsEnc :: (Num u, Ord u, FromPtSize u) 
           => FontSize -> Point2 u -> EncodedText -> BoundingBox u
textBoundsEnc sz pt enc = textBoundsBody sz pt (textLength enc) 


textBoundsBody :: (Num u, Ord u, FromPtSize u) 
               => FontSize -> Point2 u -> Int -> BoundingBox u
textBoundsBody sz baseline_left len = boundingBox bl tr 
  where
    h           = fromPtSize $ textHeight sz
    w           = fromPtSize $ textWidth  sz len
    dd          = fromPtSize $ descenderDepth sz
    bl          = baseline_left .-^ V2 0 dd 
    tr          = bl .+^ V2 w h




-- | Count the charcters in the supplied string.
--
-- Note escapes count as one character - for instance the length 
-- of this string:
--
-- > abcd&#egrave;f
--
-- ... is 6.
-- 
charCount :: String -> CharCount
charCount = outstep 0 
  where
    outstep n ('&':'#':xs)  = instep n xs
    outstep n (_:xs)        = outstep (n+1) xs
    outstep n []            = n
    
    instep  n (';':xs)      = outstep (n+1) xs
    instep  n (_:xs)        = instep  n xs
    instep  n []            = n                

-- Note - the last case of instep indicates a malformed string, 
-- but there is nothing that can be done. Promoting to Maybe or 
-- Either would complicated the interface.