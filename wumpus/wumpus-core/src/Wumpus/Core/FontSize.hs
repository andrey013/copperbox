{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
  , PtScale
  , scalePt

  -- * Scaling values derived from Courier
  , mono_width
  , mono_cap_height
  , mono_x_height
  , mono_descender
  , mono_ascender
  , mono_left_margin
  , mono_right_margin

  -- * Metrics calculation
  , charWidth
  , textWidth
  , capHeight
  , xcharHeight
  , ascenderHeight
  , descenderDepth
  , textBounds
  , textBoundsEsc
  , charCount

  ) where

import Wumpus.Core.BoundingBox
import Wumpus.Core.Geometry
import Wumpus.Core.PtSize
import Wumpus.Core.Text.Base



type CharCount = Int
type FontSize = Int


-- | Wrapped Double representing 1\/1000 of the scale factor
-- (Point size) of a font. AFM files encode all measurements 
-- as these units. 
-- 
newtype PtScale = PtScale { getPtScale :: Double } 
  deriving (Eq,Ord,Num,Floating,Fractional,Real,RealFrac,RealFloat)


instance Show PtScale where
  showsPrec p d = showsPrec p (getPtScale d)

scalePt :: PtScale -> PtSize -> PtSize 
scalePt sc sz = sz * realToFrac sc


-- NOTE - I\'ve largely tried to follow the terminoloy from 
-- Edward Tufte\'s /Visual Explantions/, page 99.
--


-- | The ratio of width to point size of a letter in Courier.
--
-- > mono_width = 0.6 
--
mono_width :: PtScale
mono_width = 0.6

-- | The ratio of cap height to point size of a letter in Courier.
--
-- > mono_cap_height = 0.563
-- 
mono_cap_height :: PtScale 
mono_cap_height = 0.563




-- | The ratio of x height to point size of a letter in Courier. 
--
-- This is also known as the \"body height\".
--
-- > mono_x_height = 0.417
-- 
mono_x_height :: PtScale
mono_x_height = 0.417


-- | The ratio of descender depth to point size of a letter in 
-- Courier.
-- 
-- > mono_descender = -0.186
-- 
mono_descender :: PtScale
mono_descender = (-0.186)


-- | The ratio of ascender to point size of a letter in Courier.
-- 
-- > mono_ascender = 0.604
-- 
mono_ascender :: PtScale
mono_ascender = 0.604


-- | The distance from baseline to max height as a ratio to point 
-- size for Courier.
-- 
-- > mono_max_height = 0.820
-- 
mono_max_height :: PtScale 
mono_max_height = 0.820


-- | The distance from baseline to max depth as a ratio to point 
-- size for Courier.
-- 
-- > max_depth = -0.273
-- 
mono_max_depth :: PtScale 
mono_max_depth = (-0.273)


-- | The left margin for the bounding box of printed text as a 
-- ratio to point size for Courier.
-- 
-- > mono_left_margin = -0.46
-- 
mono_left_margin :: PtScale 
mono_left_margin = (-0.46)


-- | The right margin for the bounding box of printed text as a 
-- ratio to point size for Courier.
-- 
-- > mono_right_margin = 50
-- 
mono_right_margin :: PtScale 
mono_right_margin = 50


-- | Approximate the width of a monospace character using 
-- metrics derived from the Courier font.
--
charWidth :: FontSize -> PtSize
charWidth = scalePt mono_width . fromIntegral





-- | Text width at @sz@ point size of the string @s@. All
-- characters are counted literally - it is expected that 
-- @CharCount@ has been calculated with the @charCount@ function.
--
-- Note - this does not account for left and right margins around
-- the printed text.
--
textWidth :: FontSize -> CharCount -> PtSize
textWidth _  n | n <= 0 = 0
textWidth sz n          = fromIntegral n * charWidth sz


-- | Height of capitals e.g. \'A\' using metrics derived 
-- the Courier monospaced font.
--
capHeight :: FontSize -> PtSize
capHeight = fromIntegral


-- | Height of the lower-case char \'x\' using metrics derived 
-- the Courier monospaced font.
--
xcharHeight :: FontSize -> PtSize
xcharHeight = scalePt mono_x_height . fromIntegral



-- | Ascender height for font size @sz@ using metrics from the 
-- Courier monospaced font.
-- 
ascenderHeight :: FontSize -> PtSize
ascenderHeight = scalePt mono_ascender . fromIntegral 



-- | Descender depth for font size @sz@ using metrics from the 
-- Courier monospaced font.
-- 
descenderDepth :: FontSize -> PtSize
descenderDepth = scalePt mono_descender . fromIntegral 


-- | 'textBounds' : @ font_size * baseline_left * text -> BBox@
--
-- Find the bounding box for the character count at the 
-- supplied font-size.
-- 
-- The supplied point represents the baseline left corner of the 
-- a regular upper-case letter (that is without descenders).
-- The bounding box adds a margin around all sides of the text.
--  
-- The metrics used are derived from Courier - a monospaced font.
-- For proportional fonts the calculated bounding box will 
-- usually be too long.
--
textBounds :: (Num u, Ord u, FromPtSize u) 
           => FontSize -> Point2 u -> String -> BoundingBox u
textBounds sz pt ss = textBoundsBody sz pt (charCount ss) 


-- | 'textBoundsEnc' : @ font_size * baseline_left * escaped_text -> BBox@
-- 
--  Version of textBounds for EscapedText.
--
textBoundsEsc :: (Num u, Ord u, FromPtSize u) 
           => FontSize -> Point2 u -> EscapedText -> BoundingBox u
textBoundsEsc sz pt esc = textBoundsBody sz pt (textLength esc) 


textBoundsBody :: (Num u, Ord u, FromPtSize u) 
               => FontSize -> Point2 u -> Int -> BoundingBox u
textBoundsBody sz (P2 x y) len = boundingBox ll ur
  where
    pt_sz       = fromIntegral sz
    w           = fromPtSize $ textWidth  sz len
    left_m      = fromPtSize $ scalePt mono_left_margin pt_sz
    right_m     = fromPtSize $ scalePt mono_left_margin pt_sz
    max_depth   = fromPtSize $ scalePt mono_max_depth  pt_sz
    max_height  = fromPtSize $ scalePt mono_max_height pt_sz
    ll          = P2 (x + left_m)      (y + max_depth)
    ur          = P2 (x + w + right_m) (y + max_height)




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