{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Extra.Text
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  experimental
-- Portability :  GHC only
--
-- Text handling
-- 
--------------------------------------------------------------------------------

module Wumpus.Extra.Text 
  ( 
  
  -- * Type synonym
    FontSize

  -- * Courier metrics at 48 point
  , courier48_width
  , courier48_body_height
  , courier48_height
  , courier48_descender_depth
  , courier48_spacer_width

  , widthAt48pt
  , textWidth
  , textHeight

  -- * Picture constructors
  , textline
  , colouredTextline

  ) where

import Wumpus.Core
import Wumpus.Core.Colour ( black )

import Data.AffineSpace         -- vector-space


type FontSize = Int

-- | The width of a letter in Courier at 48 pt.
--
-- The value is not entirely accurate but it is satisfactory.
courier48_width :: Num u => u
courier48_width = 26


-- | The height of a letter without accents, ascenders or 
-- descenders in Courier at 48 pt .
--
-- The value is not entirely accurate but it is satisfactory - 
-- some letters are taller than others (e.g. numbers are taller 
-- then capitals).
courier48_body_height :: Num u => u 
courier48_body_height = 30


-- | The /common maximum/ height of a letter in Courier at 48pt.
--
-- By common maximum the letter is allowed to have both an accent 
-- or ascender and a descender.
--
-- Naturally the height is 48.0.
--
courier48_height :: Num u => u
courier48_height = 48


-- | The depth of a descender in Courier at 48 pt.
-- 
-- Also the height of an ascender.
courier48_descender_depth :: Num u => u 
courier48_descender_depth = 9



-- | The spacing between letters printed directly with 
-- PostScript\'s show command for Courier at 48 pt.
--
-- The value is not entirely accurate but it is satisfactory.
courier48_spacer_width :: Num u => u
courier48_spacer_width = 3


-- | Width of the supplied string when printed at 48pt.
widthAt48pt :: Fractional u => String -> u
widthAt48pt s = courier48_width * len + courier48_spacer_width * len_sub
  where
    len      = fromIntegral $ length s
    len_sub  = len - 1.0

--- | Text width at @sz@ point size of the string @s@. All
-- characters are counted literally - special chars may cause
-- problems (this a current deficiency of Wumpus).
textWidth :: Fractional u => FontSize -> String -> u
textWidth sz s = (fromIntegral sz)/48 * widthAt48pt s

-- | Text height is just identity/double-coercion, i.e. 
-- @18 == 18.0@. The /size/ of a font is (apparently) the maximum
-- height (body + descender max + ascender max).
textHeight :: Num u =>  FontSize -> u
textHeight = fromIntegral

-- | Descender depth for font size @sz@.
-- 
-- (The metrics are taken from Courier, of course).
--
descenderDepth :: Fractional u => FontSize -> u
descenderDepth sz =  (fromIntegral sz) / 48 * courier48_descender_depth


-- | Draw a line of text at the given point. The bounding box is
-- calculated with Courier metrics - as Courier is a monospaced
-- font the bounding bow will be strictly wider than necessary 
-- for variable width fonts.
--
-- The supplied point represents the bottom-left corner of the 
-- bounding box accounting for descenders, this is different to
-- PostScript\'s behaviour where the the start point is the 
-- bottom-left of a stem (e.g. H,I) and does not account for
-- descenders.
--
textline :: (Fractional u, Ord u) 
         => FontAttr -> Point2 u -> String -> Picture u
textline = colouredTextline black



-- | Coloured version of 'textline'. Same conditions vis bounding
-- box metrics and start point apply. 
colouredTextline :: (Fractional u, Ord u, ToPSColour c)
                 => c -> FontAttr -> Point2 u -> String -> Picture u
colouredTextline  c attr pt s = 
    frameWithin (textlabel (toPSColour c,attr) pt' s) bb
  where
    sz          = font_size attr
    h           = textHeight sz
    w           = textWidth  sz s          
    bb          = bbox pt (pt .+^ (V2 w h))
    pt'         = pt .+^ V2 0 (descenderDepth sz)

