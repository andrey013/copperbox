{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Drawing.Text.Base
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Helpers for working with measured / advance text.
-- 
--------------------------------------------------------------------------------

module Wumpus.Drawing.Text.Base
  ( 

    advtext
  , textVector
  , charVector

  , multilineHeight
  , borderedTextPos
  , borderedRotTextPos

  , centerToBaseline
  , centerSpinePoints

  ) where

import Wumpus.Drawing.Chains

import Wumpus.Basic.Kernel                      -- package: wumpus-basic

import Wumpus.Core                              -- package: wumpus-core
import Wumpus.Core.Text.GlyphIndices

import Data.VectorSpace                         -- package: vector-space

import Control.Applicative
import Data.Char
import qualified Data.Map               as Map
import Data.Maybe 


-- | Single line text, returning its advance vector.
--
advtext :: FromPtSize u => EscapedText -> AdvGraphic u
advtext esc = lift0R1 (textVector esc) >>= \v -> replaceAns v $ escapedline esc


-- kernchar versions to do...


textVector :: FromPtSize u => EscapedText -> DrawingInfo (AdvanceVec u)
textVector esc = 
    cwLookupTable >>= \table -> 
       let cs = destrEscapedText id esc 
       in return $ foldr (\c v -> v ^+^ (charWidth table c)) (vec 0 0) cs
     
charVector :: FromPtSize u => EscapedChar -> DrawingInfo (AdvanceVec u)
charVector ch = cwLookupTable >>= \table -> return $ charWidth table ch
   

-- | This is outside the Drawing context as we don\'t want to get
-- the @cwLookupTable@ for every char.
--
charWidth :: FromPtSize u 
          => CharWidthLookup u -> EscapedChar -> AdvanceVec u
charWidth fn (CharLiteral c) = fn $ ord c
charWidth fn (CharEscInt i)  = fn i
charWidth fn (CharEscName s) = fn ix
  where
    ix = fromMaybe (-1) $ Map.lookup s ps_glyph_indices


--------------------------------------------------------------------------------
-- Measurement and start points for multiline text


-- | Height of multiline text is cap_height to descender for the 
-- first line, then baseline-to-baseline span for the remaining
-- lines.
--
multilineHeight :: (Real u, Floating u, FromPtSize u) 
                   => Int -> DrawingInfo u
multilineHeight line_count 
    | line_count < 1  = return 0
    | line_count == 1 = glyphVerticalSpan
    | otherwise       = fn <$> glyphVerticalSpan <*> baselineSpacing
  where
    fn h1 bspan = let rest_spans = bspan * fromIntegral (line_count - 1)
                  in h1 + rest_spans



-- | Variant of 'textObjectPos' where the calculation includes
-- margins around all four sides of the enclosing rectangle.
--
-- Margin sizes are taken from the 'text_margin' field in the 
-- 'DrawingContext'.
--
borderedTextPos :: (Real u, Floating u, FromPtSize u) 
                      => Int -> u -> DrawingInfo (ObjectPos u)
borderedTextPos line_count w =
    multilineHeight line_count >>= \h ->
    getTextMargin              >>= \(xsep,ysep) -> 
    let hw    = xsep + (0.5 * w)
        hh    = ysep + (0.5 * h)
    in return $ ObjectPos hw hw hh hh 





-- | LR text needs the objectPos under rotation.
--
borderedRotTextPos :: (Real u, Floating u, FromPtSize u) 
             => Radian -> Int -> u -> DrawingInfo (ObjectPos u)
borderedRotTextPos theta line_count max_w =
    fmap (orthoObjectPos theta) $ borderedTextPos line_count max_w 


-- | Note - this returns the answer in center form, regardless
-- of whether the input was in center form.
-- 
-- So it is probably not a general enough function for the 
-- PosImage library.
--
orthoObjectPos :: (Real u, Floating u) 
               => Radian -> ObjectPos u -> ObjectPos u
orthoObjectPos theta (ObjectPos xmin xmaj ymin ymaj) = 
    ObjectPos bbox_hw bbox_hw bbox_hh bbox_hh
  where
    input_hw  = 0.5 * (xmin + xmaj)
    input_hh  = 0.5 * (ymin + ymaj)
    bbox0     = BBox (P2 (-input_hw) (-input_hh)) (P2 input_hw input_hh)
    bbox1     = retraceBoundary (rotateAbout theta zeroPt) bbox0
    bbox_hw   = 0.5 * (boundaryWidth  bbox1)
    bbox_hh   = 0.5 * (boundaryHeight bbox1)




-- | Calculate the distance from the center of a one-line textbox 
-- to the baseline. Note the height of a textbox is @vspan@ which 
-- is cap_height + descender
--
centerToBaseline :: (Fractional u, FromPtSize u) => DrawingInfo u
centerToBaseline = 
    (\ch vspan -> ch - 0.5 * vspan) <$> glyphCapHeight <*> glyphVerticalSpan

-- | All drawing is done on a spine that plots points from the 
-- first line of text. The spine is calculated from its center and
-- has to account for the inclination.
--
centerSpinePoints :: Floating u 
                  => Int -> Radian -> LocChain u
centerSpinePoints n theta
    | n <= 1    = promoteR1 $ \ctr -> return [ctr]
    | otherwise = promoteR1 $ \ctr -> 
                    baselineSpacing >>= \h1 -> 
                    let dist_top = h1 * centerCount n
                        top      = thetaNorthwards dist_top theta ctr    
                    in return (take n $ iterate (thetaSouthwards h1 theta) top)


-- | Count the steps from the center to an end:
--
-- >
-- > 1 = 0     .
-- > 2 = 0.5   .__.
-- > 3 = 1     .__.__.
-- > 4 = 1.5   .__.__.__.
-- >
--
centerCount :: Fractional u => Int -> u
centerCount i = (fromIntegral i) / 2 - 0.5
