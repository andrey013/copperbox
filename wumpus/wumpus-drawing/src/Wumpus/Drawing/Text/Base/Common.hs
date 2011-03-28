{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Drawing.Text.Base.Common
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

module Wumpus.Drawing.Text.Base.Common
  ( 

    makeBoundedPosImage
  , emptyBoundedPosImage
  , advtext
  , textVector
  , textOPosZero

  , charVector
  , charOPosZero
  , hkernVector
  , hkernOPosZero

  , multilineHeight
  , borderedTextPos
  , borderedRotTextPos

  , centerToBaseline
  , centerSpineDisps

  ) where


import Wumpus.Basic.Kernel                      -- package: wumpus-basic

import Wumpus.Core                              -- package: wumpus-core
import Wumpus.Core.Text.GlyphIndices

import Data.VectorSpace                         -- package: vector-space

import Control.Applicative
import Data.Char
import qualified Data.Map               as Map
import Data.Maybe 


-- | Most text objects are @PosImage BoundingBox u@.
--
-- This builds them.
--
makeBoundedPosImage :: Num u 
                    => ObjectPos u 
                    -> LocImage t u 
                    -> PosImage BoundingBox u
makeBoundedPosImage opos gf = makePosImage opos body
  where
    body = promoteR1 $ \pt -> 
           let bb = objectPosBoundsLocus opos pt
           in replaceAns bb $ gf `at` pt


emptyBoundedPosImage :: InterpretUnit u => PosImage BoundingBox u
emptyBoundedPosImage = makeBoundedPosImage (ObjectPos 0 0 0 0) emptyLocGraphic

-- | Single line text, returning its advance vector.
--
advtext :: InterpretUnit u => EscapedText -> AdvGraphic u
advtext esc = lift0R1 (textVector esc) >>= body
  where
    body v = replaceAns v $ escTextLine esc


textVector :: InterpretUnit u => EscapedText -> Query (AdvanceVec u)
textVector esc = 
    cwLookupTable >>= \table -> 
    pointSize    >>= \sz    -> 
    let cs = destrEscapedText id esc 
    in return $ foldr (step sz table) (vec 0 0) cs
  where
    step sz table ch v = (v ^+^) $ charWidth sz table ch

charVector :: InterpretUnit u => EscapedChar -> Query (AdvanceVec u)
charVector ch = 
    (\table sz -> charWidth sz table ch) <$> cwLookupTable <*> pointSize

-- | Build the ObjectPos of a single line of EscapedText.
-- 
-- The locus of the ObjectPos is baseline left - margins are 
-- added.
--
textOPosZero :: InterpretUnit u => EscapedText -> Query (ObjectPos u)
textOPosZero esc = textVector esc >>= bllOPosZero

-- | Build the ObjectPos of an EscapedChar.
-- 
-- The locus of the ObjectPos is baseline left - margins are 
-- added.
--
charOPosZero :: InterpretUnit u => EscapedChar -> Query (ObjectPos u)
charOPosZero ch = charVector ch >>= bllOPosZero 


bllOPosZero :: InterpretUnit u => AdvanceVec u -> Query (ObjectPos u)
bllOPosZero (V2 w _) = 
    (\(xsep,ysep) ymin ymaj -> ObjectPos xsep (w+xsep) (ymin+ysep) (ymaj+ysep))
      <$> textMargin <*> capHeight <*> descender


   
-- | 'hkernVector' : @ [kerning_char] -> AdvanceVec @
-- 
-- 'hkernvector' takes whatever length is paired with the 
-- EscapedChar for the init of the the list, for the last element 
-- it takes the charVector.
--
hkernVector :: InterpretUnit u => [KernChar u] -> Query (AdvanceVec u)
hkernVector = go 0
  where
    go w []             = return $ V2 w 0
    go w [(_, ch)]      = fmap (addWidth w) (charVector ch)
    go w ((dx,_ ):xs)   = go (w + dx) xs
    
    addWidth w (V2 x y) = V2 (w+x) y


hkernOPosZero :: InterpretUnit u => [KernChar u] -> Query (ObjectPos u)
hkernOPosZero xs = hkernVector xs >>= bllOPosZero
 
-- | This is outside the Drawing context as we don\'t want to get
-- the @cwLookupTable@ for every char.
--
charWidth :: InterpretUnit u 
          => FontSize -> CharWidthLookup -> EscapedChar -> AdvanceVec u
charWidth sz fn (CharLiteral c) = fmap (dinterp sz) $ fn $ ord c
charWidth sz fn (CharEscInt i)  = fmap (dinterp sz) $ fn i
charWidth sz fn (CharEscName s) = fmap (dinterp sz) $ fn ix
  where
    ix = fromMaybe (-1) $ Map.lookup s ps_glyph_indices



--------------------------------------------------------------------------------
-- Measurement and start points for multiline text


-- | Height of multiline text is cap_height to descender for the 
-- first line, then baseline-to-baseline span for the remaining
-- lines.
--
multilineHeight :: (Real u, Floating u, InterpretUnit u) 
                => Int -> Query u
multilineHeight line_count 
    | line_count < 1  = pure 0
    | line_count == 1 = verticalSpan
    | otherwise       = fn <$> verticalSpan <*> baselineSpacing
  where
    fn h1 bspan = let rest_spans = bspan * fromIntegral (line_count - 1)
                  in h1 + rest_spans



-- | Variant of 'textObjectPos' where the calculation includes
-- margins around all four sides of the enclosing rectangle.
--
-- Margin sizes are taken from the 'text_margin' field in the 
-- 'DrawingContext'.
--
borderedTextPos :: (Real u, Floating u, InterpretUnit u) 
                => Int -> u -> Query (ObjectPos u)
borderedTextPos line_count w =
    multilineHeight line_count >>= \h ->
    textMargin                 >>= \(xsep,ysep) -> 
    let hw    = xsep + (0.5 * w)
        hh    = ysep + (0.5 * h)
    in return $ ObjectPos hw hw hh hh 





-- | LR text needs the objectPos under rotation.
--
borderedRotTextPos :: (Real u, Floating u, InterpretUnit u) 
             => Radian -> Int -> u -> Query (ObjectPos u)
borderedRotTextPos theta line_count max_w =
    borderedTextPos line_count max_w >>= orthoObjectPos theta


-- | Note - this returns the answer in center form, regardless
-- of whether the input was in center form.
-- 
-- So it is probably not a general enough function for the 
-- PosImage library.
--
orthoObjectPos :: (Real u, Floating u, InterpretUnit u) 
               => Radian -> ObjectPos u -> Query (ObjectPos u)
orthoObjectPos ang opos = 
    (\sz -> intraMapFunctor sz (dblOrthoObjectPos ang) opos) 
      <$> pointSize


dblOrthoObjectPos :: Radian -> ObjectPos Double -> ObjectPos Double
dblOrthoObjectPos theta (ObjectPos xmin xmaj ymin ymaj) = 
    ObjectPos bbox_hw bbox_hw bbox_hh bbox_hh
  where
    bbox0     = BBox (P2 (-input_hw) (-input_hh)) (P2 input_hw input_hh)
    bbox1     = retraceBoundary id $ drotateAbout theta (zeroPt::DPoint2) bbox0
    bbox_hw   = 0.5 * (boundaryWidth  bbox1)
    bbox_hh   = 0.5 * (boundaryHeight bbox1)
    input_hw  = 0.5 * (xmin + xmaj)
    input_hh  = 0.5 * (ymin + ymaj)
    



-- | Calculate the distance from the center of a one-line textbox 
-- to the baseline. Note the height of a textbox is @vspan@ which 
-- is cap_height + descender
--
centerToBaseline :: (Fractional u, InterpretUnit u) => Query u
centerToBaseline = 
    (\ch vspan -> ch - 0.5 * vspan) <$> capHeight <*> verticalSpan

-- | All drawing is done on a spine that plots points from the 
-- first line of text. The spine is calculated from its center and
-- has to account for the inclination.
--
centerSpineDisps :: Floating u 
                 => Int -> Radian -> Query (PointDisplace u, PointDisplace u)
centerSpineDisps n theta =  
    baselineSpacing >>= \h1 -> 
    let dist_top    = h1 * centerCount n
        vec_to_top  = thetaNorthwards dist_top theta
    in return (vec_to_top, thetaSouthwards h1 theta)


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


