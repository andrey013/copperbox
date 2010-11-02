{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Shapes.Plaintext
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Plaintext is a bit like a shape but does not generate a path 
-- and cannot be scaled (it can be rotated or translated).
--
-- \*\* WARNING \*\* - the type of Plaintext is not ideal. This
-- module is pending substantial revision.
-- 
--------------------------------------------------------------------------------

module Wumpus.Basic.Shapes.Plaintext
  (

    PlaintextAnchor
  , DPlaintextAnchor
  , Plaintext
  , DPlaintext
  , LocPlaintext
  , BoxMargin(..)
  , uniformMargin
  , regularMargin
  , setMargin

  , plaintext
  , multitext
  , drawText

  ) where

import Wumpus.Basic.Anchors
import Wumpus.Basic.Graphic
import Wumpus.Basic.Shapes.Base
import Wumpus.Basic.Shapes.Derived

import Wumpus.Core                              -- package: wumpus-core

import Data.AffineSpace                         -- package: vector-space

import Control.Applicative




--------------------------------------------------------------------------------
-- Free label

-- Free label is a rectangle that /is not drawn/, the 
-- constructor should always create some text.

newtype PlaintextAnchor u = PlaintextAnchor  { getPlaintext :: Rectangle u }

type DPlaintextAnchor = PlaintextAnchor Double

type instance DUnit (PlaintextAnchor u) = u


data BoxMargin u = BoxMargin
      { margin_left     :: !u
      , margin_right    :: !u
      , margin_top      :: !u
      , margin_bottom   :: !u
      }
  deriving (Eq,Ord,Show)

data DxString u = DxString !u String
  deriving (Eq,Ord,Show)


data Plaintext u = Plaintext
      { text_ctm      :: ShapeCTM u
      , text_text     :: [DxString u]
      , text_margin   :: BoxMargin u 
      }
  deriving (Eq,Ord,Show)

type DPlaintext = Plaintext Double

type instance DUnit (Plaintext u) = u

type LocPlaintext u = Point2 u -> Plaintext u

instance (Real u, Floating u) => CenterAnchor (PlaintextAnchor u) where
  center = center . getPlaintext


instance (Real u, Floating u) => CardinalAnchor (PlaintextAnchor u) where
  north = north . getPlaintext
  south = south . getPlaintext
  east  = east . getPlaintext
  west  = west . getPlaintext

instance (Real u, Floating u) => CardinalAnchor2 (PlaintextAnchor u) where
  northeast = northeast . getPlaintext
  southeast = southeast . getPlaintext
  southwest = southwest . getPlaintext
  northwest = northwest . getPlaintext


instance (Real u, Floating u) => RadialAnchor (PlaintextAnchor u) where
  radialAnchor theta = radialAnchor theta . getPlaintext


updateCTM :: (ShapeCTM u -> ShapeCTM u) -> Plaintext u -> Plaintext u
updateCTM fn = (\s i -> s { text_ctm = fn i }) <*> text_ctm


instance (Real u, Floating u) => Rotate (Plaintext u) where
  rotate r = updateCTM (rotate r)

instance (Real u, Floating u) => RotateAbout (Plaintext u) where
  rotateAbout r pt = updateCTM (rotateAbout r pt)

-- Note scaling doe not scale the text...

instance Num u => Scale (Plaintext u) where
  scale sx sy = updateCTM (scale sx sy)


instance Num u => Translate (Plaintext u) where
  translate dx dy = updateCTM (translate dx dy)


-- Note - To be consistent with Shapes and Coordinate, the 
-- plaintext constructor should be /context-free/ regarding the 
-- DrawingCtx.
--
-- This means plaintext cannot derive a default margin based on 
-- the FontSize. In practice only Courier is likely to have an
-- acceptable default margin, Wumpus-Basic generally greatly
-- overestimates the length of text in non-monospaced fonts and
-- margins will have to be judged /by eye/.
-- 

plaintext :: Num u => String -> LocPlaintext u
plaintext ss pt = Plaintext { text_ctm    = makeShapeCTM pt
                            , text_text   = [DxString 0 ss]
                            , text_margin = zero_box_margin }



multitext :: Num u => [(u,String)] -> LocPlaintext u
multitext xs pt = 
    Plaintext { text_ctm    = makeShapeCTM pt
              , text_text   = map (uncurry DxString) xs
              , text_margin = zero_box_margin }



zero_box_margin :: Num u => BoxMargin u
zero_box_margin = BoxMargin { margin_left     = 0
                            , margin_right    = 0
                            , margin_top      = 0
                            , margin_bottom   = 0 }

uniformMargin :: (Num u, FromPtSize u) => u -> DrawingR (BoxMargin u)
uniformMargin scaling_factor = 
   (\sz -> let u = scaling_factor * sz in BoxMargin { margin_left     = u
                                                    , margin_right    = u
                                                    , margin_top      = u
                                                    , margin_bottom   = u })
    <$> monoFontPointSize

-- | 'regularMargin' : 
-- @ unit_margin * right_margin -> BoxMargin @
--
-- Create a box margin where left, top and bottom margin are all  
-- set to the unit_margin. The right_margin is an independent
-- parameter as it is used to accommodate the over-estimation of
-- textlabel widths by Wumpus - typically it will be \*negative\*.
-- 
regularMargin :: u -> u -> BoxMargin u
regularMargin um rm = BoxMargin { margin_left     = um
                                , margin_right    = rm
                                , margin_top      = um
                                , margin_bottom   = um }


setMargin :: BoxMargin u -> Plaintext u -> Plaintext u
setMargin i = (\s -> s { text_margin = i }) 


drawText :: (Real u, Floating u, FromPtSize u)
         => Plaintext u -> Image u (PlaintextAnchor u)
drawText x = intoImage (multiLineRect x) (drawMultiLines x)




multiLineRect :: (Fractional u, Ord u, FromPtSize u) 
              => Plaintext u -> DrawingR (PlaintextAnchor u)
multiLineRect (Plaintext { text_ctm=ctm, text_margin=box, text_text=xs }) = 
   (\w h -> PlaintextAnchor $ expandedRectangle box w h ctm)
    <$> maxWidth xs <*> monoMultiLineHeight (length xs)

maxWidth :: (Ord u, FromPtSize u) => [DxString u] -> DrawingR u
maxWidth xs = maximum <$> mapM lineWidth1 xs
  where
    lineWidth1 (DxString dx ss) = (\w -> w+dx) <$> monoTextLength ss                   


drawMultiLines :: (Real u, Floating u, FromPtSize u) 
               => Plaintext u -> Graphic u
drawMultiLines (Plaintext { text_ctm=ctm, text_text=xs }) = 
    let (ctr,ang) = runShapeGeom ctm $ (,) <$> shapeCenter <*> shapeAngle
        num_rows  = length xs
    in maxWidth xs                   >>= \w   ->
       baselineLefts num_rows w ctr  >>= \pts -> 
       cat $ zipWith (drawOneLine ang) xs (map (rotateAbout ang ctr) pts)
  where
    cat (y:ys) = oconcat y ys
    cat []     = error "Plaintext supplied with empty string"
  
            

drawOneLine :: (Real u, Floating u, FromPtSize u) 
            => Radian -> DxString u -> Point2 u -> Graphic u 
drawOneLine ang (DxString dx ss) bl =
    rotTextline ang ss `at` bl .+^ avec ang dx



rotTextline :: (Real u, Floating u) => Radian -> String -> LocGraphic u
rotTextline theta ss = 
    promote1 (\baseline_left -> withTextAttr $ \rgb attr -> 
                         primGraphic $ rtextlabel rgb attr ss theta baseline_left)


expandedRectangle :: Fractional u 
                  => BoxMargin u -> u -> u -> ShapeCTM u -> Rectangle u
expandedRectangle (BoxMargin { margin_left=xl, margin_right=xr
                             , margin_top=yt,  margin_bottom=yb }) w h ctm = 
    mkRectangle hw hh (translate dx dy ctm)
  where
    hw  = 0.5 * (xl + xr + w)
    hh  = 0.5 * (yt + yb + h)
    dx  = 0.5 * (xr - xl)
    dy  = 0.5 * (yt - yb)
    





baselineLefts :: (Fractional u, FromPtSize u)  
                 => Int -> u -> Point2 u -> DrawingR [Point2 u]
baselineLefts num_rows max_width ctr = 
   (\rect_height ptsize spacing descender_depth -> 
        let dy = 0.5 * rect_height - 0.5 * ptsize
            v1 = vec (0.5 * max_width) (dy + descender_depth)
        in headIterate num_rows (.+^ vvec spacing) (ctr .-^ v1) )
      <$> monoMultiLineHeight num_rows 
                <*> monoFontPointSize <*> baselineSpacing <*> monoDescenderDepth



headIterate :: Int -> (a -> a) -> a -> [a]
headIterate n f a = step 0 a []
  where
    step i x xs | i < n = step (i+1) (f x) (x:xs)
    step _ _ xs         = xs

