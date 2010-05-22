{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Extra.Shape.Base
-- Copyright   :  (c) Stephen Tetley 2009-2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC with TypeFamilies and more
--
-- Common core for shapes (anchors...)
-- 
--------------------------------------------------------------------------------

module Wumpus.Extra.Shape.Base
  ( 

  -- * Current transformation matrix
    CTM
  , translateCTM
  , scaleCTM
  , rotateCTM
  , rotateAboutCTM

  -- * Composite
  , Composite(..)
  , labelledComposite

  -- * Shape label
  , ShapeLabel(..)
  , drawShapeLabel

  -- * Anchors
  , AnchorCenter(..)
  , AnchorCardinal(..)


  ) where

import Wumpus.Core hiding ( CTM )

import Data.AffineSpace         -- package: vector-space


--------------------------------------------------------------------------------
-- Shapes will generally include a translation matrix...

type CTM u = Matrix3'3 u

translateCTM :: Num u => u -> u -> CTM u -> CTM u
translateCTM x y m = translationMatrix x y * m

scaleCTM :: Num u => u -> u -> CTM u -> CTM u
scaleCTM x y m = scalingMatrix x y * m

rotateCTM       :: (Floating u, Real u) => Radian -> CTM u -> CTM u
rotateCTM r m   = rotationMatrix r * m

rotateAboutCTM  :: (Floating u, Real u) => Radian -> Point2 u -> CTM u -> CTM u
rotateAboutCTM r pt m = originatedRotationMatrix r pt * m




--------------------------------------------------------------------------------

newtype Composite u = Composite { getPrimitives :: [Primitive u] }


labelledComposite :: (Fractional u, Ord u) 
                  => CTM u -> Point2 u -> Maybe ShapeLabel -> Primitive u -> Composite u
labelledComposite ctm ctr mb_lbl prim = maybe (Composite [prim]) sk mb_lbl 
  where
    sk lbl = let lbl_prim = drawShapeLabel lbl (ctm *# ctr)
             -- note lbl_prim needs the CTM applying ...
             -- Looks like wumpus-core needs extending with
             -- a 'matrix-apply' affine transformation
             in Composite [ lbl_prim, prim ]
 


data ShapeLabel = ShapeLabel
      { shapelabel_text         :: String
      , shapelabel_font_props   :: FontAttr
      , shapelabel_font_colour  :: PSRgb
      }


drawShapeLabel :: (Fractional u, Ord u) => ShapeLabel -> Point2 u -> Primitive u
drawShapeLabel sl ctr = textlabel attr (shapelabel_text sl) pt
  where
    attr     = (shapelabel_font_colour sl, shapelabel_font_props sl)
    font_sz  = font_size $ shapelabel_font_props sl
    text     = shapelabel_text sl
    bb       = textBounds font_sz zeroPt (length text)
    V2 w2 h2 = ur_corner bb .-. ll_corner bb
    pt       = ctr .-^ V2 (w2 / 2) (h2 / 2)


--------------------------------------------------------------------------------
-- Anchors

class AnchorCenter a where
  center :: DUnit a ~ u => a -> Point2 u

class AnchorCardinal a where
  north :: DUnit a ~ u => a -> Point2 u
  south :: DUnit a ~ u => a -> Point2 u
  east  :: DUnit a ~ u => a -> Point2 u
  west  :: DUnit a ~ u => a -> Point2 u

  northeast :: DUnit a ~ u => a -> Point2 u
  southeast :: DUnit a ~ u => a -> Point2 u
  southwest :: DUnit a ~ u => a -> Point2 u
  northwest :: DUnit a ~ u => a -> Point2 u

