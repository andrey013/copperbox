{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Shapes.Base
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC with TypeFamilies and more
--
-- Common core for shapes (anchors...)
-- 
--------------------------------------------------------------------------------

module Wumpus.Shapes.Base
  ( 

  -- * Shape label
    ShapeLabel(..)
  , AddLabel(..)
  , basicLabel
  , updateText
  , drawShapeLabel
  , DrawShape(..)

  -- * Anchors
  , AnchorCenter(..)
  , AnchorCardinal(..)
  , AnchorText(..)

  ) where

import Wumpus.Core                      -- package: wumpus-core
import Wumpus.Core.Colour ( black )

import Wumpus.Basic.Graphic             -- package: wumpus-basic

import Data.AffineSpace                 -- package: vector-space

data ShapeLabel = ShapeLabel
      { shapelabel_text         :: String
      , shapelabel_font_props   :: FontAttr
      , shapelabel_font_colour  :: DRGB
      }
  deriving (Eq,Show)

basicLabel :: String -> ShapeLabel
basicLabel text = ShapeLabel text wumpus_default_font black

updateText :: String -> ShapeLabel -> ShapeLabel
updateText text s = s { shapelabel_text = text } 

deconsLabel :: ShapeLabel -> ((DRGB,FontAttr), String) 
deconsLabel (ShapeLabel ss fa rgb) = ((rgb,fa),ss)

drawShapeLabel :: (Real u, Floating u)
               => ShapeLabel -> Point2 u -> Radian -> Primitive u
drawShapeLabel sl ctr ang = rotatePrimitive ang $ textlabel (rgb,attr) text pt
  where
    ((rgb,attr),text) = deconsLabel sl
    font_sz           = font_size attr
    twidth            = textWidth  font_sz (length text)
    theight           = textHeight font_sz
    pt                = let p1 = ctr .-^ V2 (0.5 * twidth) (0.5 * theight)
                        in rotateAbout ang ctr p1


-- can all shapes (except coordinates) be stroked and filled?

class DrawShape sh where
  strokeShape :: (Stroke t, Real u, Floating u) => t -> sh u -> Graphic u
  fillShape   :: (Fill t, Real u, Floating u)   => t -> sh u -> Graphic u 

-- yuck...
--
class AddLabel t where
  addLabel :: t -> String -> t

--------------------------------------------------------------------------------
-- Anchors

class AnchorCenter t where
  center :: DUnit t ~ u => t -> Point2 u

class AnchorCardinal t where
  north :: DUnit t ~ u => t -> Point2 u
  south :: DUnit t ~ u => t -> Point2 u
  east  :: DUnit t ~ u => t -> Point2 u
  west  :: DUnit t ~ u => t -> Point2 u

  northeast :: DUnit t ~ u => t -> Point2 u
  southeast :: DUnit t ~ u => t -> Point2 u
  southwest :: DUnit t ~ u => t -> Point2 u
  northwest :: DUnit t ~ u => t -> Point2 u




class AnchorText t where
  textAnchor :: DUnit t ~ u => t -> Point2 u

