{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Shapes.Derived
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- 
-- 
--------------------------------------------------------------------------------

module Wumpus.Basic.Shapes.Derived
  ( 
    Rectangle
  , rectangle

  , Circle
  , circle

  ) where

import Wumpus.Basic.Anchors
import Wumpus.Basic.Graphic
import Wumpus.Basic.Shapes.Base


import Wumpus.Core                      -- package: wumpus-core


-- import Control.Applicative


--------------------------------------------------------------------------------
-- Rectangles

data Rectangle u = Rectangle 
      { rect_ctm    :: ShapeCTM u
      , rect_hw     :: !u
      , rect_hh     :: !u 
      }
  deriving (Eq,Show)

-- Not deriving much value from Shape...

-- type SRectangle u = SRectangle { getRectangle :: Shape Rectangle u }

type instance DUnit (Rectangle u) = u


instance (Real u, Floating u) => CenterAnchor (Rectangle u) where
  center = ctmCenter . rect_ctm


calcRectPoint :: (Real u, Floating u) 
              => (u -> u -> Point2 u) -> Rectangle u -> Point2 u
calcRectPoint f (Rectangle { rect_ctm=ctm, rect_hw = hw, rect_hh = hh }) =
    let pt = f hw hh in ctmDisplace pt ctm

instance (Real u, Floating u) => CardinalAnchor (Rectangle u) where
  north = calcRectPoint $ \ _  hh -> P2 0 hh
  south = calcRectPoint $ \ _  hh -> P2 0 (-hh)
  east  = calcRectPoint $ \ hw _  -> P2 hw 0
  west  = calcRectPoint $ \ hw _  -> P2 (-hw) 0

instance (Real u, Floating u) => CardinalAnchor2 (Rectangle u) where
  northeast = calcRectPoint $ \ hw hh -> P2 hw hh
  southeast = calcRectPoint $ \ hw hh -> P2 hw (-hh)
  southwest = calcRectPoint $ \ hw hh -> P2 (-hw) (-hh)
  northwest = calcRectPoint $ \ hw hh -> P2 (-hw) hh


rectangle :: (Floating u, Real u) => u -> u -> Shape u (Rectangle u)
rectangle hw hh = Shape { src_ctm = identityCTM
                        , out_fun = outputRect hw hh
                        }


outputRect :: (Real u, Floating u) 
           => u -> u -> ShapeCTM u -> Image u (Rectangle u)
outputRect hw hh ctm = intoImage (pureDF rect) (drawRect rect) 
  where
    rect = Rectangle { rect_ctm = ctm, rect_hw = hw, rect_hh = hh }



-- Ideally don\'t want to be able to (re-)draw a SRect only take 
-- anchor points from it...
--
-- Does this matter? Yes and its solved!
--


drawRect :: (Real u, Floating u) => Rectangle u -> Graphic u
drawRect = borderedPath . rectPath


rectPath :: (Real u, Floating u) => Rectangle u -> PrimPath u
rectPath = vertexPath . extractVertexPoints

extractVertexPoints :: (Real u, Floating u) => Rectangle u -> [Point2 u]
extractVertexPoints rect = [bl,br,tr,tl]
  where
    bl        = southwest rect
    tr        = northeast rect
    br        = southeast rect
    tl        = northwest rect




--------------------------------------------------------------------------------


data Circle u = Circle 
      { circ_ctm    :: ShapeCTM u
      , circ_radius :: !u 
      }
  deriving (Eq,Show)
  


type instance DUnit (Circle u) = u

instance (Real u, Floating u) => CenterAnchor (Circle u) where
  center = ctmCenter . circ_ctm


circle :: (Floating u, Real u) => u -> Shape u (Circle u)
circle radius = Shape { src_ctm = identityCTM
                      , out_fun = outputCirc radius
                      }


outputCirc :: (Real u, Floating u) 
           => u -> ShapeCTM u -> Image u (Circle u)
outputCirc rad ctm = intoImage (pureDF rect) (drawCirc rect) 
  where
    rect = Circle { circ_ctm = ctm, circ_radius = rad }


drawCirc :: (Real u, Floating u) => Circle u -> Graphic u
drawCirc = borderedPath . circlePath


circlePath :: (Real u, Floating u) => Circle u -> PrimPath u
circlePath = curvedPath . circlePoints 

circlePoints :: (Real u, Floating u) => Circle u -> [Point2 u]
circlePoints (Circle { circ_ctm=ctm, circ_radius=radius }) = map fn all_points
  where
    fn pt       = ctmDisplace pt ctm
    all_points  = bezierCircle 2 radius zeroPt 
