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

  -- * CTM 
    CTM(..)
  , identityCTM
  , scaleCTM
  , rotateCTM
  , translateCTM
  , ctmDisplace

  -- * Shape label
  , ShapeLabel(..)
  , AddLabel(..)
  , basicLabel
  , updateText
  , drawShapeLabel
  , labelGraphic

  , DrawShape(..)

  -- * Anchors
  , AnchorCenter(..)
  , AnchorCardinal(..)
  , AnchorText(..)

  ) where

import Wumpus.Shapes.Utils

import Wumpus.Core hiding ( CTM )       -- package: wumpus-core
import Wumpus.Core.Colour ( black )

import Wumpus.Basic.Graphic             -- package: wumpus-basic

import Data.AffineSpace                 -- package: vector-space

import Control.Applicative

data CTM u = CTM 
      { ctm_pos                 :: Point2 u
      , ctm_scale_x             :: u
      , ctm_scale_y             :: u
      , ctm_rotation            :: Radian
      }
  deriving (Eq,Show)

type instance DUnit (CTM u) = u

identityCTM :: Num u => CTM u
identityCTM = CTM { ctm_pos      = P2 0 0 
                  , ctm_scale_x  = 1, ctm_scale_y = 1
                  , ctm_rotation = 0 }

scaleCTM :: Num u => u -> u -> CTM u -> CTM u
scaleCTM x1 y1 = star2 (\s x y -> s { ctm_scale_x = x1*x
                                    , ctm_scale_y = y1*y })
                       ctm_scale_x
                       ctm_scale_y

rotateCTM :: Radian -> CTM u -> CTM u
rotateCTM ang1 = star (\s ang -> s { ctm_rotation = circularModulo $ ang1+ang })
                      ctm_rotation

translateCTM :: Num u => u -> u -> CTM u -> CTM u
translateCTM x1 y1 = star (\s (P2 x y) -> s { ctm_pos = P2 (x+x1) (y+y1) })
                          ctm_pos


ctmDisplace :: (Real u, Floating u) => u -> u -> CTM u -> Point2 u
ctmDisplace x y ctm = translate cx cy $ rotate (ctm_rotation ctm) (P2 x' y')
  where
    x'        = liftA (*x) ctm_scale_x ctm
    y'        = liftA (*y) ctm_scale_y ctm
    P2 cx cy  = ctm_pos ctm


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
               => ShapeLabel -> CTM u -> Primitive u
drawShapeLabel sl ctm = rotatePrimitive ang $ textlabel (rgb,attr) text pt
  where
    (ang,ctr)         = (ctm_rotation ctm, ctm_pos ctm)        
    ((rgb,attr),text) = deconsLabel sl
    font_sz           = font_size attr
    twidth            = textWidth  font_sz (length text)
    theight           = textHeight font_sz
    pt                = let p1 = ctr .-^ V2 (0.5 * twidth) (0.5 * theight)
                        in rotateAbout ang ctr p1

-- Note - labels are not scaled ....
--
labelGraphic :: (Real u, Floating u) 
             => CTM u -> ShapeLabel -> Graphic u
labelGraphic ctm lbl = wrapG $ drawShapeLabel lbl ctm


-- can all shapes (except coordinates) be stroked and filled?

class DrawShape sh where
  strokeShape :: (Stroke t, u ~ DUnit sh) => t -> sh -> Graphic u
  fillShape   :: (Fill t, u ~ DUnit sh)   => t -> sh -> Graphic u 

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
