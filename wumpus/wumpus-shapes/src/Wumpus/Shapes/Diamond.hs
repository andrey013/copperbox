{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Shapes.Diamond
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC with TypeFamilies and more
--
-- Diamond
-- 
--------------------------------------------------------------------------------

module Wumpus.Shapes.Diamond
  ( 

    Diamond(..)
  , DDiamond
  , diamond
  , diamond_  

  ) where

import Wumpus.Shapes.Base 
import Wumpus.Shapes.Utils

import Wumpus.Core                              -- package: wumpus-core
import Wumpus.Basic.Anchors                     -- package: wumpus-basic
import Wumpus.Basic.Graphic
import Wumpus.Basic.Graphic.DrawingAttr
import Wumpus.Basic.Monads.Drawing

import Data.AffineSpace                         -- package: vector-space
import Data.VectorSpace

-- | Diamond.
--
data Diamond u = Diamond 
      { dia_half_width    :: u
      , dia_half_height   :: u
      , dia_ctm           :: CTM u
      , dia_label         :: Maybe ShapeLabel
      }

type DDiamond = Diamond Double

type instance DUnit (Diamond u) = u

-- CTM * ctr * half_width * half_height      


-- CTM * half_width * half_height      
--
withGeom :: Num u => (CTM u -> u -> u -> a) -> Diamond u -> a
withGeom f (Diamond { dia_ctm=ctm, dia_half_width=hw, dia_half_height=hh }) =
    f ctm hw hh
     
calcPoint :: (Real u, Floating u) => (u -> u -> Vec2 u) -> Diamond u -> Point2 u
calcPoint f = withGeom $ \ctm hw hh -> 
    let (V2 x y) = f hw hh in ctmDisplace x y ctm


--------------------------------------------------------------------------------
-- Instances 
  

instance (Real u, Floating u) => CenterAnchor (Diamond u) where
  center = ctmCenter . dia_ctm



midpoint :: Fractional u 
         => (Diamond u -> Point2 u) 
         -> (Diamond u -> Point2 u) 
         -> Diamond u -> Point2 u
midpoint f g d = a .+^ v
  where
    a = f d
    b = g d
    v = (b .-. a) ^* 0.5


instance (Real u, Floating u) =>  CardinalAnchor (Diamond u) where
  north = calcPoint $ \ _  hh -> vvec hh
  south = calcPoint $ \ _  hh -> vvec (-hh)
  east  = calcPoint $ \ hw _  -> hvec hw
  west  = calcPoint $ \ hw _  -> hvec (-hw)

instance (Real u, Floating u) =>  CardinalAnchor2 (Diamond u) where
  northeast = midpoint north east
  southeast = midpoint south east
  southwest = midpoint south west
  northwest = midpoint north west


-- helper
updateCTM :: (CTM u -> CTM u) -> Diamond u -> Diamond u
updateCTM f = star (\s m -> s { dia_ctm = f m } ) dia_ctm

instance (Floating u, Real u) => Rotate (Diamond u) where
  rotate r = updateCTM (rotateCTM r) 

instance Num u => Scale (Diamond u) where
  scale x y = updateCTM (scaleCTM x y)

instance Num u => Translate (Diamond u) where
  translate x y = updateCTM (translateCTM x y)


--------------------------------------------------------------------------------
-- Construction

-- | @rectangle : width * height * center_pt -> rectangle@
--
diamond :: Fractional u => u -> u -> Diamond u
diamond w h = Diamond (w*0.5) (h*0.5) identityCTM Nothing

diamond_ :: Fractional u => u -> u -> String -> Diamond u
diamond_ w h str = (diamond w h) { dia_label = Just $ ShapeLabel str }

--------------------------------------------------------------------------------


strokeD :: (Real u, Floating u)
        => DrawingAttr -> Point2 u -> Diamond u -> Graphic u
strokeD attr (P2 x y) = 
    wrapG . cstroke (strokeAttr attr) . diamondPath . translate x y 


fillD :: (Real u, Floating u) 
      => DrawingAttr -> Point2 u -> Diamond u -> Graphic u
fillD attr (P2 x y) = 
    wrapG . fill (fillAttr attr) . diamondPath . translate x y

textD :: (Real u, Floating u, FromPtSize u) 
      => DrawingAttr -> Point2 u -> Diamond u -> Graphic u
textD attr (P2 x y) dia = maybe id sk $ dia_label dia
  where
    ctm      = dia_ctm $ translate x y dia
    sk label = labelGraphic label (textAttr attr) ctm 


make :: (Real u, Floating u) 
     => DrawingAttr -> Point2 u -> Diamond u -> Diamond u
make _ (P2 x y) = translate x y
 


instance (Real u, Floating u, FromPtSize u) => Draw (Diamond u) where
  draw dia = AGraphic df (\a p -> make a p dia)
    where
      df attr pt = textD attr pt dia . strokeD attr pt dia
                                     . fillD   attr pt dia


--
 

diamondPath :: (Real u, Floating u) => Diamond u -> PrimPath u
diamondPath = vertexPath . extractVertexPoints

extractVertexPoints :: (Real u, Floating u) => Diamond u -> [Point2 u]
extractVertexPoints = sequence [south,east,north,west]

