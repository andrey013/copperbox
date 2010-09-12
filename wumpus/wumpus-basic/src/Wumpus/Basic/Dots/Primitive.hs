{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Dots.Primitive
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC with TypeFamilies and more
--
-- Dots
-- 
--------------------------------------------------------------------------------

module Wumpus.Basic.Dots.Primitive
  ( 


  -- * Dots
    dotChar
  , dotText

  , dotHLine
  , dotVLine
  , dotX
  , dotPlus
  , dotCross
  , dotDiamond
  , dotBDiamond 
  , dotDisk
  , dotSquare
  , dotCircle
{-  
  , dotPentagon
  , dotStar
-}
  , dotAsterisk
  , dotOPlus
  , dotOCross
  , dotFOCross


  ) where


import Wumpus.Basic.Graphic.DrawingContext
import Wumpus.Basic.Graphic.Graphic
import Wumpus.Basic.Graphic.Image
import Wumpus.Basic.Graphic.PointSupply ( polygonPointsV )

import Wumpus.Core                      -- package: wumpus-core

import Data.AffineSpace                 -- package: vector-space
import Data.VectorSpace

import Control.Applicative

-- Marks should be the height of a lower-case letter...

-- NOTES
--
-- TikZ has both stroked and bordered (filled and outline-stroked)
-- marks e.g. square and square*
--


-- | Composition operator...
--
--
cc :: LocImage u -> LocImage u -> LocImage u
cc f g = \pt -> f pt `appendImage` g pt



-- | A dot is the height of a lowercase \'x\'.
-- 
standardSize :: FromPtSize u => (u -> LocImage u) -> LocImage u
standardSize f = \pt -> asksObj markHeight >>= \h -> f h pt

halfHeightSize :: (Fractional u, FromPtSize u) 
               => (u -> LocImage u) -> LocImage u
halfHeightSize f = \pt -> asksObj markHeight >>= \h -> f (h * 0.5) pt



shiftOrigin :: Num u => u -> u -> LocImage u -> LocImage u
shiftOrigin dx dy f = \pt -> f (displace dx dy pt)

dotChar :: (Fractional u, FromPtSize u) => Char -> LocImage u
dotChar ch = dotText [ch]




-- Note - eta-expanded (?)
--
dotText :: (Fractional u, FromPtSize u) => String -> LocImage u
dotText ss pt = asksObj (textDimensions ss) >>= \(w,h) -> 
                shiftOrigin (0.5 * (-w)) (0.5 * (-h)) (textline ss) pt





-- | Supplied point is the center.
--
axialLine :: Fractional u => Vec2 u -> LocImage u
axialLine v = localPoint (\ctr -> ctr .-^ (0.5 *^ v)) (straightLine v)



dotHLine :: (Fractional u, FromPtSize u) => LocImage u 
dotHLine = standardSize (\h -> axialLine (hvec h))
    

dotVLine :: (Fractional u, FromPtSize u) => LocImage u 
dotVLine = standardSize (\h -> axialLine (vvec h)) 


dotX :: (Fractional u, FromPtSize u) => LocImage u
dotX = standardSize (\h -> let w = 0.75 * h in
                           axialLine (vec w h) `cc` axialLine (vec (-w) h))



dotPlus :: (Fractional u, FromPtSize u) =>  LocImage u
dotPlus = dotVLine `cc` dotHLine


dotCross :: (Floating u, FromPtSize u) =>  LocImage u
dotCross = standardSize 
             (\h -> axialLine (avec ang h) `cc` axialLine (avec (-ang) h))
  where
    ang = pi*0.25  



-- needs horizontal pinch...

pathDiamond :: (Fractional u, FromPtSize u) 
            => Point2 u -> DrawingObject (PrimPath u)
pathDiamond pt = (\h -> let hh    = 0.66 * h; hw = 0.5 * h 
                        in vertexPath [dvs hh, dve hw,dvn hh, dvw hw])
                   <$> asksObj markHeight
  where
    dvs hh = pt .+^ vvec (-hh)
    dve hw = pt .+^ hvec hw
    dvn hh = pt .+^ vvec hh
    dvw hw = pt .+^ hvec (-hw)



dotDiamond :: (Fractional u, FromPtSize u) => LocImage u
dotDiamond = \pt -> pathDiamond pt >>= closedStroke  


dotBDiamond :: (Fractional u, FromPtSize u) => LocImage u
dotBDiamond = \pt -> pathDiamond pt >>= borderedPath


-- | Note disk is filled.
--
dotDisk :: (Fractional u, FromPtSize u) => LocImage u
dotDisk = halfHeightSize filledDisk 



dotSquare :: (Fractional u, FromPtSize u) => LocImage u
dotSquare = standardSize (\h -> let d = 0.5*(-h) in 
                                shiftOrigin d d $ strokedRectangle h h) 
    


dotCircle :: (Fractional u, FromPtSize u) => LocImage u
dotCircle = standardSize strokedDisk 


dotBCircle :: (Fractional u, FromPtSize u) => LocImage u
dotBCircle = standardSize borderedDisk 


{-
dotPentagon :: (Floating u, FromPtSize u) => LocImage u
dotPentagon attr = 
    wrapG . cstroke (stroke_colour attr) (stroke_props attr) 
          . vertexPath . polygonPointsV 5 hh
  where
    hh      = 0.5 * markHeight attr

-} 

{-
dotStar :: (Floating u, FromPtSize u) => LocImage u 
dotStar pt = asksObj markHeight >>= \h -> 
             let ps = polygonPointsV 5 (h * 0.5) pt in step ps
  where
    fn p1 p2  = openStroke $ path p1 [lineTo p2] 

-}


dotAsterisk :: (Floating u, FromPtSize u) => LocImage u
dotAsterisk = standardSize (\h -> lineF1 h `cc` lineF2 h `cc` lineF3 h)
  where
    ang       = (pi*2) / 6
    lineF1 z  = axialLine (vvec z)
    lineF2 z  = axialLine (avec ((pi*0.5) + ang)    z)
    lineF3 z  = axialLine (avec ((pi*0.5) + ang + ang) z)



dotOPlus :: (Fractional u, FromPtSize u) => LocImage u
dotOPlus = dotCircle `cc` dotPlus


dotOCross :: (Floating u, FromPtSize u) => LocImage u
dotOCross = dotCircle `cc` dotCross


dotFOCross :: (Floating u, FromPtSize u) => LocImage u
dotFOCross = dotCross `cc` dotBCircle 


-- bkCircle :: (Fractional u, FromPtSize u) => LocImage u
-- bkCircle = disk (fillAttr attr) (0.5*markHeight attr) 

