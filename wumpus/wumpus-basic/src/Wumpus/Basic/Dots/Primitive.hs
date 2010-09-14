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
  , dotFDiamond
  , dotBDiamond 
  , dotDisk
  , dotSquare
  , dotCircle  
  , dotPentagon
  , dotStar
  , dotAsterisk
  , dotOPlus
  , dotOCross
  , dotFOCross


  ) where


import Wumpus.Basic.Graphic

import Wumpus.Core                      -- package: wumpus-core

import Data.AffineSpace                 -- package: vector-space
import Data.VectorSpace

import Control.Applicative
import Data.List

-- Marks should be the height of a lower-case letter...

-- NOTES
--
-- TikZ has both stroked and bordered (filled and outline-stroked)
-- marks e.g. square and square*
--


-- | 'polygonPoints' : @ num_points * radius * center -> [point] @ 
--
polygonPoints :: Floating u => Int -> u -> Point2 u -> [Point2 u]
polygonPoints n radius ctr = unfoldr phi (0,(pi*0.5))
  where
    theta = (pi*2) / fromIntegral n
    
    phi (i,ang) | i < n     = Just (ctr .+^ avec ang radius, (i+1,ang+theta))
                | otherwise = Nothing



-- | A dot is the height of a lowercase \'x\'.
-- 
standardSize :: FromPtSize u => (u -> LocGraphic u) -> LocGraphic u
standardSize f = \pt -> asksObj markHeight >>= \h -> f h pt

halfHeightSize :: (Fractional u, FromPtSize u) 
               => (u -> LocGraphic u) -> LocGraphic u
halfHeightSize f = \pt -> asksObj markHeight >>= \h -> f (h * 0.5) pt



shiftOrigin :: Num u => u -> u -> LocGraphic u -> LocGraphic u
shiftOrigin dx dy f = \pt -> f (displace dx dy pt)

dotChar :: (Fractional u, FromPtSize u) => Char -> LocGraphic u
dotChar ch = dotText [ch]




-- Note - eta-expanded (?)
--
dotText :: (Fractional u, FromPtSize u) => String -> LocGraphic u
dotText ss pt = asksObj (textDimensions ss) >>= \(w,h) -> 
                shiftOrigin (0.5 * (-w)) (0.5 * (-h)) (textline ss) pt





-- | Supplied point is the center.
--
axialLine :: Fractional u => Vec2 u -> LocGraphic u
axialLine v = localPoint (\ctr -> ctr .-^ (0.5 *^ v)) (straightLine v)



dotHLine :: (Fractional u, FromPtSize u) => LocGraphic u 
dotHLine = standardSize (\h -> axialLine (hvec h))
    

dotVLine :: (Fractional u, FromPtSize u) => LocGraphic u 
dotVLine = standardSize (\h -> axialLine (vvec h)) 


dotX :: (Fractional u, FromPtSize u) => LocGraphic u
dotX = standardSize (\h -> let w = 0.75 * h in
                           axialLine (vec w h) `appendAt` axialLine (vec (-w) h))



dotPlus :: (Fractional u, FromPtSize u) =>  LocGraphic u
dotPlus = dotVLine `appendAt` dotHLine


dotCross :: (Floating u, FromPtSize u) =>  LocGraphic u
dotCross = standardSize 
             (\h -> axialLine (avec ang h) `appendAt` axialLine (avec (-ang) h))
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



dotDiamond :: (Fractional u, FromPtSize u) => LocGraphic u
dotDiamond = \pt -> pathDiamond pt >>= closedStroke  

dotFDiamond :: (Fractional u, FromPtSize u) => LocGraphic u
dotFDiamond = \pt -> pathDiamond pt >>= filledPath  

dotBDiamond :: (Fractional u, FromPtSize u) => LocGraphic u
dotBDiamond = \pt -> pathDiamond pt >>= borderedPath


-- | Note disk is filled.
--
dotDisk :: (Fractional u, FromPtSize u) => LocGraphic u
dotDisk = halfHeightSize filledDisk 



dotSquare :: (Fractional u, FromPtSize u) => LocGraphic u
dotSquare = standardSize (\h -> let d = 0.5*(-h) in 
                                shiftOrigin d d $ strokedRectangle h h) 
    


dotCircle :: (Fractional u, FromPtSize u) => LocGraphic u
dotCircle = halfHeightSize strokedDisk 


dotBCircle :: (Fractional u, FromPtSize u) => LocGraphic u
dotBCircle = halfHeightSize borderedDisk 



dotPentagon :: (Floating u, FromPtSize u) => LocGraphic u
dotPentagon pt = asksObj markHeight >>= \h ->
                 closedStroke $ vertexPath $ polygonPoints 5 (0.5*h) pt

 


dotStar :: (Floating u, FromPtSize u) => LocGraphic u 
dotStar pt = asksObj markHeight >>= \h -> 
             let (p:ps) = polygonPoints 5 (0.5*h) pt in gcat (fn p) $ map fn ps
  where
    fn p1  = openStroke $ path pt [lineTo p1] 




dotAsterisk :: (Floating u, FromPtSize u) => LocGraphic u
dotAsterisk = standardSize (\h -> lineF1 h `appendAt` lineF2 h `appendAt` lineF3 h)
  where
    ang       = (pi*2) / 6
    lineF1 z  = axialLine (vvec z)
    lineF2 z  = axialLine (avec ((pi*0.5) + ang)    z)
    lineF3 z  = axialLine (avec ((pi*0.5) + ang + ang) z)



dotOPlus :: (Fractional u, FromPtSize u) => LocGraphic u
dotOPlus = dotCircle `appendAt` dotPlus


dotOCross :: (Floating u, FromPtSize u) => LocGraphic u
dotOCross = dotCircle `appendAt` dotCross


dotFOCross :: (Floating u, FromPtSize u) => LocGraphic u
dotFOCross = dotCross `appendAt` dotBCircle 


-- bkCircle :: (Fractional u, FromPtSize u) => LocGraphic u
-- bkCircle = disk (fillAttr attr) (0.5*markHeight attr) 

