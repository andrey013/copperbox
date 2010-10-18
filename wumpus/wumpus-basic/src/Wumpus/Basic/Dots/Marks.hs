{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Dots.Marks
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Marks - dots without anchor handles.
--
-- \*\* WARNING \*\* - names are expected to change - filled and
-- background-filled marks need a naming convention.
-- 
--------------------------------------------------------------------------------

module Wumpus.Basic.Dots.Marks
  ( 


  -- * Marks
    markChar
  , markText

  , markHLine
  , markVLine
  , markX
  , markPlus
  , markCross
  , markDiamond
  , markFDiamond
  , markBDiamond 
  , markDisk
  , markSquare
  , markCircle  
  , markPentagon
  , markStar
  , markAsterisk
  , markOPlus
  , markOCross
  , markFOCross


  ) where


import Wumpus.Basic.Graphic
import Wumpus.Basic.Utils.Combinators

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




shiftOrigin :: Num u => u -> u -> LocGraphic u -> LocGraphic u
shiftOrigin dx dy f = \pt -> f (displace dx dy pt)

markChar :: (Fractional u, Ord u, FromPtSize u) => Char -> LocGraphic u
markChar ch = markText [ch]




markText :: (Fractional u, Ord u, FromPtSize u) => String -> LocGraphic u
markText ss = centermonoTextline ss





-- | Supplied point is the center.
--
axialLine :: Fractional u => Vec2 u -> LocGraphic u
axialLine v = localPoint (\ctr -> ctr .-^ (0.5 *^ v)) (straightLine v)


markHLine :: (Fractional u, FromPtSize u) => LocGraphic u 
markHLine = bindAsk markHeight $ \h -> axialLine (hvec h)


markVLine :: (Fractional u, FromPtSize u) => LocGraphic u 
markVLine = bindAsk markHeight $ \h -> axialLine (vvec h) 


markX :: (Fractional u, FromPtSize u) => LocGraphic u
markX = bindAsk markHeight $ \h -> 
    let w = 0.75 * h in axialLine (vec w h) `oplus` axialLine (vec (-w) h)



markPlus :: (Fractional u, FromPtSize u) =>  LocGraphic u
markPlus = markVLine `oplus` markHLine


markCross :: (Floating u, FromPtSize u) =>  LocGraphic u
markCross = bindAsk markHeight $ \h ->  
    (axialLine $ avec ang h) `oplus` (axialLine $ avec (-ang) h)
  where
    ang = pi*0.25  

-- needs horizontal pinch...

pathDiamond :: (Fractional u, FromPtSize u) 
            => Point2 u -> DrawingR (PrimPath u)
pathDiamond pt = (\h -> let hh    = 0.66 * h; hw = 0.5 * h 
                        in vertexPath [dvs hh, dve hw,dvn hh, dvw hw])
                   <$> markHeight
  where
    dvs hh = pt .+^ vvec (-hh)
    dve hw = pt .+^ hvec hw
    dvn hh = pt .+^ vvec hh
    dvw hw = pt .+^ hvec (-hw)



markDiamond :: (Fractional u, FromPtSize u) => LocGraphic u
markDiamond = pathDiamond `bindInto` closedStroke  

markFDiamond :: (Fractional u, FromPtSize u) => LocGraphic u
markFDiamond = pathDiamond `bindInto` filledPath


-- Note - the (const . fn) composition doesn\'t /tell/ much about
-- what is going on - though obviously it can be decoded - make 
-- the function obvious to the second argument. 
-- 
-- A named combinator might be better.
--

markBDiamond :: (Fractional u, FromPtSize u) => LocGraphic u
markBDiamond = pathDiamond `bindInto` borderedPath


-- | Note disk is filled.
--
markDisk :: (Fractional u, FromPtSize u) => LocGraphic u
markDisk = bindAsk markHalfHeight filledDisk 



markSquare :: (Fractional u, FromPtSize u) => LocGraphic u
markSquare = bindAsk markHeight $ \h -> 
    let d = 0.5*(-h) in shiftOrigin d d $ strokedRectangle h h
    


markCircle :: (Fractional u, FromPtSize u) => LocGraphic u
markCircle = bindAsk markHalfHeight strokedDisk 


markBCircle :: (Fractional u, FromPtSize u) => LocGraphic u
markBCircle = bindAsk markHalfHeight borderedDisk 



markPentagon :: (Floating u, FromPtSize u) => LocGraphic u
markPentagon = bindAsk markHeight $ \h ->
    closedStroke . vertexPath . polygonPoints 5 (0.5*h)

 


markStar :: (Floating u, FromPtSize u) => LocGraphic u 
markStar pt = markHeight >>= \h -> 
              let ps = polygonPoints 5 (0.5*h) pt in step $ map fn ps
  where
    fn p1       = openStroke $ path pt [lineTo p1] 
    step (x:xs) = oconcat x xs
    step _      = error "markStar - unreachable"


-- Note - relies on the functional instance of OPlus

markAsterisk :: (Floating u, FromPtSize u) => LocGraphic u
markAsterisk = bindAsk markHeight $ \h -> 
    lineF1 h `oplus` lineF2 h `oplus` lineF3 h
  where
    ang       = (pi*2) / 6
    lineF1 z  = axialLine (vvec z)
    lineF2 z  = axialLine (avec ((pi*0.5) + ang)    z)
    lineF3 z  = axialLine (avec ((pi*0.5) + ang + ang) z)



markOPlus :: (Fractional u, FromPtSize u) => LocGraphic u
markOPlus = markCircle `oplus` markPlus


markOCross :: (Floating u, FromPtSize u) => LocGraphic u
markOCross = markCircle `oplus` markCross


markFOCross :: (Floating u, FromPtSize u) => LocGraphic u
markFOCross = markCross `oplus` markBCircle 


-- bkCircle :: (Fractional u, FromPtSize u) => LocGraphic u
-- bkCircle = disk (fillAttr attr) (0.5*markHeight attr) 

