{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Drawing.Dots.Marks
-- Copyright   :  (c) Stephen Tetley 2010-2011
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Marks - dots without anchor handles.
--
-- The text and char marks need loaded glyph metrics for proper 
-- centering. 
--
-- \*\* WARNING \*\* - names are expected to change - filled and
-- background-filled marks need a naming convention.
-- 
--------------------------------------------------------------------------------

module Wumpus.Drawing.Dots.Marks
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
  , markTriangle

  ) where


import Wumpus.Drawing.Text.Base.RotTextZero

import Wumpus.Basic.Geometry                    -- package: wumpus-basic
import Wumpus.Basic.Kernel        

import Wumpus.Core                              -- package: wumpus-core

import Data.AffineSpace                         -- package: vector-space
import Data.VectorSpace


-- Marks should be the height of a lower-case letter...

-- NOTES
--
-- TikZ has both stroked and bordered (filled and outline-stroked)
-- marks e.g. square and square*
--





markChar :: (Real u, Floating u, InterpretUnit u) => Char -> LocGraphic u
markChar ch = markText [ch]




markText :: (Real u, Floating u, InterpretUnit u) => String -> LocGraphic u
markText ss = pushR1 ignoreAns $ ccTextline ss


umark :: InterpretUnit u => LocGraphic Em -> LocGraphic u
umark = uconvLocImageF


-- | Supplied point is the center.
--
axialLine :: (Fractional u, InterpretUnit u) => Vec2 u -> LocGraphic u
axialLine v = moveStart (\ctr -> ctr .-^ (0.5 *^ v)) (locStraightLine v)


markHLine :: (Fractional u, InterpretUnit u) => LocGraphic u 
markHLine = umark $ axialLine (hvec 1)


markVLine :: (Fractional u, InterpretUnit u) => LocGraphic u 
markVLine = umark $ axialLine (vvec 1) 


markX :: (Fractional u, InterpretUnit u) => LocGraphic u
markX = umark $ axialLine (vec 0.75 1) `oplus` axialLine (vec (-0.75) 1)



markPlus :: (Fractional u, InterpretUnit u) =>  LocGraphic u
markPlus = markVLine `oplus` markHLine


markCross :: (Floating u, InterpretUnit u) =>  LocGraphic u
markCross = 
    umark $ axialLine (avec ang 1) `oplus` axialLine (avec (-ang) 1)
  where 
    ang = pi*0.25  





markDiamond :: (Fractional u, InterpretUnit u) => LocGraphic u
markDiamond = umark $ drawVertexPathAlg STROKE (diamondPathAlg 0.5 0.66)

markFDiamond :: (Fractional u, InterpretUnit u) => LocGraphic u
markFDiamond = umark $ drawVertexPathAlg FILL (diamondPathAlg 0.5 0.66)



markBDiamond :: (Fractional u, InterpretUnit u) => LocGraphic u
markBDiamond = umark $ drawVertexPathAlg FILL_STROKE (diamondPathAlg 0.5 0.66)


-- | Note disk is filled.
--
markDisk :: (Fractional u, InterpretUnit u) => LocGraphic u
markDisk = umark $ dcDisk FILL 0.5



markSquare :: (Fractional u, InterpretUnit u) => LocGraphic u
markSquare = umark $ drawVertexPathAlg STROKE (rectanglePathAlg 1 1)


markCircle :: (Fractional u, InterpretUnit u) => LocGraphic u
markCircle = umark $ dcDisk STROKE 0.5


markBCircle :: (Fractional u, InterpretUnit u) => LocGraphic u
markBCircle = umark $ dcDisk FILL_STROKE 0.5



markPentagon :: (Floating u, InterpretUnit u) => LocGraphic u
markPentagon = umark $ drawVertexPathAlg STROKE (polygonPathAlg 5 0.5)
 


markStar :: (Floating u, InterpretUnit u) => LocGraphic u 
markStar = umark $ starLines 0.5

starLines :: (Floating u, InterpretUnit u) => u -> LocGraphic u
starLines hh = promoteR1 $ \ctr -> 
    let ps = runPathAlgPoint ctr $ polygonPathAlg 5 hh
    in step $ map (fn ctr) ps
  where
    fn p0 p1    = straightLine p0 p1
    step (x:xs) = oconcat x xs
    step _      = error "starLines - unreachable"


markAsterisk :: (Floating u, InterpretUnit u) => LocGraphic u
markAsterisk = umark $ asteriskLines 1

asteriskLines :: (Floating u, InterpretUnit u) => u -> LocGraphic u
asteriskLines h = lineF1 `oplus` lineF2 `oplus` lineF3
  where
    ang     = (pi*2) / 6
    lineF1  = axialLine (vvec h)
    lineF2  = axialLine (avec ((pi*0.5) + ang)    h)
    lineF3  = axialLine (avec ((pi*0.5) + ang + ang) h)


markOPlus :: (Fractional u, InterpretUnit u) => LocGraphic u
markOPlus = markCircle `oplus` markPlus


markOCross :: (Floating u, InterpretUnit u) => LocGraphic u
markOCross = markCircle `oplus` markCross


markFOCross :: (Floating u, InterpretUnit u) => LocGraphic u
markFOCross = markBCircle `oplus` markCross


-- bkCircle :: (Fractional u, InterpretUnit u) => LocGraphic u
-- bkCircle = disk (fillAttr attr) (0.5*markHeight attr) 



markTriangle :: (Floating u, InterpretUnit u) => LocGraphic u
markTriangle = umark $ drawVertexPathAlg STROKE alg 
  where
    alg = pathIterateLocus $ fn3 $ equilateralTriangleVertices 1
    fn3 = \(a,b,c) -> [a,b,c]
