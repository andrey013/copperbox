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


import Wumpus.Drawing.Text.RotTextLR

import Wumpus.Basic.Geometry.Paths              -- package: wumpus-basic
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




infixr 9 `renderPathWith`

renderPathWith :: LocQuery u PrimPath 
               -> (PrimPath -> Graphic u) 
               -> LocGraphic u
renderPathWith qy mk = promoteR1 $ \pt -> applyQ1 qy pt &=> mk



markChar :: (Real u, Floating u, InterpretUnit u) => Char -> LocGraphic u
markChar ch = markText [ch]




markText :: (Real u, Floating u, InterpretUnit u) => String -> LocGraphic u
markText ss = ignoreAns $ textAlignCenter ss




-- | Supplied point is the center.
--
axialLine :: (Fractional u, InterpretUnit u) => Vec2 u -> LocGraphic u
axialLine v = moveStart (\ctr -> ctr .-^ (0.5 *^ v)) (locStraightLine v)


markHLine :: (Fractional u, InterpretUnit u) => LocGraphic u 
markHLine = markHeight &=> \h -> axialLine (hvec h)


markVLine :: (Fractional u, InterpretUnit u) => LocGraphic u 
markVLine = markHeight &=> \h -> axialLine (vvec h) 


markX :: (Fractional u, InterpretUnit u) => LocGraphic u
markX = markHeight &=> mkX 
  where
    mkX h = let w = 0.75 * h
              in axialLine (vec w h) `oplus` axialLine (vec (-w) h)



markPlus :: (Fractional u, InterpretUnit u) =>  LocGraphic u
markPlus = markVLine `oplus` markHLine


markCross :: (Floating u, InterpretUnit u) =>  LocGraphic u
markCross = markHeight &=> mkCross
  where
    mkCross h = axialLine (avec ang h) `oplus` axialLine (avec (-ang) h)
    ang       = pi*0.25  

-- Note - height is extended slightly to look good...

pathDiamond :: (Fractional u, InterpretUnit u) 
            => LocQuery u PrimPath
pathDiamond = 
    promoteQ1 $ \pt -> 
      markHeight >>= \h -> let cp = diamondCoordPath (0.5*h) (0.66*h) 
                           in coordinatePrimPath cp pt



-- closedStroke :: (a -> ctx -> prim) 
-- pathDiamond  :: (ctx -> pt -> a)
-- ans          :: (ctx -> pt -> prim)

markDiamond :: (Fractional u, InterpretUnit u) => LocGraphic u
markDiamond = pathDiamond `renderPathWith` closedStroke

markFDiamond :: (Fractional u, InterpretUnit u) => LocGraphic u
markFDiamond = pathDiamond `renderPathWith` filledPath


-- Note - the (const . fn) composition doesn\'t /tell/ much about
-- what is going on - though obviously it can be decoded - make 
-- the function obvious to the second argument. 
-- 
-- A named combinator might be better.
--

markBDiamond :: (Fractional u, InterpretUnit u) => LocGraphic u
markBDiamond = pathDiamond `renderPathWith` borderedPath


-- | Note disk is filled.
--
markDisk :: (Fractional u, InterpretUnit u) => LocGraphic u
markDisk = markHalfHeight &=> filledDisk



markSquare :: (Fractional u, InterpretUnit u) => LocGraphic u
markSquare = 
    markHeight &=> \h -> 
    let d = 0.5*(-h) in moveStart (displace d d) $ strokedRectangle h h
    


markCircle :: (Fractional u, InterpretUnit u) => LocGraphic u
markCircle = markHalfHeight &=> strokedDisk


markBCircle :: (Fractional u, InterpretUnit u) => LocGraphic u
markBCircle = markHalfHeight &=> borderedDisk 



markPentagon :: (Floating u, InterpretUnit u) => LocGraphic u
markPentagon = promoteR1 $ \pt -> 
    pentagonPath pt &=> closedStroke
  where
    pentagonPath pt = markHalfHeight >>= \hh -> 
                      coordinatePrimPath (polygonCoordPath 5 hh) pt

 


markStar :: (Floating u, InterpretUnit u) => LocGraphic u 
markStar = markHeight &=> \h -> starLines (0.5*h)

starLines :: (Floating u, InterpretUnit u) => u -> LocGraphic u
starLines hh = promoteR1 $ \ctr -> 
    let cp = polygonCoordPath 5 hh
    in step $ map (fn ctr) $ cp ctr
  where
    fn p0 p1    = straightLine p0 p1
    step (x:xs) = oconcat x xs
    step _      = error "starLines - unreachable"


markAsterisk :: (Floating u, InterpretUnit u) => LocGraphic u
markAsterisk = markHeight &=> asteriskLines

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
markFOCross = markCross `oplus` markBCircle 


-- bkCircle :: (Fractional u, InterpretUnit u) => LocGraphic u
-- bkCircle = disk (fillAttr attr) (0.5*markHeight attr) 



markTriangle :: (Floating u, InterpretUnit u) => LocGraphic u
markTriangle = tripath `renderPathWith` closedStroke
  where
    tripath = promoteQ1 $ \pt -> 
                markHeight >>= \h -> 
                  let cp = equilateralTriangleCoordPath h
                  in coordinatePrimPath cp pt

