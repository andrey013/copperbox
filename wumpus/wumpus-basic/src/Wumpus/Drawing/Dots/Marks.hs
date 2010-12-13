{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Drawing.Dots.Marks
-- Copyright   :  (c) Stephen Tetley 2010
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


import Wumpus.Basic.Kernel
import Wumpus.Drawing.Text.LRText

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


liftQuery :: DrawingInfo a -> (a -> LocCF u b) -> LocCF u b
liftQuery q mf = (static1 q) `bind1` mf  


infixr 9 `renderPath`

renderPath :: LocDrawingInfo u (PrimPath u) 
           -> (PrimPath u -> Graphic u) 
           -> LocGraphic u
renderPath m k = m `bind1` (static1 . k)

-- intoLocImage :: LocCF u a -> LocCF u (z,b) -> LocCF u (a,b)
-- intoLocImage = postcomb1 (\a (_,b) -> (a,b))

shiftOrigin :: Num u => u -> u -> LocGraphic u -> LocGraphic u
shiftOrigin dx dy = prepro1 (displace dx dy)



markChar :: (Fractional u, Ord u, FromPtSize u) => Char -> LocGraphic u
markChar ch = markText [ch]




markText :: (Fractional u, Ord u, FromPtSize u) => String -> LocGraphic u
markText ss = postpro1 (\(_,b) -> (uNil,b)) $ singleLineCC ss





-- | Supplied point is the center.
--
axialLine :: Fractional u => Vec2 u -> LocGraphic u
axialLine v = localPoint (\ctr -> ctr .-^ (0.5 *^ v)) (straightLine v)


markHLine :: (Fractional u, FromPtSize u) => LocGraphic u 
markHLine = liftQuery markHeight $ \h -> axialLine (hvec h)


markVLine :: (Fractional u, FromPtSize u) => LocGraphic u 
markVLine = liftQuery markHeight $ \h -> axialLine (vvec h) 


markX :: (Fractional u, FromPtSize u) => LocGraphic u
markX = liftQuery markHeight $ \h -> 
    let w = 0.75 * h in axialLine (vec w h) `oplus` axialLine (vec (-w) h)



markPlus :: (Fractional u, FromPtSize u) =>  LocGraphic u
markPlus = markVLine `oplus` markHLine


markCross :: (Floating u, FromPtSize u) =>  LocGraphic u
markCross = liftQuery markHeight $ \h ->  
    (axialLine $ avec ang h) `oplus` (axialLine $ avec (-ang) h)
  where
    ang = pi*0.25  

-- Note - height is extended slightly to look good...

pathDiamond :: (Fractional u, FromPtSize u) 
            => LocDrawingInfo u (PrimPath u)
pathDiamond = liftQuery markHeight $ \h -> pure $ diamondPath (0.5*h) (0.66*h)



-- closedStroke :: (a -> ctx -> prim) 
-- pathDiamond  :: (ctx -> pt -> a)
-- ans          :: (ctx -> pt -> prim)

markDiamond :: (Fractional u, FromPtSize u) => LocGraphic u
markDiamond = pathDiamond `renderPath` closedStroke

markFDiamond :: (Fractional u, FromPtSize u) => LocGraphic u
markFDiamond = pathDiamond `renderPath` filledPath


-- Note - the (const . fn) composition doesn\'t /tell/ much about
-- what is going on - though obviously it can be decoded - make 
-- the function obvious to the second argument. 
-- 
-- A named combinator might be better.
--

markBDiamond :: (Fractional u, FromPtSize u) => LocGraphic u
markBDiamond = pathDiamond `renderPath` borderedPath


-- | Note disk is filled.
--
markDisk :: (Fractional u, FromPtSize u) => LocGraphic u
markDisk = liftQuery markHalfHeight filledDisk 



markSquare :: (Fractional u, FromPtSize u) => LocGraphic u
markSquare = liftQuery markHeight $ \h -> 
    let d = 0.5*(-h) in shiftOrigin d d $ strokedRectangle h h
    


markCircle :: (Fractional u, FromPtSize u) => LocGraphic u
markCircle = liftQuery markHalfHeight strokedDisk 


markBCircle :: (Fractional u, FromPtSize u) => LocGraphic u
markBCircle = liftQuery markHalfHeight borderedDisk 



markPentagon :: (Floating u, FromPtSize u) => LocGraphic u
markPentagon = liftQuery markHeight $ \h ->
    promote1 $ \pt -> closedStroke $ vertexPath $ polygonPoints 5 (0.5*h) pt

 


markStar :: (Floating u, FromPtSize u) => LocGraphic u 
markStar = 
    liftQuery markHeight $ \h -> starLines (0.5*h)

starLines :: Floating u => u -> LocGraphic u
starLines hh = 
    promote1 $ \ctr -> step $ map (fn ctr) $ polygonPoints 5 hh ctr
  where
    fn p0 p1    = openStroke $ primPath p0 [lineTo p1]
    step (x:xs) = oconcat x xs
    step _      = error "starLines - unreachable"


markAsterisk :: (Floating u, FromPtSize u) => LocGraphic u
markAsterisk = liftQuery markHeight asteriskLines

asteriskLines :: Floating u => u -> LocGraphic u
asteriskLines h = lineF1 `oplus` lineF2 `oplus` lineF3
  where
    ang     = (pi*2) / 6
    lineF1  = axialLine (vvec h)
    lineF2  = axialLine (avec ((pi*0.5) + ang)    h)
    lineF3  = axialLine (avec ((pi*0.5) + ang + ang) h)


markOPlus :: (Fractional u, FromPtSize u) => LocGraphic u
markOPlus = markCircle `oplus` markPlus


markOCross :: (Floating u, FromPtSize u) => LocGraphic u
markOCross = markCircle `oplus` markCross


markFOCross :: (Floating u, FromPtSize u) => LocGraphic u
markFOCross = markCross `oplus` markBCircle 


-- bkCircle :: (Fractional u, FromPtSize u) => LocGraphic u
-- bkCircle = disk (fillAttr attr) (0.5*markHeight attr) 



markTriangle :: (Floating u, FromPtSize u) => LocGraphic u
markTriangle = tripath `renderPath` closedStroke
  where
    tripath = liftQuery markHeight $ \h -> pure $ equilateralTrianglePath h

