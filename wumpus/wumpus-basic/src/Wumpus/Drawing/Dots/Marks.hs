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




infixr 9 `renderPathWith`

renderPathWith :: LocDrawingInfo u (PrimPath u) 
               -> (PrimPath u -> Graphic u) 
               -> LocGraphic u
renderPathWith m k = m >>= (lift0R1 . k)



markChar :: (Fractional u, Ord u, FromPtSize u) => Char -> LocGraphic u
markChar ch = markText [ch]




markText :: (Fractional u, Ord u, FromPtSize u) => String -> LocGraphic u
markText ss = fmap (replaceL uNil) $ singleLineCC ss




-- | Supplied point is the center.
--
axialLine :: Fractional u => Vec2 u -> LocGraphic u
axialLine v = moveStartPoint (\ctr -> ctr .-^ (0.5 *^ v)) (straightLine v)


markHLine :: (Fractional u, FromPtSize u) => LocGraphic u 
markHLine = lift0R1 markHeight >>= \h -> axialLine (hvec h)


markVLine :: (Fractional u, FromPtSize u) => LocGraphic u 
markVLine = lift0R1 markHeight >>= \h -> axialLine (vvec h) 


markX :: (Fractional u, FromPtSize u) => LocGraphic u
markX = lift0R1 markHeight >>= mkX 
  where
    mkX h = let w = 0.75 * h
              in axialLine (vec w h) `oplus` axialLine (vec (-w) h)



markPlus :: (Fractional u, FromPtSize u) =>  LocGraphic u
markPlus = markVLine `oplus` markHLine


markCross :: (Floating u, FromPtSize u) =>  LocGraphic u
markCross = markHeight >>= mkCross
  where
    mkCross h = axialLine (avec ang h) `oplus` axialLine (avec (-ang) h)
    ang       = pi*0.25  

-- Note - height is extended slightly to look good...

pathDiamond :: (Fractional u, FromPtSize u) 
            => LocDrawingInfo u (PrimPath u)
pathDiamond = 
    promoteR1 $ \pt -> 
      markHeight >>= \h -> pure $ diamondPath (0.5*h) (0.66*h) pt



-- closedStroke :: (a -> ctx -> prim) 
-- pathDiamond  :: (ctx -> pt -> a)
-- ans          :: (ctx -> pt -> prim)

markDiamond :: (Fractional u, FromPtSize u) => LocGraphic u
markDiamond = pathDiamond `renderPathWith` closedStroke

markFDiamond :: (Fractional u, FromPtSize u) => LocGraphic u
markFDiamond = pathDiamond `renderPathWith` filledPath


-- Note - the (const . fn) composition doesn\'t /tell/ much about
-- what is going on - though obviously it can be decoded - make 
-- the function obvious to the second argument. 
-- 
-- A named combinator might be better.
--

markBDiamond :: (Fractional u, FromPtSize u) => LocGraphic u
markBDiamond = pathDiamond `renderPathWith` borderedPath


-- | Note disk is filled.
--
markDisk :: (Fractional u, FromPtSize u) => LocGraphic u
markDisk = lift0R1 markHalfHeight >>= filledDisk 



markSquare :: (Fractional u, FromPtSize u) => LocGraphic u
markSquare = 
    lift0R1 markHeight >>= \h -> 
    let d = 0.5*(-h) in moveStartPoint (displace d d) $ strokedRectangle h h
    


markCircle :: (Fractional u, FromPtSize u) => LocGraphic u
markCircle = lift0R1 markHalfHeight >>= strokedDisk 


markBCircle :: (Fractional u, FromPtSize u) => LocGraphic u
markBCircle = lift0R1 markHalfHeight >>= borderedDisk 



markPentagon :: (Floating u, FromPtSize u) => LocGraphic u
markPentagon = 
    promoteR1 $ \pt -> 
      markHeight >>= \h -> closedStroke $ vertexPath $ pentagonPath pt (0.5*h)
  where
    pentagonPath pt hh = polygonPoints 5 hh pt

 


markStar :: (Floating u, FromPtSize u) => LocGraphic u 
markStar = lift0R1 markHeight >>= \h -> starLines (0.5*h)

starLines :: Floating u => u -> LocGraphic u
starLines hh = 
    promoteR1 $ \ctr -> step $ map (fn ctr) $ polygonPoints 5 hh ctr
  where
    fn p0 p1    = openStroke $ primPath p0 [lineTo p1]
    step (x:xs) = oconcat x xs
    step _      = error "starLines - unreachable"


markAsterisk :: (Floating u, FromPtSize u) => LocGraphic u
markAsterisk = lift0R1 markHeight >>= asteriskLines

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
markTriangle = tripath `renderPathWith` closedStroke
  where
    tripath = promoteR1 $ \pt -> 
                markHeight >>= \h -> pure $ equilateralTrianglePath h pt

