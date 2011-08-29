{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Drawing.Dots.SimpleDots
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Simple dots - no anchor handles.
-- 
-- Use these where you just want to draw Dots, and do not need
-- connectors between them. 
--
-- The text and char marks need loaded glyph metrics for proper 
-- centering. 
--
-- \*\* WARNING \*\* - names are expected to change - filled and
-- background-filled marks need a naming convention.
-- 
--------------------------------------------------------------------------------

module Wumpus.Drawing.Dots.SimpleDots
  ( 

  -- * Unit for marks (0.75 the font size)
    MarkSize

  , smallDisk
  , largeDisk
  , smallCirc
  , largeCirc  



  -- * Dots
  , dotNone

  , dotChar
  , dotText
  , dotEscChar
  , dotEscText


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
  , dotTriangle

  ) where


import Wumpus.Drawing.Paths

import Wumpus.Basic.Geometry                    -- package: wumpus-basic
import Wumpus.Basic.Kernel        

import Wumpus.Core                              -- package: wumpus-core

import Data.VectorSpace                         -- package: vector-space

import Data.Monoid

-- Marks should be the height of a lower-case letter...

-- NOTES
--
-- TikZ has both stroked and bordered (filled and outline-stroked)
-- marks e.g. square and square*
--

-- Cap height is a good size for Dots.


-- | MarkUnit is a contextual unit like 'Em' and 'En'.
-- 
-- It is 3\/4 of the current font size.
--
newtype MarkSize = MarkSize { getMarkSize :: Double }
  deriving (Eq,Ord,Num,Floating,Fractional,Real,RealFrac,RealFloat)

instance Show MarkSize where
  showsPrec p d = showsPrec p (getMarkSize d)


instance InterpretUnit MarkSize where
  normalize sz a = (realToFrac  a) * 0.75 * fromIntegral sz
  dinterp sz d   = (4/3) * (realToFrac d) / (fromIntegral sz)

instance Tolerance MarkSize where 
  eq_tolerance     = 0.001
  length_tolerance = 0.01


umark :: InterpretUnit u => LocGraphic MarkSize -> LocGraphic u
umark = uconvF


-- | Filled disk - radius 0.25 MarkSize.
--
smallDisk :: InterpretUnit u => LocGraphic u
smallDisk = umark $ dcDisk DRAW_FILL 0.25


-- | Filled disk - radius 1.0 MarkSize.
--
largeDisk :: InterpretUnit u => LocGraphic u
largeDisk = umark $ dcDisk DRAW_FILL 1


-- | Stroked disk (circle) - radius 0.25 MarkSize.
--
smallCirc :: InterpretUnit u => LocGraphic u
smallCirc = umark $ dcDisk DRAW_STROKE 0.25


-- | Stroked disk (circle) - radius 1.0 MarkSize.
--
largeCirc :: InterpretUnit u => LocGraphic u
largeCirc = umark $ dcDisk DRAW_STROKE 1


-- possibly:
-- szCirc :: u -> LocGraphic u

dotNone :: InterpretUnit u => LocGraphic u
dotNone = emptyLocImage

dotChar :: (Real u, Floating u, InterpretUnit u) => Char -> LocGraphic u
dotChar ch = dotText [ch]



dotText :: (Real u, Floating u, InterpretUnit u) => String -> LocGraphic u
dotText ss = ignoreAns $ runPosObject (posText ss) CENTER

dotEscChar :: (Real u, Floating u, InterpretUnit u) 
           => EscapedChar -> LocGraphic u
dotEscChar = dotEscText . wrapEscChar

dotEscText :: (Real u, Floating u, InterpretUnit u) 
           => EscapedText -> LocGraphic u
dotEscText esc = ignoreAns $ runPosObject (posEscText esc) CENTER

-- TODO - need Upright versions of dots...



-- | Supplied point is the center.
--
axialLine :: (Fractional u, InterpretUnit u) => Vec2 u -> LocGraphic u
axialLine v = moveStart (negateV (0.5 *^ v)) (locStraightLine v)


dotHLine :: (Fractional u, InterpretUnit u) => LocGraphic u 
dotHLine = umark $ axialLine (hvec 1)


dotVLine :: (Fractional u, InterpretUnit u) => LocGraphic u 
dotVLine = umark $ axialLine (vvec 1) 


dotX :: (Fractional u, InterpretUnit u) => LocGraphic u
dotX = umark $ axialLine (vec 0.75 1) `mappend` axialLine (vec (-0.75) 1)



dotPlus :: (Fractional u, InterpretUnit u) =>  LocGraphic u
dotPlus = dotVLine `mappend` dotHLine


dotCross :: (Floating u, InterpretUnit u) =>  LocGraphic u
dotCross = 
    umark $ axialLine (avec ang 1) `mappend` axialLine (avec (-ang) 1)
  where 
    ang = pi*0.25  





dotDiamond :: (Fractional u, InterpretUnit u) => LocGraphic u
dotDiamond = umark $ drawPathScheme_ CSTROKE (diamondPathScm 0.5 0.66)

dotFDiamond :: (Fractional u, InterpretUnit u) => LocGraphic u
dotFDiamond = umark $ drawPathScheme_ CFILL (diamondPathScm 0.5 0.66)



dotBDiamond :: (Fractional u, InterpretUnit u) => LocGraphic u
dotBDiamond = umark $ drawPathScheme_ CFILL_STROKE (diamondPathScm 0.5 0.66)


-- | Note disk is filled.
--
dotDisk :: (Fractional u, InterpretUnit u) => LocGraphic u
dotDisk = umark $ dcDisk DRAW_FILL 0.5



dotSquare :: (Fractional u, InterpretUnit u) => LocGraphic u
dotSquare = umark $ drawPathScheme_ CSTROKE (rectanglePathScm 1 1)


dotCircle :: (Fractional u, InterpretUnit u) => LocGraphic u
dotCircle = umark $ dcDisk DRAW_STROKE 0.5


dotBCircle :: (Fractional u, InterpretUnit u) => LocGraphic u
dotBCircle = umark $ dcDisk DRAW_FILL_STROKE 0.5



dotPentagon :: (Floating u, InterpretUnit u) => LocGraphic u
dotPentagon = umark $ drawPathScheme_ CSTROKE (polygonPathScm 5 0.5)
 


dotStar :: (Floating u, Ord u, InterpretUnit u, Tolerance u) 
        => LocGraphic u 
dotStar = umark $ starLines 0.5

starLines :: (Floating u, Ord u, InterpretUnit u, Tolerance u) 
          => u -> LocGraphic u
starLines hh = promoteLoc $ \ctr ->
    let alg = polygonPathScm 5 hh
    in liftQuery (qapplyLoc (pathSchemeSegmentInits alg) ctr) >>= \ps -> 
       step $ map (fn ctr) ps
  where
    fn p0 p1    = straightLine p0 p1
    step (x:xs) = mconcat $ x:xs
    step _      = error "starLines - unreachable"

dotAsterisk :: (Floating u, InterpretUnit u) => LocGraphic u
dotAsterisk = umark $ asteriskLines 1

asteriskLines :: (Floating u, InterpretUnit u) => u -> LocGraphic u
asteriskLines h = lineF1 `mappend` lineF2 `mappend` lineF3
  where
    ang     = (pi*2) / 6
    lineF1  = axialLine (vvec h)
    lineF2  = axialLine (avec ((pi*0.5) + ang)    h)
    lineF3  = axialLine (avec ((pi*0.5) + ang + ang) h)


dotOPlus :: (Fractional u, InterpretUnit u) => LocGraphic u
dotOPlus = dotCircle `mappend` dotPlus


dotOCross :: (Floating u, InterpretUnit u) => LocGraphic u
dotOCross = dotCircle `mappend` dotCross


dotFOCross :: (Floating u, InterpretUnit u) => LocGraphic u
dotFOCross = dotBCircle `mappend` dotCross


-- bkCircle :: (Fractional u, InterpretUnit u) => LocGraphic u
-- bkCircle = disk (fillAttr attr) (0.5*markHeight attr) 



dotTriangle :: (Floating u, InterpretUnit u) => LocGraphic u
dotTriangle = umark $ drawPathScheme_ CSTROKE alg 
  where
    alg = pathIterateLocus $ fn3 $ equilateralTriangleVertices 1
    fn3 = \(a,b,c) -> [a,b,c]
