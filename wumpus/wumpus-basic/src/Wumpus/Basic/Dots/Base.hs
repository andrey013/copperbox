{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Dots.Base
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

module Wumpus.Basic.Dots.Base
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
import Wumpus.Basic.Graphic.DrawingAttr
import Wumpus.Basic.Graphic.PointSupply
import Wumpus.Basic.Utils.HList

import Wumpus.Core                      -- package: wumpus-core

import Data.AffineSpace                 -- package: vector-space
import Data.VectorSpace

-- Marks should be the height of a lower-case letter...

-- NOTES
--
-- TikZ has both stroked and bordered (filled and outline-stroked)
-- marks e.g. square and square*
--


dotChar :: (Fractional u, FromPtSize u) => Char -> DrawingAttr -> GraphicF u
dotChar ch = dotText [ch]

dotText :: (Fractional u, FromPtSize u) => String -> DrawingAttr -> GraphicF u
dotText str attr = \ctr -> let pt = disp (-hw) (-hh) ctr in
    wrapG $ textlabel (stroke_colour attr) (font_props attr) str pt
  where
    sz = font_size  $ font_props attr
    hh = fromPtSize $ 0.5 * numeralHeight sz
    hw = fromPtSize $ 0.5 * textWidth sz (length str) 

-- | Supplied point is the center.
--
axialLine :: Fractional u => RGBi -> StrokeAttr -> Vec2 u -> GraphicF u
axialLine rgb attr v = \ctr -> let pt = ctr .-^ (0.5 *^ v) in
    wrapG $ ostroke rgb attr $ path pt [lineTo $ pt .+^ v]
 




-- Better would be a version of straightLine where the point is 
-- the center not the start...
-- 
dotHLine :: (Fractional u, FromPtSize u) => DrawingAttr -> GraphicF u 
dotHLine attr = let w = markHeight attr in 
    axialLine (stroke_colour attr) (stroke_props attr) (hvec w)
    

dotVLine :: (Fractional u, FromPtSize u) => DrawingAttr -> GraphicF u 
dotVLine attr = let h = markHeight attr in 
    axialLine (stroke_colour attr) (stroke_props attr) (vvec h)


dotX :: (Fractional u, FromPtSize u) => DrawingAttr -> GraphicF u
dotX attr = ls1 `cc` ls2
  where
    h        = markHeight attr
    w        = 0.75 * h
    ls1      = axialLine (stroke_colour attr) (stroke_props attr) (vec w h)
    ls2      = axialLine (stroke_colour attr) (stroke_props attr) (vec (-w) h)


dotPlus :: (Fractional u, FromPtSize u) => DrawingAttr -> GraphicF u
dotPlus attr = dotVLine attr `cc` dotHLine attr


dotCross :: (Floating u, FromPtSize u) => DrawingAttr -> GraphicF u
dotCross attr = ls1 `cc` ls2
  where
    z        = markHeight attr
    ls1      = axialLine (stroke_colour attr) (stroke_props attr) (avec (pi*0.25) z)
    ls2      = axialLine (stroke_colour attr) (stroke_props attr) (avec (negate $ pi*0.25) z)


-- needs horizontal pinch...

pathDiamond :: (Fractional u, FromPtSize u) => DrawingAttr -> PathF u
pathDiamond attr = vertexPath . sequence [dvs,dve,dvn,dvw]
  where
    hh    = 0.66  * markHeight attr
    hw    = 0.5   * markHeight attr
    dvs   = (.+^ vvec (-hh))
    dve   = (.+^ hvec hw)
    dvn   = (.+^ vvec hh)
    dvw   = (.+^ hvec (-hw))

type PathF u = Point2 u -> PrimPath u

dotDiamond :: (Fractional u, FromPtSize u) => DrawingAttr -> GraphicF u
dotDiamond attr = 
    wrapG . cstroke (stroke_colour attr) (stroke_props attr) . pathDiamond attr

dotFDiamond :: (Fractional u, FromPtSize u) => DrawingAttr -> GraphicF u
dotFDiamond attr = dotDiamond attr `cc` filled 
  where
    filled = wrapG . fill (fill_colour attr) . pathDiamond attr



-- | Note disk is filled.
--
dotDisk :: (Fractional u, FromPtSize u) => DrawingAttr -> GraphicF u
dotDisk attr = disk (fill_colour attr) (0.5*markHeight attr) 


dotSquare :: (Fractional u, FromPtSize u) => DrawingAttr -> GraphicF u
dotSquare attr = let u = markHeight attr in
     strokedRectangle (stroke_colour attr) (stroke_props attr) u u 
    


dotCircle :: (Fractional u, FromPtSize u) => DrawingAttr -> GraphicF u
dotCircle attr = disk (stroke_colour attr) (0.5*markHeight attr) 


dotPentagon :: (Floating u, FromPtSize u) => DrawingAttr -> GraphicF u
dotPentagon attr = 
    wrapG . cstroke (stroke_colour attr) (stroke_props attr) 
          . vertexPath . polygonPointsV 5 hh
  where
    hh      = 0.5 * markHeight attr

 

dotStar :: (Floating u, FromPtSize u) => DrawingAttr -> GraphicF u 
dotStar attr = \pt -> veloH (fn pt) $ polygonPointsV 5 hh pt
  where
    hh        = 0.5 * markHeight attr
    fn pt pt' = wrapG $ cstroke (stroke_colour attr) (stroke_props attr) 
                      $ path pt [lineTo pt'] 




dotAsterisk :: (Floating u, FromPtSize u) => DrawingAttr -> GraphicF u
dotAsterisk attr = ls1 `cc` ls2 `cc` ls3
  where
    z         = markHeight attr
    (rgb,sa)  = strokeAttr attr
    ang       = two_pi / 6
    ls1       = axialLine rgb sa (vvec z)
    ls2       = axialLine rgb sa (avec (half_pi + ang)    z)
    ls3       = axialLine rgb sa (avec (half_pi + ang + ang) z)


dotOPlus :: (Fractional u, FromPtSize u) => DrawingAttr -> GraphicF u
dotOPlus attr = dotCircle attr `cc` dotPlus attr


dotOCross :: (Floating u, FromPtSize u) => DrawingAttr -> GraphicF u
dotOCross attr = dotCircle attr `cc` dotCross attr


dotFOCross :: (Floating u, FromPtSize u) => DrawingAttr -> GraphicF u
dotFOCross attr = dotCircle attr `cc` dotCross attr `cc` bkCircle attr 

bkCircle :: (Fractional u, FromPtSize u) => DrawingAttr -> GraphicF u
bkCircle attr = disk (fillAttr attr) (0.5*markHeight attr) 
