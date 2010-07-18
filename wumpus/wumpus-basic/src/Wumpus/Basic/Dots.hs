{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Dots
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

module Wumpus.Basic.Dots
  ( 

  -- * Mark drawing attributes
    MarkAttr(..)
  , standardAttr   

  -- * Dots
  , dotHLine
  , dotVLine
  , dotX
  , dotPlus
  , dotCross
  , dotDiamond
  , dotDisk
  , dotSquare
  , dotCircle
  , dotPentagon
  , dotStar
  , dotAsterisk
  , dotOPlus
  , dotOCross

  ) where


import Wumpus.Basic.Graphic
import Wumpus.Basic.SVGColours
import Wumpus.Basic.Utils.HList

import Wumpus.Core                      -- package: wumpus-core

import Data.AffineSpace                 -- package: vector-space
import Data.VectorSpace

import Control.Applicative
import Data.List

data MarkAttr = MarkAttr 
      { line_width         :: Double
      , point_size         :: FontSize
      , mark_colour        :: DRGB
      , mark_second_colour :: DRGB
      }
  deriving (Eq,Show)

standardAttr :: FontSize -> MarkAttr
standardAttr sz = MarkAttr { line_width         = 1.0
                           , point_size         = sz
                           , mark_colour        = black
                           , mark_second_colour = gold  }

 
primaryAttr :: MarkAttr -> (DRGB, StrokeAttr)
primaryAttr = liftA2 (,) mark_colour (LineWidth . line_width)

markHeight :: Fractional u => MarkAttr -> u
markHeight = xcharHeight . point_size

-- Marks should be the height of a lower-case letter...

-- NOTES
--
-- Affine transforming Points, LineSegments etc. before
-- they become pictures is _GOOD_! The calculations are done in 
-- Wumpus and so don't cause extra (gsave... grestore) in 
-- PostScript.
--

    

-- | Supplied point is the center.
--
axialLine :: (Stroke t, Fractional u) => t -> Vec2 u -> GraphicF u
axialLine t v = \ctr -> let pt = ctr .-^ (0.5 *^ v) in
    wrapG $ ostroke t $ path pt [lineTo $ pt .+^ v]
 



-- Better would be a version of straightLine where the point is 
-- the center not the start...
-- 
dotHLine :: Fractional u => MarkAttr -> GraphicF u 
dotHLine attr = let w = markHeight attr in 
    axialLine (primaryAttr attr) (hvec w)
    

dotVLine :: Fractional u => MarkAttr -> GraphicF u 
dotVLine attr = let h = markHeight attr in 
    axialLine (primaryAttr attr) (vvec h)


dotX :: Fractional u => MarkAttr -> GraphicF u
dotX attr = ls1 `cc` ls2
  where
    h        = markHeight attr
    w        = 0.75 * h
    ls1      = axialLine (primaryAttr attr) (vec w    h)
    ls2      = axialLine (primaryAttr attr) (vec (-w) h)


dotPlus :: Fractional u => MarkAttr -> GraphicF u
dotPlus attr = dotVLine attr `cc` dotHLine attr


dotCross :: Floating u => MarkAttr -> GraphicF u
dotCross attr = ls1 `cc` ls2
  where
    z        = markHeight attr
    ls1      = axialLine (primaryAttr attr) (avec (pi*0.25)    z)
    ls2      = axialLine (primaryAttr attr) (avec (negate $ pi*0.25) z)


-- needs horizontal pinch...
dotDiamond :: Fractional u => MarkAttr -> GraphicF u
dotDiamond attr = 
    wrapG . cstroke (primaryAttr attr) . vertexPath . sequence [dvs,dve,dvn,dvw]
  where
    hh    = 0.66  * markHeight attr
    hw    = 0.5   * markHeight attr
    dvs   = (.+^ vvec (-hh))
    dve   = (.+^ hvec hw)
    dvn   = (.+^ vvec hh)
    dvw   = (.+^ hvec (-hw))





-- | Note disk is filled.
--
dotDisk :: Fractional u => MarkAttr -> GraphicF u
dotDisk attr = disk (mark_colour attr) (0.5*markHeight attr) 


dotSquare :: Fractional u => MarkAttr -> GraphicF u
dotSquare attr = let u = markHeight attr in
     strokedRectangle (primaryAttr attr) u u 
    


dotCircle :: Fractional u => MarkAttr -> GraphicF u
dotCircle attr = disk (primaryAttr attr) (0.5*markHeight attr) 


dotPentagon :: Floating u => MarkAttr -> GraphicF u
dotPentagon attr = 
    wrapG . cstroke (primaryAttr attr) . vertexPath . polygonPointsV 5 hh
  where
    hh      = 0.5 * markHeight attr

 

two_pi :: Radian
two_pi = 2.0 * pi

half_pi :: Radian
half_pi = 0.5 * pi


polygonPointsV :: Floating u => Int -> u -> Point2 u -> [Point2 u]
polygonPointsV n radius = sequence vecs
  where
    theta = two_pi / fromIntegral n
    vecs  = unfoldr phi (0,half_pi)
    
    phi (i,ang) | i < n     = Just ((.+^ avec ang radius), (i+1,ang+theta))
                | otherwise = Nothing


dotStar :: Floating u => MarkAttr -> GraphicF u 
dotStar attr = \pt -> veloH (fn pt) $ polygonPointsV 5 hh pt
  where
    hh        = 0.5 * markHeight attr
    fn pt pt' = wrapG $ cstroke (primaryAttr attr) $ path pt [lineTo pt'] 




dotAsterisk :: Floating u => MarkAttr -> GraphicF u
dotAsterisk attr = ls1 `cc` ls2 `cc` ls3
  where
    z        = markHeight attr
    props    = primaryAttr attr
    ang      = two_pi / 6
    ls1      = axialLine props (vvec z)
    ls2      = axialLine props (avec (half_pi + ang)    z)
    ls3      = axialLine props (avec (half_pi + ang + ang) z)


dotOPlus :: Fractional u
         => MarkAttr -> GraphicF u
dotOPlus attr = dotCircle attr `cc` dotPlus attr


dotOCross :: Floating u => MarkAttr -> GraphicF u
dotOCross attr = dotCircle attr `cc` dotCross attr

