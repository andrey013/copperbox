{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Kernel.Objects.Graphic
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  highly unstable
-- Portability :  GHC 
--
-- Graphic type - this is largely equivalent to Primitive in
-- Wumpus-Core, but drawing attributes are implicitly supplied 
-- by the DrawingContext.
--
-- API in @Wumpus.Core@, but here they exploit the implicit 
-- @DrawingContext@.
--
--------------------------------------------------------------------------------

module Wumpus.Basic.Kernel.Objects.Graphic
  (

    Graphic
  , DGraphic


  -- * LocGraphic  
  , LocGraphic
  , DLocGraphic


  , LocThetaGraphic
  , DLocThetaGraphic


  , moveOrigin

  , locPath
  , emptyLocPath
  , emptyLocGraphic


  , openStroke
  , closedStroke
  , filledPath
  , borderedPath

  , textline
  , rtextline
  , escapedline
  , rescapedline

  , hkernline
  , vkernline

  , strokedEllipse
  , rstrokedEllipse
  , filledEllipse
  , rfilledEllipse

  , borderedEllipse
  , rborderedEllipse

  , straightLine
  , straightLineBetween
  , curveBetween

  , strokedRectangle
  , filledRectangle
  , borderedRectangle

  , strokedCircle
  , filledCircle
  , borderedCircle
  
  , strokedDisk
  , filledDisk
  , borderedDisk

  ) where

import Wumpus.Basic.Kernel.Base.ContextFun
import Wumpus.Basic.Kernel.Base.BaseDefs
import Wumpus.Basic.Kernel.Base.QueryDC
import Wumpus.Basic.Kernel.Base.WrappedPrimitive
import Wumpus.Basic.Kernel.Geometry.Paths
import Wumpus.Basic.Kernel.Objects.BaseObjects

import Wumpus.Core                              -- package: wumpus-core

import Data.AffineSpace                         -- package: vector-space

import Control.Applicative

--------------------------------------------------------------------------------
-- Graphic

-- | Simple drawing - produce a primitive, access the DrawingContext
-- if required.
--
type Graphic u          = Image u (UNil u)

type DGraphic           = Graphic Double


-- | /Originated/ drawing - produce a primitive respective to the 
-- supplied start-point, access the DrawingContext if required.
--
type LocGraphic u       = LocImage u (UNil u)

type DLocGraphic        = LocGraphic Double




-- | /Originated/ drawing - produce a primitive respective to the 
-- supplied start-point, access the DrawingContext if required.
--
type LocThetaGraphic u       = LocThetaImage u (UNil u)

type DLocThetaGraphic        = LocThetaGraphic Double




moveOrigin :: PointDisplace u -> LocImage u a -> LocImage u a
moveOrigin f ma = promoteR1 $ \pt -> ma `at` f pt

--------------------------------------------------------------------------------



graphicBody :: Primitive u -> (UNil u, PrimGraphic u)
graphicBody p = (uNil, primGraphic p)

locGraphicBody :: (Point2 u -> Primitive u) 
               -> (Point2 u -> (UNil u, PrimGraphic u))
locGraphicBody fn = \pt -> (uNil, primGraphic $ fn pt)


locThetaGraphicBody :: (Radian -> Point2 u -> Primitive u) 
                    -> (Point2 u -> Radian -> (UNil u, PrimGraphic u))
locThetaGraphicBody fn = \pt theta -> (uNil, primGraphic $ fn theta pt)


--------------------------------------------------------------------------------

-- | This is the analogue to 'vectorPath' in @Wumpus-core@.
--
locPath :: Num u => [Vec2 u] -> LocDrawingInfo u (PrimPath u)
locPath vs = promoteR1 $ \pt  -> pure $ vectorPath pt vs


-- | This is the analogue to 'emptyPath' in @Wumpus-core@.
--
emptyLocPath :: Num u => LocDrawingInfo u (PrimPath u)
emptyLocPath = locPath []


emptyLocGraphic :: Num u => LocGraphic u
emptyLocGraphic = emptyLocPath >>= (lift0R1 . openStroke)


-- | This is the analogue to 'ostroke' in @Wumpus-core@.
--
openStroke :: Num u => PrimPath u -> Graphic u
openStroke pp = 
    withStrokeAttr $ \rgb attr -> graphicBody $ ostroke rgb attr pp


-- | This is the analogue to 'cstroke' in @Wumpus-core@.
--
closedStroke :: Num u => PrimPath u -> Graphic u
closedStroke pp = 
    withStrokeAttr $ \rgb attr -> graphicBody $ cstroke rgb attr pp


-- | This is the analogue to 'fill' in @Wumpus-core@.
--
filledPath :: Num u => PrimPath u -> Graphic u
filledPath pp = withFillAttr $ \rgb -> graphicBody $ fill rgb pp
                 

-- | This is the analogue to 'fillStroke' in @Wumpus-core@.
--
borderedPath :: Num u => PrimPath u -> Graphic u
borderedPath pp =
    withBorderedAttr $ \frgb attr srgb -> 
                           graphicBody $ fillStroke frgb attr srgb pp




-- | This is the analogue to 'textlabel' in @Wumpus-core@.
--
textline :: Num u => String -> LocGraphic u
textline ss = adaptR1 $
    withTextAttr $ \rgb attr -> locGraphicBody (textlabel rgb attr ss)




-- | This is the analogue to 'rtextlabel' in @Wumpus-core@.
--
rtextline :: Num u => String -> LocThetaGraphic u
rtextline ss = adaptR2 $
    withTextAttr $ \rgb attr -> locThetaGraphicBody (rtextlabel rgb attr ss)



-- | This is the analogue to 'escapedlabel' in @Wumpus-core@.
--
escapedline :: Num u => EscapedText -> LocGraphic u
escapedline ss = adaptR1 $
    withTextAttr $ \rgb attr -> locGraphicBody (escapedlabel rgb attr ss)



-- | This is the analogue to 'rescapedlabel' in @Wumpus-core@.
--
rescapedline :: Num u => EscapedText -> LocThetaGraphic u
rescapedline ss = adaptR2 $
    withTextAttr $ \rgb attr -> locThetaGraphicBody (rescapedlabel rgb attr ss)




-- | This is the analogue to 'hkernlabel' in @Wumpus-core@.
--
hkernline :: Num u => [KerningChar u] -> LocGraphic u
hkernline xs = adaptR1 $
    withTextAttr $ \rgb attr -> locGraphicBody (hkernlabel rgb attr xs)


-- | This is the analogue to 'vkernlabel' in @Wumpus-core@.
--
vkernline :: Num u => [KerningChar u] -> LocGraphic u
vkernline xs = adaptR1 $ 
    withTextAttr $ \rgb attr -> locGraphicBody (vkernlabel rgb attr xs)




--------------------------------------------------------------------------------





-- | This is the analogue to 'strokeEllipse' in @Wumpus-core@.
--
strokedEllipse :: Num u => u -> u -> LocGraphic u
strokedEllipse hw hh = adaptR1 $ 
    withStrokeAttr $ \rgb attr -> locGraphicBody (strokeEllipse rgb attr hw hh)



-- | This is the analogue to 'rstrokeEllispe' in @Wumpus-core@.
--
rstrokedEllipse :: Num u => u -> u -> LocThetaGraphic u
rstrokedEllipse hw hh = adaptR2 $
    withStrokeAttr $ \rgb attr -> locThetaGraphicBody (rstrokeEllipse rgb attr hw hh)


-- | This is the analogue to 'fillEllispe' in @Wumpus-core@.
--
filledEllipse :: Num u => u -> u -> LocGraphic u
filledEllipse hw hh = adaptR1 $ 
    withFillAttr $ \rgb -> locGraphicBody (fillEllipse rgb hw hh)


-- | This is the analogue to 'rfillEllispe' in @Wumpus-core@.
--
rfilledEllipse :: Num u => u -> u -> LocThetaGraphic u
rfilledEllipse hw hh = adaptR2 $ 
    withFillAttr $ \rgb -> locThetaGraphicBody (rfillEllipse rgb hw hh)



-- | This is the analogue to 'fillStrokeEllispe' in @Wumpus-core@.
--
borderedEllipse :: Num u => u -> u -> LocGraphic u
borderedEllipse hw hh = adaptR1 $
    withBorderedAttr $ \frgb attr srgb -> 
      locGraphicBody (fillStrokeEllipse frgb attr srgb hw hh)

-- | This is the analogue to 'rfillStrokeEllispe' in @Wumpus-core@.
--
rborderedEllipse :: Num u => u -> u -> LocThetaGraphic u
rborderedEllipse hw hh = adaptR2 $ 
    withBorderedAttr $ \frgb attr srgb -> 
      locThetaGraphicBody (rfillStrokeEllipse frgb attr srgb hw hh)



-- Note - clipping needs a picture as well as a path, so there is
-- no analogous @clippedPath@ function.

--------------------------------------------------------------------------------


straightLine :: Fractional u => Vec2 u -> LocGraphic u
straightLine v = mf >>= (lift0R1 . openStroke)
  where
    mf = promoteR1 $ \pt -> pure $ primPath pt [lineTo $ pt .+^ v]

          
-- | Draw a straight line - start and end point are supplied 
-- explicitly.
-- 
straightLineBetween :: Fractional u => Point2 u -> Point2 u -> Graphic u
straightLineBetween p1 p2 = openStroke $ primPath p1 [lineTo p2]



-- | Draw a Bezier curve - all points are supplied explicitly.
-- 
curveBetween :: Fractional u 
             => Point2 u -> Point2 u -> Point2 u -> Point2 u -> Graphic u
curveBetween sp cp1 cp2 ep = openStroke $ primPath sp [curveTo cp1 cp2 ep]


-- This is a permuted version of the cardinal-prime combinator...
-- 
-- > (r2 -> a) -> (a -> r1 -> ans) -> (r1 -> r2 -> ans)
--

drawWith :: (Point2 u -> PrimPath u) -> (PrimPath u -> Graphic u) -> LocGraphic u 
drawWith g mf = promoteR1 (\pt -> mf $ g pt)


-- | Supplied point is /bottom left/.
--
strokedRectangle :: Fractional u => u -> u -> LocGraphic u
strokedRectangle w h = rectanglePath w h `drawWith` closedStroke


-- | Supplied point is /bottom left/.
--
filledRectangle :: Fractional u => u -> u -> LocGraphic u
filledRectangle w h = rectanglePath w h `drawWith` filledPath

-- | Supplied point is /bottom left/.
--
borderedRectangle :: Fractional u => u -> u -> LocGraphic u
borderedRectangle w h = rectanglePath w h `drawWith` borderedPath


-- | Supplied point is center. Circle is drawn with Bezier 
-- curves. 
--
strokedCircle :: Floating u => Int -> u -> LocGraphic u
strokedCircle n r = (curvedPath . bezierCircle n r) `drawWith` closedStroke 



-- | Supplied point is center. Circle is drawn with Bezier 
-- curves. 
--
filledCircle :: Floating u => Int -> u -> LocGraphic u
filledCircle n r =  (curvedPath . bezierCircle n r) `drawWith` filledPath



-- | Supplied point is center. Circle is drawn with Bezier 
-- curves. 
--
borderedCircle :: Floating u => Int -> u -> LocGraphic u
borderedCircle n r = (curvedPath . bezierCircle n r) `drawWith` borderedPath 


-- | 'disk' is drawn with Wumpus-Core\'s @ellipse@ primitive.
--
-- This is a efficient representation of circles using 
-- PostScript\'s @arc@ or SVG\'s @circle@ in the generated 
-- output. However, stroked-circles do not draw well after 
-- non-uniform scaling - the line width is scaled as well as 
-- the shape.
--
-- For stroked circles that can be scaled, consider making the 
-- circle from Bezier curves.
--
strokedDisk :: Num u => u -> LocGraphic u
strokedDisk radius = strokedEllipse radius radius

-- | Filled disk...
--
filledDisk :: Num u => u -> LocGraphic u
filledDisk radius = filledEllipse radius radius

-- | bordered disk...
--
borderedDisk :: Num u => u -> LocGraphic u
borderedDisk radius = borderedEllipse radius radius
