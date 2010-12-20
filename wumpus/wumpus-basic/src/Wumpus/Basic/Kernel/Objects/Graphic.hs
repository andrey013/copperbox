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
-- as required, e.g for fill colour, stroke colur, line width, etc.
--
type Graphic u          = Image u (UNil u)

-- | Alias of 'Graphic' where the unit type is specialized to 
-- Double. 
--
type DGraphic           = Graphic Double


-- | /Originated/ drawing - produce a primitive respective to the 
-- supplied start-point, access the DrawingContext as required.
--
type LocGraphic u       = LocImage u (UNil u)

-- | Alias of 'LocGraphic' where the unit type is specialized to 
-- Double. 
--
type DLocGraphic        = LocGraphic Double




-- | /Originated/ drawing - produce a primitive respective to the 
-- supplied start-point, access the DrawingContext as required.
--
type LocThetaGraphic u       = LocThetaImage u (UNil u)


-- | Alias of 'LocThetaGraphic' where the unit type is specialized 
-- to Double. 
--
type DLocThetaGraphic        = LocThetaGraphic Double



-- | Move the origin of a LocImage with the supplied displacement
-- function.
--
moveOrigin :: PointDisplace u -> LocImage u a -> LocImage u a
moveOrigin f ma = promoteR1 $ \pt -> ma `at` f pt

--------------------------------------------------------------------------------



graphicBody :: Primitive u -> (UNil u, PrimGraphic u)
graphicBody p = (uNil, primGraphic p)


-- | This is the analogue to 'vectorPath' in @Wumpus-core@.
--
locPath :: Num u => [Vec2 u] -> LocCF u (PrimPath u)
locPath vs = promoteR1 $ \pt  -> pure $ vectorPath pt vs


-- | This is the analogue to 'emptyPath' in @Wumpus-core@.
--
emptyLocPath :: Num u => LocCF u (PrimPath u)
emptyLocPath = locPath []

-- | Build an empty LocGraphic - this is a path with a start
-- point but no path segments. 
-- 
-- The 'emptyLocGraphic' It is treated as a /null primitive/ by 
-- @Wumpus-Core@ and is not drawn, although it does generate a 
-- minimum bounding box at the implicit start point.
-- 
emptyLocGraphic :: Num u => LocGraphic u
emptyLocGraphic = emptyLocPath >>= (lift0R1 . openStroke)


-- | 'openStroke' : @ path -> Graphic @
--
-- This is the analogue to 'ostroke' in @Wumpus-core@, but the 
-- drawing properties (colour, line width, etc.) are taken from 
-- the implicit 'DrawingContext'.
--
openStroke :: Num u => PrimPath u -> Graphic u
openStroke pp = 
    withStrokeAttr $ \rgb attr -> graphicBody $ ostroke rgb attr pp


-- | 'closedStroke' : @ path -> Graphic @
--
-- This is the analogue to 'cstroke' in @Wumpus-core@, but the 
-- drawing properties (colour, line width, etc.) are taken from 
-- the implicit 'DrawingContext'.
--
closedStroke :: Num u => PrimPath u -> Graphic u
closedStroke pp = 
    withStrokeAttr $ \rgb attr -> graphicBody $ cstroke rgb attr pp


-- | 'filledPath' : @ path -> Graphic @
-- 
-- This is the analogue to 'fill' in @Wumpus-core@, but the 
-- fill colour is taken from the implicit 'DrawingContext'.
--
--
filledPath :: Num u => PrimPath u -> Graphic u
filledPath pp = withFillAttr $ \rgb -> graphicBody $ fill rgb pp
                 

-- | 'borderedPath' : @ path -> Graphic @
--
-- This is the analogue to 'fillStroke' in @Wumpus-core@, but the 
-- drawing properties (fill colour, border colour, line width, 
-- etc.) are taken from the implicit 'DrawingContext'.
--
--
borderedPath :: Num u => PrimPath u -> Graphic u
borderedPath pp =
    withBorderedAttr $ \frgb attr srgb -> 
      graphicBody $ fillStroke frgb attr srgb pp




-- | This is the analogue to 'textlabel' in @Wumpus-core@.
--
textline :: Num u => String -> LocGraphic u
textline ss = 
    promoteR1 $ \pt -> 
      withTextAttr $ \rgb attr -> graphicBody (textlabel rgb attr ss pt)




-- | This is the analogue to 'rtextlabel' in @Wumpus-core@.
--
rtextline :: Num u => String -> LocThetaGraphic u
rtextline ss = 
    promoteR2 $ \pt theta -> 
      withTextAttr $ \rgb attr -> graphicBody (rtextlabel rgb attr ss theta pt)



-- | This is the analogue to 'escapedlabel' in @Wumpus-core@.
--
escapedline :: Num u => EscapedText -> LocGraphic u
escapedline ss = 
    promoteR1 $ \pt -> 
      withTextAttr $ \rgb attr -> graphicBody (escapedlabel rgb attr ss pt)



-- | This is the analogue to 'rescapedlabel' in @Wumpus-core@.
--
rescapedline :: Num u => EscapedText -> LocThetaGraphic u
rescapedline ss = 
    promoteR2 $ \pt theta -> 
      withTextAttr $ \rgb attr -> graphicBody (rescapedlabel rgb attr ss theta pt)




-- | This is the analogue to 'hkernlabel' in @Wumpus-core@.
--
hkernline :: Num u => [KerningChar u] -> LocGraphic u
hkernline xs = 
    promoteR1 $ \pt -> 
      withTextAttr $ \rgb attr -> graphicBody (hkernlabel rgb attr xs pt)


-- | This is the analogue to 'vkernlabel' in @Wumpus-core@.
--
vkernline :: Num u => [KerningChar u] -> LocGraphic u
vkernline xs = 
    promoteR1 $ \pt -> 
      withTextAttr $ \rgb attr -> graphicBody (vkernlabel rgb attr xs pt)




--------------------------------------------------------------------------------





-- | This is the analogue to 'strokeEllipse' in @Wumpus-core@.
--
strokedEllipse :: Num u => u -> u -> LocGraphic u
strokedEllipse hw hh =
    promoteR1 $ \pt -> 
      withStrokeAttr $ \rgb attr -> graphicBody (strokeEllipse rgb attr hw hh pt)



-- | This is the analogue to 'rstrokeEllispe' in @Wumpus-core@.
--
rstrokedEllipse :: Num u => u -> u -> LocThetaGraphic u
rstrokedEllipse hw hh = 
    promoteR2 $ \ pt theta -> 
      withStrokeAttr $ \rgb attr -> 
        graphicBody (rstrokeEllipse rgb attr hw hh theta pt)


-- | This is the analogue to 'fillEllispe' in @Wumpus-core@.
--
filledEllipse :: Num u => u -> u -> LocGraphic u
filledEllipse hw hh = 
    promoteR1 $ \pt ->  
      withFillAttr $ \rgb -> graphicBody (fillEllipse rgb hw hh pt)


-- | This is the analogue to 'rfillEllispe' in @Wumpus-core@.
--
rfilledEllipse :: Num u => u -> u -> LocThetaGraphic u
rfilledEllipse hw hh = 
    promoteR2 $ \pt theta ->
      withFillAttr $ \rgb -> graphicBody (rfillEllipse rgb hw hh theta pt)



-- | This is the analogue to 'fillStrokeEllispe' in @Wumpus-core@.
--
borderedEllipse :: Num u => u -> u -> LocGraphic u
borderedEllipse hw hh =
    promoteR1 $ \pt -> 
      withBorderedAttr $ \frgb attr srgb -> 
        graphicBody (fillStrokeEllipse frgb attr srgb hw hh pt)

-- | This is the analogue to 'rfillStrokeEllispe' in @Wumpus-core@.
--
rborderedEllipse :: Num u => u -> u -> LocThetaGraphic u
rborderedEllipse hw hh = 
    promoteR2 $ \pt theta -> 
      withBorderedAttr $ \frgb attr srgb -> 
        graphicBody (rfillStrokeEllipse frgb attr srgb hw hh theta pt)



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
drawWith g mf = promoteR1 $ \pt -> mf (g pt)


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
-- For stroked circles that can be adequately scaled, use 
-- 'strokedCircle' instead.
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
