{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Kernel.Objects.DrawingPrimitives
-- Copyright   :  (c) Stephen Tetley 2010-2011
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
--
--------------------------------------------------------------------------------

module Wumpus.Basic.Kernel.Objects.DrawingPrimitives
  (

  -- * Paths
    locPath
  , emptyLocPath
  , emptyLocGraphic


  , openStroke
  , closedStroke
  , filledPath
  , borderedPath

  -- * Text
  , textline
  , rtextline
  , escapedline
  , rescapedline

  , hkernline
  , vkernline

  -- * Lines
  , straightLine
  , straightLineBetween
  , curveBetween

  -- * Circles
  , strokedCircle
  , filledCircle
  , borderedCircle

  -- * Ellipses
  , strokedEllipse
  , rstrokedEllipse
  , filledEllipse
  , rfilledEllipse

  , borderedEllipse
  , rborderedEllipse

  -- * Rectangles
  , strokedRectangle
  , filledRectangle
  , borderedRectangle

  -- * Disks
  
  , strokedDisk
  , filledDisk
  , borderedDisk

  ) where

import Wumpus.Basic.Kernel.Base.ContextFun
import Wumpus.Basic.Kernel.Base.QueryDC
import Wumpus.Basic.Kernel.Base.WrappedPrimitive
import Wumpus.Basic.Kernel.Objects.Graphic

import Wumpus.Core                              -- package: wumpus-core

import Data.AffineSpace                         -- package: vector-space

import Control.Applicative



-- Helper
--
graphicAns :: Primitive u -> (UNil u, PrimGraphic u)
graphicAns p = (uNil, primGraphic p)


--------------------------------------------------------------------------------
-- Paths

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
    withStrokeAttr $ \rgb attr -> graphicAns $ ostroke rgb attr pp


-- | 'closedStroke' : @ path -> Graphic @
--
-- This is the analogue to 'cstroke' in @Wumpus-core@, but the 
-- drawing properties (colour, line width, etc.) are taken from 
-- the implicit 'DrawingContext'.
--
closedStroke :: Num u => PrimPath u -> Graphic u
closedStroke pp = 
    withStrokeAttr $ \rgb attr -> graphicAns $ cstroke rgb attr pp


-- | 'filledPath' : @ path -> Graphic @
-- 
-- This is the analogue to 'fill' in @Wumpus-core@, but the 
-- fill colour is taken from the implicit 'DrawingContext'.
--
--
filledPath :: Num u => PrimPath u -> Graphic u
filledPath pp = withFillAttr $ \rgb -> graphicAns $ fill rgb pp
                 

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
      graphicAns $ fillStroke frgb attr srgb pp



--------------------------------------------------------------------------------
-- Text

-- | 'textline' : @ string -> LocGraphic @
-- 
-- Create a text 'LocGraphic' - i.e. a functional type 
-- /from Point to Graphic/.
--
-- The implicit point of the LocGraphic is the baseline left.
--
-- This is the analogue to 'textlabel' in @Wumpus-core@.
--
textline :: Num u => String -> LocGraphic u
textline ss = 
    promoteR1 $ \pt -> 
      withTextAttr $ \rgb attr -> graphicAns (textlabel rgb attr ss pt)




-- | 'rtextline' : @ string -> LocThetaGraphic @
-- 
-- Create a text 'LocThetaGraphic' - i.e. a functional type 
-- /from Point and Angle to Graphic/.
--
-- The implicit point of the LocGraphic is the baseline left, the
-- implicit angle is rotation factor of the text.
--
-- Note - rotated text often does not render well in PostScript or
-- SVG. Rotated text should be used sparingly.
-- 
-- This is the analogue to 'rtextlabel' in @Wumpus-core@.
--
rtextline :: Num u => String -> LocThetaGraphic u
rtextline ss = 
    promoteR2 $ \pt theta -> 
      withTextAttr $ \rgb attr -> graphicAns (rtextlabel rgb attr ss theta pt)



-- | This is the analogue to 'escapedlabel' in @Wumpus-core@.
--
escapedline :: Num u => EscapedText -> LocGraphic u
escapedline ss = 
    promoteR1 $ \pt -> 
      withTextAttr $ \rgb attr -> graphicAns (escapedlabel rgb attr ss pt)



-- | This is the analogue to 'rescapedlabel' in @Wumpus-core@.
--
rescapedline :: Num u => EscapedText -> LocThetaGraphic u
rescapedline ss = 
    promoteR2 $ \pt theta -> 
      withTextAttr $ \rgb attr -> graphicAns (rescapedlabel rgb attr ss theta pt)




-- | This is the analogue to 'hkernlabel' in @Wumpus-core@.
--
hkernline :: Num u => [KerningChar u] -> LocGraphic u
hkernline xs = 
    promoteR1 $ \pt -> 
      withTextAttr $ \rgb attr -> graphicAns (hkernlabel rgb attr xs pt)


-- | This is the analogue to 'vkernlabel' in @Wumpus-core@.
--
vkernline :: Num u => [KerningChar u] -> LocGraphic u
vkernline xs = 
    promoteR1 $ \pt -> 
      withTextAttr $ \rgb attr -> graphicAns (vkernlabel rgb attr xs pt)




--------------------------------------------------------------------------------
-- Lines

-- | Draw a straight line form the implicit start point displaced 
-- with the supplied vector.
-- 
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


--------------------------------------------------------------------------------
-- Circles

-- | 'strokedCircle' : @ radius -> LocGraphic @
--
-- Supplied point is center. The circle is drawn with four 
-- Bezier curves. 
--
strokedCircle :: Floating u => u -> LocGraphic u
strokedCircle r = promoteR1 (closedStroke . curvedPath . bezierCircle r)



-- | Supplied point is center. Circle is drawn with Bezier 
-- curves. 
--
filledCircle :: Floating u => u -> LocGraphic u
filledCircle r =  promoteR1 (filledPath . curvedPath . bezierCircle r)



-- | Supplied point is center. Circle is drawn with Bezier 
-- curves. 
--
borderedCircle :: Floating u => u -> LocGraphic u
borderedCircle r = promoteR1 (borderedPath . curvedPath . bezierCircle r)


--------------------------------------------------------------------------------
-- Ellipses


-- WARNING - probably want to use a Bezier ellipse here?


-- | This is the analogue to 'strokeEllipse' in @Wumpus-core@.
--
strokedEllipse :: Num u => u -> u -> LocGraphic u
strokedEllipse hw hh =
    promoteR1 $ \pt -> 
      withStrokeAttr $ \rgb attr -> graphicAns (strokeEllipse rgb attr hw hh pt)



-- | This is the analogue to 'rstrokeEllispe' in @Wumpus-core@.
--
rstrokedEllipse :: Num u => u -> u -> LocThetaGraphic u
rstrokedEllipse hw hh = 
    promoteR2 $ \ pt theta -> 
      withStrokeAttr $ \rgb attr -> 
        graphicAns (rstrokeEllipse rgb attr hw hh theta pt)


-- | This is the analogue to 'fillEllispe' in @Wumpus-core@.
--
filledEllipse :: Num u => u -> u -> LocGraphic u
filledEllipse hw hh = 
    promoteR1 $ \pt ->  
      withFillAttr $ \rgb -> graphicAns (fillEllipse rgb hw hh pt)


-- | This is the analogue to 'rfillEllispe' in @Wumpus-core@.
--
rfilledEllipse :: Num u => u -> u -> LocThetaGraphic u
rfilledEllipse hw hh = 
    promoteR2 $ \pt theta ->
      withFillAttr $ \rgb -> graphicAns (rfillEllipse rgb hw hh theta pt)



-- | This is the analogue to 'fillStrokeEllispe' in @Wumpus-core@.
--
borderedEllipse :: Num u => u -> u -> LocGraphic u
borderedEllipse hw hh =
    promoteR1 $ \pt -> 
      withBorderedAttr $ \frgb attr srgb -> 
        graphicAns (fillStrokeEllipse frgb attr srgb hw hh pt)

-- | This is the analogue to 'rfillStrokeEllispe' in @Wumpus-core@.
--
rborderedEllipse :: Num u => u -> u -> LocThetaGraphic u
rborderedEllipse hw hh = 
    promoteR2 $ \pt theta -> 
      withBorderedAttr $ \frgb attr srgb -> 
        graphicAns (rfillStrokeEllipse frgb attr srgb hw hh theta pt)



-- Note - clipping needs a picture as well as a path, so there is
-- no analogous @clippedPath@ function.

--------------------------------------------------------------------------------
-- Rectangles



-- | Supplied point is /bottom-left/.
--
rectanglePath :: Num u => u -> u -> Point2 u -> PrimPath u
rectanglePath w h bl = primPath bl [ lineTo br, lineTo tr, lineTo tl ]
  where
    br = bl .+^ hvec w
    tr = br .+^ vvec h
    tl = bl .+^ vvec h

-- | Supplied point is /bottom left/.
--
strokedRectangle :: Fractional u => u -> u -> LocGraphic u
strokedRectangle w h = promoteR1 (closedStroke . rectanglePath w h)


-- | Supplied point is /bottom left/.
--
filledRectangle :: Fractional u => u -> u -> LocGraphic u
filledRectangle w h = promoteR1 (filledPath . rectanglePath w h)

-- | Supplied point is /bottom left/.
--
borderedRectangle :: Fractional u => u -> u -> LocGraphic u
borderedRectangle w h = promoteR1 (borderedPath . rectanglePath w h)


---------------------------------------------------------------------------



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
