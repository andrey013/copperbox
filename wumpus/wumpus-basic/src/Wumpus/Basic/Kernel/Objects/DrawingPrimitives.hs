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
  , straightLineGraphic
  , curveGraphic

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

  , strokedEllipseDisk
  , filledEllipseDisk
  , borderedEllipseDisk

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

-- | 'locPath' : @ [next_vector] -> (Point2 ~> PrimPath) @
--
-- Create a path 'LocCF' - i.e. a functional type 
-- /from Point to PrimPath/.
-- 
-- This is the analogue to 'vectorPath' in @Wumpus-Core@, but the 
-- result is produced /within/ the 'DrawingContext'.
--
locPath :: Num u => [Vec2 u] -> LocCF u (PrimPath u)
locPath vs = promoteR1 $ \pt  -> pure $ vectorPath pt vs


-- | 'emptyLocPath' : @ (Point ~> PrimPath) @
--
-- Create an empty path 'LocCF' - i.e. a functional type 
-- /from Point to PrimPath/.
--
-- This is the analogue to 'emptyPath' in @Wumpus-Core@, but the
-- result is produced /within/ the 'DrawingContext'.
--
emptyLocPath :: Num u => LocCF u (PrimPath u)
emptyLocPath = locPath []




--
-- Drawing paths (stroke, fill, bordered)...
--

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
-- This is the analogue to 'textlabel' in @Wumpus-core@, but the
-- text properties (font family, font size, colour) are taken from
-- the implicit 'DrawingContext'.
--
textline :: PtSize u => String -> LocGraphic u
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
rtextline :: PtSize u => String -> LocThetaGraphic u
rtextline ss = 
    promoteR2 $ \pt theta -> 
      withTextAttr $ \rgb attr -> graphicAns (rtextlabel rgb attr ss theta pt)



-- | 'escapedline' : @ escaped_text -> LocGraphic @
-- 
-- Create a text 'LocGraphic' - i.e. a functional type 
-- /from Point to Graphic/.
--
-- The implicit point of the LocGraphic is the baseline left.
--
-- This is the analogue to 'escapedlabel' in @Wumpus-core@, but 
-- the text properties (font family, font size, colour) are taken 
-- from the implicit 'DrawingContext'.
--
escapedline :: PtSize u => EscapedText -> LocGraphic u
escapedline ss = 
    promoteR1 $ \pt -> 
      withTextAttr $ \rgb attr -> graphicAns (escapedlabel rgb attr ss pt)



-- | 'rescapedline' : @ escaped_text -> LocThetaGraphic @
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
-- This is the analogue to 'rescapedlabel' in @Wumpus-core@, but
-- the text properties (font family, font size, colour) are taken 
-- from the implicit 'DrawingContext'.
--
rescapedline :: PtSize u => EscapedText -> LocThetaGraphic u
rescapedline ss = 
    promoteR2 $ \pt theta -> 
      withTextAttr $ \rgb attr -> graphicAns (rescapedlabel rgb attr ss theta pt)




-- | 'hkernline' : @ [kern_char] -> LocGraphic @
-- 
-- Create a horizontally kerned text 'LocGraphic' - i.e. a 
-- functional type /from Point to Graphic/.
--
-- The implicit point of the LocGraphic is the baseline left.
--
-- This is the analogue to 'hkernlabel' in @Wumpus-core@, but 
-- the text properties (font family, font size, colour) are taken 
-- from the implicit 'DrawingContext'.
--
hkernline :: PtSize u => [KerningChar u] -> LocGraphic u
hkernline xs = 
    promoteR1 $ \pt -> 
      withTextAttr $ \rgb attr -> graphicAns (hkernlabel rgb attr xs pt)


-- | 'vkernline' : @ [kern_char] -> LocGraphic @
-- 
-- Create a vertically kerned text 'LocGraphic' - i.e. a 
-- functional type /from Point to Graphic/.
--
-- The implicit point of the LocGraphic is the baseline left.
--
-- This is the analogue to 'vkernlabel' in @Wumpus-core@, but 
-- the text properties (font family, font size, colour) are taken 
-- from the implicit 'DrawingContext'.
--
vkernline :: PtSize u => [KerningChar u] -> LocGraphic u
vkernline xs = 
    promoteR1 $ \pt -> 
      withTextAttr $ \rgb attr -> graphicAns (vkernlabel rgb attr xs pt)




--------------------------------------------------------------------------------
-- Lines

-- | 'straightLine' : @ vec_to -> LocGraphic @ 
--
-- Create a stright line 'LocGraphic' - i.e. a functional type 
-- /from Point to Graphic/.
--
-- The implicit point of the LocGraphic is the start point, the 
-- end point is calculated by displacing the start point with the 
-- supplied vector.
--
-- The line properties (colour, pen thickness, etc.) are taken 
-- from the implicit 'DrawingContext'.
-- 
straightLine :: Fractional u => Vec2 u -> LocGraphic u
straightLine v = mf >>= (lift0R1 . openStroke)
  where
    mf = promoteR1 $ \pt -> pure $ primPath pt [lineTo $ pt .+^ v]

          
-- | 'straightLineGraphic' : @ start_point * end_point -> LocGraphic @ 
-- 
-- Create a straight line 'Graphic', the start and end point 
-- are supplied explicitly.
-- 
-- The line properties (colour, pen thickness, etc.) are taken 
-- from the implicit 'DrawingContext'.
-- 
straightLineGraphic :: Fractional u => Point2 u -> Point2 u -> Graphic u
straightLineGraphic p1 p2 = openStroke $ primPath p1 [lineTo p2]



-- | 'curveGraphic' : @ start_point * control_point1 * 
--        control_point2 * end_point -> Graphic @ 
-- 
-- Create a Bezier curve 'Graphic', all control points are 
-- supplied explicitly.
-- 
-- The line properties (colour, pen thickness, etc.) are taken 
-- from the implicit 'DrawingContext'.
-- 
curveGraphic :: Fractional u 
             => Point2 u -> Point2 u -> Point2 u -> Point2 u -> Graphic u
curveGraphic sp cp1 cp2 ep = openStroke $ primPath sp [curveTo cp1 cp2 ep]


--------------------------------------------------------------------------------
-- Circles

-- | 'strokedCircle' : @ radius -> LocGraphic @
--
-- Create a stroked circle 'LocGraphic' - the implicit point is 
-- center. The circle is drawn with four Bezier curves. 
-- 
-- The line properties (colour, pen thickness, etc.) are taken 
-- from the implicit 'DrawingContext'.
-- 
strokedCircle :: Floating u => u -> LocGraphic u
strokedCircle r = promoteR1 (closedStroke . curvedPath . bezierCircle r)



-- | 'filledCircle' : @ radius -> LocGraphic @
--
-- Create a filled circle 'LocGraphic' - the implicit point is 
-- center. The circle is drawn with four Bezier curves. 
-- 
-- The fill colour is taken from the implicit 'DrawingContext'.
-- 
filledCircle :: Floating u => u -> LocGraphic u
filledCircle r =  promoteR1 (filledPath . curvedPath . bezierCircle r)



-- | 'borderedCircle' : @ radius -> LocGraphic @
--
-- Create a bordered circle 'LocGraphic' - the implicit point is 
-- center. The circle is drawn with four Bezier curves. 
-- 
-- The background fill colour and the outline stroke properties 
-- are taken from the implicit 'DrawingContext'.
-- 
borderedCircle :: Floating u => u -> LocGraphic u
borderedCircle r = promoteR1 (borderedPath . curvedPath . bezierCircle r)


--------------------------------------------------------------------------------
-- Ellipses



-- | 'strokedEllipse' : @ x_radius * y_radius -> LocGraphic @
--
-- Create a stroked ellipse 'LocGraphic' - the implicit point is 
-- center. The ellipse is drawn with four Bezier curves. 
-- 
-- The line properties (colour, pen thickness, etc.) are taken 
-- from the implicit 'DrawingContext'.
-- 
strokedEllipse :: Floating u => u -> u -> LocGraphic u
strokedEllipse rx ry =
    promoteR1 (closedStroke . curvedPath . bezierEllipse rx ry)



-- | 'rstrokedEllipse' : @ x_radius * y_radius -> LocThetaGraphic @
--
-- Create a stroked ellipse 'LocThetaGraphic' - the implicit point
-- is center and the angle is rotation about the center. The 
-- ellipse is drawn with four Bezier curves. 
-- 
-- The line properties (colour, pen thickness, etc.) are taken 
-- from the implicit 'DrawingContext'.
-- 
rstrokedEllipse :: (Real u, Floating u) => u -> u -> LocThetaGraphic u
rstrokedEllipse hw hh = 
    promoteR2 $ \ pt theta -> 
      closedStroke $ curvedPath $ rbezierEllipse hw hh theta pt 


-- | 'filledEllipse' : @ x_radius * y_radius -> LocGraphic @
--
-- Create a filled ellipse 'LocGraphic' - the implicit point is 
-- center. The ellipse is drawn with four Bezier curves. 
-- 
-- The fill colour is taken from the implicit 'DrawingContext'.
-- 
filledEllipse :: Floating u => u -> u -> LocGraphic u
filledEllipse hw hh = 
    promoteR1 (filledPath . curvedPath . bezierEllipse hw hh)


-- | 'rfilledEllipse' : @ x_radius * y_radius -> LocGraphic @
--
-- Create a filled ellipse 'LocThetaGraphic' - the implicit point
-- is center and the angle is rotation about the center. The 
-- ellipse is drawn with four Bezier curves.  
-- 
-- The fill colour is taken from the implicit 'DrawingContext'.
-- 
rfilledEllipse :: (Real u, Floating u) => u -> u -> LocThetaGraphic u
rfilledEllipse hw hh = 
    promoteR2 $ \ pt theta -> 
      filledPath $ curvedPath $ rbezierEllipse hw hh theta pt 



-- | 'borderedEllipse' : @ x_radius * y_radius -> LocGraphic @
--
-- Create a bordered ellipse 'LocGraphic' - the implicit point is 
-- center. The ellipse is drawn with four Bezier curves. 
-- 
-- The background fill colour and the outline stroke properties 
-- are taken from the implicit 'DrawingContext'.
-- 
borderedEllipse :: Floating u => u -> u -> LocGraphic u
borderedEllipse hw hh =
    promoteR1 (borderedPath . curvedPath . bezierEllipse hw hh)



-- | 'rborderedEllipse' : @ x_radius * y_radius -> LocGraphic @
--
-- Create a bordered ellipse 'LocThetaGraphic' - the implicit point
-- is center and the angle is rotation about the center. The 
-- ellipse is drawn with four Bezier curves.  
-- 
-- The background fill colour and the outline stroke properties 
-- are taken from the implicit 'DrawingContext'.
-- 
rborderedEllipse :: (Real u, Floating u) => u -> u -> LocThetaGraphic u
rborderedEllipse hw hh = 
    promoteR2 $ \ pt theta -> 
      borderedPath $ curvedPath $ rbezierEllipse hw hh theta pt 



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


-- | 'strokedRectangle' : @ width * height -> LocGraphic @
--
-- Create a stroked rectangle 'LocGraphic' - the implicit point is 
-- bottom-left. 
-- 
-- The line properties (colour, pen thickness, etc.) are taken 
-- from the implicit 'DrawingContext'.
-- 
strokedRectangle :: Fractional u => u -> u -> LocGraphic u
strokedRectangle w h = promoteR1 (closedStroke . rectanglePath w h)


-- | 'filledRectangle' : @ width * height -> LocGraphic @
--
-- Create a filled rectangle 'LocGraphic' - the implicit point is 
-- the bottom-left. 
-- 
-- The fill colour is taken from the implicit 'DrawingContext'.
-- 
filledRectangle :: Fractional u => u -> u -> LocGraphic u
filledRectangle w h = promoteR1 (filledPath . rectanglePath w h)


-- | 'borderedRectangle' : @ width * height -> LocGraphic @
--
-- Create a bordered rectangle 'LocGraphic' - the implicit point is 
-- bottom-left. 
-- 
-- The background fill colour and the outline stroke properties 
-- are taken from the implicit 'DrawingContext'.
-- 
borderedRectangle :: Fractional u => u -> u -> LocGraphic u
borderedRectangle w h = promoteR1 (borderedPath . rectanglePath w h)


---------------------------------------------------------------------------


-- | 'strokedDisk' : @ radius -> LocGraphic @
--
-- Create a stroked circle 'LocGraphic' - the implicit point is 
-- the center. 
-- 
-- This is a efficient representation of circles using 
-- PostScript\'s @arc@ or SVG\'s @circle@ in the generated 
-- output. However, stroked-circles do not draw well after 
-- non-uniform scaling - the pen width is scaled as well as 
-- the shape.
--
-- For stroked circles that can be adequately scaled, use 
-- 'strokedCircle' instead.
--
-- The line properties (colour, pen thickness, etc.) are taken 
-- from the implicit 'DrawingContext'.
-- 
strokedDisk :: PtSize u => u -> LocGraphic u
strokedDisk r = strokedEllipseDisk r r


-- | 'filledDisk' : @ radius -> LocGraphic @
--
-- Create a filled circle 'LocGraphic' - the implicit point is 
-- the center. 
-- 
-- This is a efficient representation of circles using 
-- PostScript\'s @arc@ or SVG\'s @circle@ in the generated 
-- output. As the circle is filled rather than drawn with a 
-- \"pen\" a @filledDisk@ can be scaled. 
--
-- The fill colour is taken from the implicit 'DrawingContext'.
-- 
filledDisk :: PtSize u => u -> LocGraphic u
filledDisk r = filledEllipseDisk r r


-- | 'borderedDisk' : @ radius -> LocGraphic @
--
-- Create a bordered circle 'LocGraphic' - the implicit point is 
-- the center. 
-- 
-- This is a efficient representation of circles using 
-- PostScript\'s @arc@ or SVG\'s @circle@ in the generated 
-- output. However, bordereded circles do not draw well after 
-- non-uniform scaling - the pen width of the outline is scaled as 
-- well as the shape.
--
-- For bordered circles that can be adequately scaled, use 
-- 'borderedCircle' instead.
--
-- The background fill colour and the outline stroke properties 
-- are taken from the implicit 'DrawingContext'.
-- 
borderedDisk :: PtSize u => u -> LocGraphic u
borderedDisk r = borderedEllipseDisk r r


-- | 'strokedEllipseDisk' : @ x_radius * y_radius -> LocGraphic @
--
-- Create a stroked ellipse 'LocGraphic' - the implicit point is 
-- the center. 
-- 
-- This is a efficient representation of circles using 
-- PostScript\'s @arc@ or SVG\'s @ellipse@ in the generated 
-- output. However, stroked ellipses do not draw well after 
-- non-uniform scaling - the pen width is scaled as well as 
-- the shape.
--
-- For stroked ellipses that can be adequately scaled, use 
-- 'strokedEllipse' instead.
--
-- The line properties (colour, pen thickness, etc.) are taken 
-- from the implicit 'DrawingContext'.
-- 
strokedEllipseDisk :: PtSize u => u -> u -> LocGraphic u
strokedEllipseDisk rx ry =
    promoteR1 $ \ pt -> 
      withStrokeAttr $ \rgb attr -> 
        graphicAns (strokeEllipse rgb attr rx ry pt)


-- | 'filledEllipseDisk' : @ x_radius * y_radius -> LocGraphic @
--
-- Create a filled ellipse 'LocGraphic' - the implicit point is 
-- the center. 
-- 
-- This is a efficient representation of ellipses using 
-- PostScript\'s @arc@ or SVG\'s @ellipse@ in the generated 
-- output. As the ellipse is filled rather than drawn with a 
-- \"pen\" a @filledEllipseDisk@ can be scaled. 
--
-- The fill colour is taken from the implicit 'DrawingContext'.
-- 
filledEllipseDisk :: PtSize u => u -> u -> LocGraphic u
filledEllipseDisk rx ry = 
    promoteR1 $ \pt ->  
      withFillAttr $ \rgb -> graphicAns (fillEllipse rgb rx ry pt)


-- | 'borderedEllipseDisk' : @ x_radius * y_radius -> LocGraphic @
--
-- Create a bordered ellipse 'LocGraphic' - the implicit point is 
-- the center. 
-- 
-- This is a efficient representation of ellipses using 
-- PostScript\'s @arc@ or SVG\'s @ellipse@ in the generated 
-- output. However, bordereded ellipses do not draw well after 
-- non-uniform scaling - the pen width of the outline is scaled as 
-- well as the shape.
--
-- For bordered ellipses that can be adequately scaled, use 
-- 'borderedEllipse' instead.
--
-- The background fill colour and the outline stroke properties 
-- are taken from the implicit 'DrawingContext'.
-- 
borderedEllipseDisk :: PtSize u => u -> u -> LocGraphic u
borderedEllipseDisk rx ry = 
    promoteR1 $ \pt -> 
      withBorderedAttr $ \frgb attr srgb -> 
        graphicAns (fillStrokeEllipse frgb attr srgb rx ry pt)
