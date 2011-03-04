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

import Wumpus.Basic.Kernel.Base.BaseDefs
import Wumpus.Basic.Kernel.Base.ContextFun
import Wumpus.Basic.Kernel.Base.QueryDC
import Wumpus.Basic.Kernel.Objects.BaseObjects
import Wumpus.Basic.Kernel.Objects.Graphic

import Wumpus.Core                              -- package: wumpus-core

import Data.AffineSpace                         -- package: vector-space

import Control.Applicative



-- Helper
--
graphicAns :: Primitive  -> GraphicAns u 
graphicAns p = imageAns (Const ()) p


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
locPath :: CxSize u => [Vec2 u] -> LocCF u PrimPath
locPath vs = promoteR1 $ \pt  ->
              mapM ctxSizeF vs >>= \vs1 ->
              ctxSizeF pt      >>= \pt1 -> 
              return $ vectorPath pt1 vs1


-- | 'emptyLocPath' : @ (Point ~> PrimPath) @
--
-- Create an empty path 'LocCF' - i.e. a functional type 
-- /from Point to PrimPath/.
--
-- This is the analogue to 'emptyPath' in @Wumpus-Core@, but the
-- result is produced /within/ the 'DrawingContext'.
--
emptyLocPath :: CxSize u => LocCF u PrimPath
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
openStroke :: Num u => PrimPath -> Graphic u
openStroke pp = 
    withStrokeAttr $ \rgb attr -> graphicAns $ ostroke rgb attr pp


-- | 'closedStroke' : @ path -> Graphic @
--
-- This is the analogue to 'cstroke' in @Wumpus-core@, but the 
-- drawing properties (colour, line width, etc.) are taken from 
-- the implicit 'DrawingContext'.
--
closedStroke :: Num u => PrimPath -> Graphic u
closedStroke pp = 
    withStrokeAttr $ \rgb attr -> graphicAns $ cstroke rgb attr pp


-- | 'filledPath' : @ path -> Graphic @
-- 
-- This is the analogue to 'fill' in @Wumpus-core@, but the 
-- fill colour is taken from the implicit 'DrawingContext'.
--
--
filledPath :: Num u => PrimPath -> Graphic u
filledPath pp = withFillAttr $ \rgb -> graphicAns $ fill rgb pp
                 

-- | 'borderedPath' : @ path -> Graphic @
--
-- This is the analogue to 'fillStroke' in @Wumpus-core@, but the 
-- drawing properties (fill colour, border colour, line width, 
-- etc.) are taken from the implicit 'DrawingContext'.
--
--
borderedPath :: Num u => PrimPath -> Graphic u
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
textline :: CxSize u => String -> LocGraphic u
textline ss = 
    promoteR1 $ \pt -> 
      ctxSizeF pt >>= \pt1 -> 
      withTextAttr $ \rgb attr -> graphicAns (textlabel rgb attr ss pt1)




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
rtextline :: CxSize u => String -> LocThetaGraphic u
rtextline ss = promoteR2 $ \pt theta -> 
    ctxSizeF pt >>= \pt1 -> 
    withTextAttr $ \rgb attr -> graphicAns (rtextlabel rgb attr ss theta pt1)



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
escapedline :: CxSize u => EscapedText -> LocGraphic u
escapedline ss = promoteR1 $ \pt -> 
    ctxSizeF pt >>= \pt1 -> 
    withTextAttr $ \rgb attr -> graphicAns (escapedlabel rgb attr ss pt1)



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
rescapedline :: CxSize u => EscapedText -> LocThetaGraphic u
rescapedline ss = promoteR2 $ \pt theta -> 
    ctxSizeF pt >>= \pt1 -> 
    withTextAttr $ \rgb attr -> 
          graphicAns (rescapedlabel rgb attr ss theta pt1)




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
hkernline :: CxSize u => [KerningChar] -> LocGraphic u
hkernline xs = promoteR1 $ \pt -> 
    ctxSizeF pt >>= \pt1 -> 
    withTextAttr $ \rgb attr -> graphicAns (hkernlabel rgb attr xs pt1)


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
vkernline :: CxSize u => [KerningChar] -> LocGraphic u
vkernline xs = 
    promoteR1 $ \pt -> 
      ctxSizeF pt >>= \pt1 -> 
      withTextAttr $ \rgb attr -> graphicAns (vkernlabel rgb attr xs pt1)

-- Design Note - [KerningChar] is the wrong type here, we need one 
-- parametric on unit.
--


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
straightLine :: (Fractional u, CxSize u) 
             => Vec2 u -> LocGraphic u
straightLine v = promoteR1 $ \pt ->
    ctxSizeF v  >>= \v1 ->
    ctxSizeF pt >>= \pt1 ->
    openStroke $ vectorPath pt1 [v1]

          
-- | 'straightLineGraphic' : @ start_point * end_point -> LocGraphic @ 
-- 
-- Create a straight line 'Graphic', the start and end point 
-- are supplied explicitly.
-- 
-- The line properties (colour, pen thickness, etc.) are taken 
-- from the implicit 'DrawingContext'.
-- 
straightLineGraphic :: (Fractional u, CxSize u) 
                    => Point2 u -> Point2 u -> Graphic u
straightLineGraphic p1 p2 = 
    ctxSizeF p1  >>= \pt1 ->
    ctxSizeF p2 >>= \pt2 ->
    openStroke $ primPath pt1 [lineTo pt2]



-- | 'curveGraphic' : @ start_point * control_point1 * 
--        control_point2 * end_point -> Graphic @ 
-- 
-- Create a Bezier curve 'Graphic', all control points are 
-- supplied explicitly.
-- 
-- The line properties (colour, pen thickness, etc.) are taken 
-- from the implicit 'DrawingContext'.
-- 
curveGraphic :: (Fractional u, CxSize u)
             => Point2 u -> Point2 u -> Point2 u -> Point2 u -> Graphic u
curveGraphic p0 p1 p2 p3 = 
    ctxSizeF p0 >>= \pt0 ->
    ctxSizeF p1 >>= \pt1 ->
    ctxSizeF p2 >>= \pt2 ->
    ctxSizeF p3 >>= \pt3 ->    
    openStroke $ primPath pt0 [curveTo pt1 pt2 pt3]


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
strokedCircle :: (Floating u, CxSize u) => u -> LocGraphic u
strokedCircle r = promoteR1 $ \pt -> 
    ctxSize r   >>= \r1 ->
    ctxSizeF pt >>= \pt1 -> 
    closedStroke $ curvedPath $ bezierCircle r1 pt1



-- | 'filledCircle' : @ radius -> LocGraphic @
--
-- Create a filled circle 'LocGraphic' - the implicit point is 
-- center. The circle is drawn with four Bezier curves. 
-- 
-- The fill colour is taken from the implicit 'DrawingContext'.
-- 
filledCircle :: (Floating u, CxSize u) => u -> LocGraphic u
filledCircle r =  promoteR1 $ \pt -> 
    ctxSize r   >>= \r1 ->
    ctxSizeF pt >>= \pt1 -> 
    filledPath $ curvedPath $ bezierCircle r1 pt1



-- | 'borderedCircle' : @ radius -> LocGraphic @
--
-- Create a bordered circle 'LocGraphic' - the implicit point is 
-- center. The circle is drawn with four Bezier curves. 
-- 
-- The background fill colour and the outline stroke properties 
-- are taken from the implicit 'DrawingContext'.
-- 
borderedCircle :: (Floating u, CxSize u) => u -> LocGraphic u
borderedCircle r = promoteR1 $ \pt -> 
    ctxSize r   >>= \r1 ->
    ctxSizeF pt >>= \pt1 ->    
    borderedPath $ curvedPath $ bezierCircle r1 pt1


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
strokedEllipse :: (Floating u, CxSize u) => u -> u -> LocGraphic u
strokedEllipse rx ry = promoteR1 $ \pt -> 
    ctxSize rx  >>= \rx1 ->
    ctxSize ry  >>= \ry1 ->
    ctxSizeF pt >>= \pt1 -> 
    closedStroke $ curvedPath $ bezierEllipse rx1 ry1 pt1



-- | 'rstrokedEllipse' : @ x_radius * y_radius -> LocThetaGraphic @
--
-- Create a stroked ellipse 'LocThetaGraphic' - the implicit point
-- is center and the angle is rotation about the center. The 
-- ellipse is drawn with four Bezier curves. 
-- 
-- The line properties (colour, pen thickness, etc.) are taken 
-- from the implicit 'DrawingContext'.
-- 
rstrokedEllipse :: (Real u, Floating u, CxSize u) 
                => u -> u -> LocThetaGraphic u
rstrokedEllipse rx ry = promoteR2 $ \ pt theta -> 
    ctxSize rx  >>= \rx1 ->
    ctxSize ry  >>= \ry1 ->
    ctxSizeF pt >>= \pt1 -> 
    closedStroke $ curvedPath $ rbezierEllipse rx1 ry1 theta pt1


-- | 'filledEllipse' : @ x_radius * y_radius -> LocGraphic @
--
-- Create a filled ellipse 'LocGraphic' - the implicit point is 
-- center. The ellipse is drawn with four Bezier curves. 
-- 
-- The fill colour is taken from the implicit 'DrawingContext'.
-- 
filledEllipse :: (Floating u, CxSize u) => u -> u -> LocGraphic u
filledEllipse rx ry = promoteR1 $ \pt -> 
    ctxSize rx  >>= \rx1 ->
    ctxSize ry  >>= \ry1 ->
    ctxSizeF pt >>= \pt1 -> 
    filledPath $ curvedPath $ bezierEllipse rx1 ry1 pt1


-- | 'rfilledEllipse' : @ x_radius * y_radius -> LocGraphic @
--
-- Create a filled ellipse 'LocThetaGraphic' - the implicit point
-- is center and the angle is rotation about the center. The 
-- ellipse is drawn with four Bezier curves.  
-- 
-- The fill colour is taken from the implicit 'DrawingContext'.
-- 
rfilledEllipse :: (Real u, Floating u, CxSize u) 
               => u -> u -> LocThetaGraphic u
rfilledEllipse rx ry = promoteR2 $ \ pt theta -> 
    ctxSize rx  >>= \rx1 ->
    ctxSize ry  >>= \ry1 ->
    ctxSizeF pt >>= \pt1 -> 
    filledPath $ curvedPath $ rbezierEllipse rx1 ry1 theta pt1



-- | 'borderedEllipse' : @ x_radius * y_radius -> LocGraphic @
--
-- Create a bordered ellipse 'LocGraphic' - the implicit point is 
-- center. The ellipse is drawn with four Bezier curves. 
-- 
-- The background fill colour and the outline stroke properties 
-- are taken from the implicit 'DrawingContext'.
-- 
borderedEllipse :: (Floating u, CxSize u) => u -> u -> LocGraphic u
borderedEllipse rx ry = promoteR1 $ \pt -> 
    ctxSize rx  >>= \rx1 ->
    ctxSize ry  >>= \ry1 ->
    ctxSizeF pt >>= \pt1 ->     
    borderedPath $ curvedPath $ bezierEllipse rx1 ry1 pt1



-- | 'rborderedEllipse' : @ x_radius * y_radius -> LocGraphic @
--
-- Create a bordered ellipse 'LocThetaGraphic' - the implicit point
-- is center and the angle is rotation about the center. The 
-- ellipse is drawn with four Bezier curves.  
-- 
-- The background fill colour and the outline stroke properties 
-- are taken from the implicit 'DrawingContext'.
-- 
rborderedEllipse :: (Real u, Floating u, CxSize u) 
                 => u -> u -> LocThetaGraphic u
rborderedEllipse hw hh = promoteR2 $ \ pt theta -> 
    ctxSizeF pt >>= \pt1 -> 
    ctxSize  hw >>= \hw1 ->
    ctxSize  hh >>= \hh1 ->
    borderedPath $ curvedPath $ rbezierEllipse hw1 hh1 theta pt1



-- Note - clipping needs a picture as well as a path, so there is
-- no analogous @clippedPath@ function.

--------------------------------------------------------------------------------
-- Rectangles



-- | Supplied point is /bottom-left/.
--
rectanglePath :: CxSize u => u -> u -> Point2 u -> DrawingInfo PrimPath
rectanglePath w h bl = 
     ctxSize w >>= \w1 ->
     ctxSize h >>= \h1 -> 
     ctxSizeF bl >>= \bl1 -> 
     let br1 = bl1 .+^ hvec w1
         tr1 = br1 .+^ vvec h1
         tl1 = bl1 .+^ vvec h1 
     in return $ primPath bl1 [ lineTo br1, lineTo tr1, lineTo tl1 ]

    


-- | 'strokedRectangle' : @ width * height -> LocGraphic @
--
-- Create a stroked rectangle 'LocGraphic' - the implicit point is 
-- bottom-left. 
-- 
-- The line properties (colour, pen thickness, etc.) are taken 
-- from the implicit 'DrawingContext'.
-- 
strokedRectangle :: CxSize u => u -> u -> LocGraphic u
strokedRectangle w h = promoteR1 $ \pt -> 
    rectanglePath w h pt >>= closedStroke


-- | 'filledRectangle' : @ width * height -> LocGraphic @
--
-- Create a filled rectangle 'LocGraphic' - the implicit point is 
-- the bottom-left. 
-- 
-- The fill colour is taken from the implicit 'DrawingContext'.
-- 
filledRectangle :: CxSize u => u -> u -> LocGraphic u
filledRectangle w h = promoteR1 $ \pt -> 
    rectanglePath w h pt >>= filledPath


-- | 'borderedRectangle' : @ width * height -> LocGraphic @
--
-- Create a bordered rectangle 'LocGraphic' - the implicit point is 
-- bottom-left. 
-- 
-- The background fill colour and the outline stroke properties 
-- are taken from the implicit 'DrawingContext'.
-- 
borderedRectangle :: CxSize u => u -> u -> LocGraphic u
borderedRectangle w h = promoteR1 $ \pt -> 
    rectanglePath w h pt >>= borderedPath 


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
strokedDisk :: CxSize u => u -> LocGraphic u
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
filledDisk :: CxSize u => u -> LocGraphic u
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
borderedDisk :: CxSize u => u -> LocGraphic u
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
strokedEllipseDisk :: CxSize u => u -> u -> LocGraphic u
strokedEllipseDisk rx ry =
    promoteR1 $ \ pt -> 
      ctxSizeF pt >>= \pt1 -> 
      ctxSize  rx >>= \rx1 ->
      ctxSize  ry >>= \ry1 ->
      withStrokeAttr $ \rgb attr -> 
        graphicAns (strokeEllipse rgb attr rx1 ry1 pt1)


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
filledEllipseDisk :: CxSize u => u -> u -> LocGraphic u
filledEllipseDisk rx ry = 
    promoteR1 $ \pt ->  
      ctxSizeF pt >>= \pt1 -> 
      ctxSize  rx >>= \rx1 ->
      ctxSize  ry >>= \ry1 ->
      withFillAttr $ \rgb -> graphicAns (fillEllipse rgb rx1 ry1 pt1)


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
borderedEllipseDisk :: CxSize u => u -> u -> LocGraphic u
borderedEllipseDisk rx ry = 
    promoteR1 $ \pt -> 
      ctxSizeF pt >>= \pt1 -> 
      ctxSize  rx >>= \rx1 ->
      ctxSize  ry >>= \ry1 ->
      withBorderedAttr $ \frgb attr srgb -> 
        graphicAns (fillStrokeEllipse frgb attr srgb rx1 ry1 pt1)
