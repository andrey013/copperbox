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

    wrapTrafo

  -- * Paths
  , locPathAU
  , locPathRU
  , emptyLocPathAU
  , emptyLocPathRU

  , vertexPathAU
  , vertexPathRU
  , curvedPathAU
  , curvedPathRU

  , openStroke
  , closedStroke
  , filledPath
  , borderedPath

  -- * Text
  , textlineAU
  , textlineRU
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
  , strokedEllipseAU
  , strokedEllipseRU
  , rstrokedEllipseAU
  , rstrokedEllipseRU
  , filledEllipseAU
  , filledEllipseRU
  , rfilledEllipseAU
  , rfilledEllipseRU
  , borderedEllipseAU
  , borderedEllipseRU
  , rborderedEllipseAU
  , rborderedEllipseRU

  -- * Rectangles
  , strokedRectangle
  , filledRectangle
  , borderedRectangle

  -- * Disks  
  , strokedDisk
  , filledDiskAU
  , filledDiskRU
  , borderedDisk

  , strokedEllipseDisk
  , filledEllipseDiskAU
  , filledEllipseDiskRU
  , borderedEllipseDisk

  ) where

import Wumpus.Basic.Kernel.Base.BaseDefs
import Wumpus.Basic.Kernel.Base.ContextFun
import Wumpus.Basic.Kernel.Base.DrawingContext
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


-- Note - Affine transformations are cannot be applied to objects
-- where we only know CtxSize.
--

wrapTrafo :: (DrawingCtxM m, Functor t, CtxSize u) 
          => (t Double -> t Double) -> t u -> m (t u)
wrapTrafo fn obj = dsizeF obj >>= \a -> usizeF (fn a)


ctxStartPoint :: CtxSize u => (Point2 Double -> CF a) -> LocCF u a
ctxStartPoint mf = promoteR1 $ \pt -> dsizeF pt >>= mf

ctxStartPointTheta :: CtxSize u 
                   => (Point2 Double -> Radian -> CF a) -> LocThetaCF u a
ctxStartPointTheta mf = promoteR2 $ \pt ang -> dsizeF pt >>= \pt1 -> mf pt1 ang


dPoint :: PsDouble u => Point2 u -> Point2 Double
dPoint = fmap toPsDouble

dVec :: PsDouble u => Vec2 u -> Vec2 Double
dVec = fmap toPsDouble

--------------------------------------------------------------------------------
-- Paths


-- | 'locPathAU' : @ [next_vector] -> (Point2 ~> PrimPath) @
--
-- Create a path 'LocCF' - i.e. a functional type 
-- /from Point to PrimPath/.
-- 
-- This is the analogue to 'vectorPath' in @Wumpus-Core@, but the 
-- result is produced /within/ the 'DrawingContext'.
--
locPathAU :: PsDouble u => [Vec2 u] -> LocCF u PrimPath
locPathAU vs = promoteR1 $ \pt  ->
    return $ vectorPrimPath (dPoint pt) (map dVec vs)


-- | 'locPathRU' : @ [next_vector] -> (Point2 ~> PrimPath) @
--
-- Create a path 'LocCF' - i.e. a functional type 
-- /from Point to PrimPath/.
-- 
-- This is the analogue to 'vectorPath' in @Wumpus-Core@, but the 
-- result is produced /within/ the 'DrawingContext'.
--
locPathRU :: CtxSize u => [Vec2 u] -> LocCF u PrimPath
locPathRU vs = ctxStartPoint $ \pt1  ->
    mapM dsizeF vs >>= \vs1 ->
    return $ vectorPrimPath pt1 vs1


-- | 'emptyLocPathAU' : @ (Point ~> PrimPath) @
--
-- Create an empty path 'LocCF' - i.e. a functional type 
-- /from Point to PrimPath/.
--
-- This is the analogue to 'emptyPath' in @Wumpus-Core@, but the
-- result is produced /within/ the 'DrawingContext'.
--
emptyLocPathAU :: PsDouble u => LocCF u PrimPath
emptyLocPathAU = locPathAU []


-- | 'emptyLocPathRU' : @ (Point ~> PrimPath) @
--
-- Create an empty path 'LocCF' - i.e. a functional type 
-- /from Point to PrimPath/.
--
-- This is the analogue to 'emptyPath' in @Wumpus-Core@, but the
-- result is produced /within/ the 'DrawingContext'.
--
emptyLocPathRU :: CtxSize u => LocCF u PrimPath
emptyLocPathRU = locPathRU []


-- | 'vertexPathAU' : @ (Point ~> PrimPath) @
--
-- Create a path made of straight line segments joining the 
-- supplied points.
--
-- This is the analogue to 'vertexPrimPath' in @Wumpus-Core@, but 
-- it is polymorphic on unit.
--
vertexPathAU :: PsDouble u => [Point2 u] -> PrimPath
vertexPathAU xs = vertexPrimPath $ map (fmap toPsDouble) xs



-- | 'vertexPathRU' : @ (Point ~> PrimPath) @
--
-- Create a path made of straight line segments joining the 
-- supplied points.
--
-- This is the analogue to 'vertexPrimPath' in @Wumpus-Core@, but 
-- it is polymorphic on unit.
--
vertexPathRU :: CtxSize u => [Point2 u] -> DrawingInfo PrimPath
vertexPathRU xs = vertexPrimPath <$> mapM dsizeF xs




-- | 'vertexPathAU' : @ (Point ~> PrimPath) @
--
-- Create a path made of straight line segments joining the 
-- supplied points.
--
-- This is the analogue to 'curvedPrimPath' in @Wumpus-Core@, but 
-- it is polymorphic on unit.
--
curvedPathAU :: PsDouble u => [Point2 u] -> PrimPath
curvedPathAU xs = curvedPrimPath $ map (fmap toPsDouble) xs



-- | 'vertexPathRU' : @ (Point ~> PrimPath) @
--
-- Create a path made of Bezier curve segments joining the 
-- supplied points.
--
-- This is the analogue to 'curvedPrimPath' in @Wumpus-Core@, but 
-- it is polymorphic on unit.
--
curvedPathRU :: CtxSize u => [Point2 u] -> DrawingInfo PrimPath
curvedPathRU xs = curvedPrimPath <$> mapM dsizeF xs

--------------------------------------------------------------------------------

--
-- Drawing paths (stroke, fill, bordered)...
--

-- | 'openStroke' : @ path -> Graphic @
--
-- This is the analogue to 'ostroke' in @Wumpus-core@, but the 
-- drawing properties (colour, line width, etc.) are taken from 
-- the implicit 'DrawingContext'.
--
openStroke :: PrimPath -> Graphic u
openStroke pp = 
    withStrokeAttr $ \rgb attr -> graphicAns $ ostroke rgb attr pp


-- | 'closedStroke' : @ path -> Graphic @
--
-- This is the analogue to 'cstroke' in @Wumpus-core@, but the 
-- drawing properties (colour, line width, etc.) are taken from 
-- the implicit 'DrawingContext'.
--
closedStroke :: PrimPath -> Graphic u
closedStroke pp = 
    withStrokeAttr $ \rgb attr -> graphicAns $ cstroke rgb attr pp


-- | 'filledPath' : @ path -> Graphic @
-- 
-- This is the analogue to 'fill' in @Wumpus-core@, but the 
-- fill colour is taken from the implicit 'DrawingContext'.
--
--
filledPath :: PrimPath -> Graphic u
filledPath pp = withFillAttr $ \rgb -> graphicAns $ fill rgb pp
                 

-- | 'borderedPath' : @ path -> Graphic @
--
-- This is the analogue to 'fillStroke' in @Wumpus-core@, but the 
-- drawing properties (fill colour, border colour, line width, 
-- etc.) are taken from the implicit 'DrawingContext'.
--
--
borderedPath :: PrimPath -> Graphic u
borderedPath pp =
    withBorderedAttr $ \frgb attr srgb -> 
      graphicAns $ fillStroke frgb attr srgb pp



--------------------------------------------------------------------------------
-- Text


-- | 'textlineAU' : @ string -> LocGraphic @
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
textlineAU :: PsDouble u => String -> LocGraphic u
textlineAU ss = promoteR1 $ \pt -> 
    withTextAttr $ \rgb attr -> graphicAns (textlabel rgb attr ss (dPoint pt))


-- | 'textlineRU' : @ string -> LocGraphic @
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
textlineRU :: CtxSize u => String -> LocGraphic u
textlineRU ss = ctxStartPoint $ \pt -> 
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
rtextline :: CtxSize u => String -> LocThetaGraphic u
rtextline ss = ctxStartPointTheta $ \pt theta -> 
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
escapedline :: CtxSize u => EscapedText -> LocGraphic u
escapedline ss = ctxStartPoint $ \pt -> 
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
rescapedline :: CtxSize u => EscapedText -> LocThetaGraphic u
rescapedline ss = ctxStartPointTheta $ \pt theta -> 
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
hkernline :: CtxSize u => [KerningChar] -> LocGraphic u
hkernline xs = ctxStartPoint $ \pt -> 
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
vkernline :: CtxSize u => [KerningChar] -> LocGraphic u
vkernline xs = 
    promoteR1 $ \pt -> 
      dsizeF pt >>= \pt1 -> 
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
straightLine :: (Fractional u, CtxSize u) 
             => Vec2 u -> LocGraphic u
straightLine v = promoteR1 $ \pt ->
    dsizeF v  >>= \v1 ->
    dsizeF pt >>= \pt1 ->
    openStroke $ vectorPrimPath pt1 [v1]

          
-- | 'straightLineGraphic' : @ start_point * end_point -> LocGraphic @ 
-- 
-- Create a straight line 'Graphic', the start and end point 
-- are supplied explicitly.
-- 
-- The line properties (colour, pen thickness, etc.) are taken 
-- from the implicit 'DrawingContext'.
-- 
straightLineGraphic :: (Fractional u, CtxSize u) 
                    => Point2 u -> Point2 u -> Graphic u
straightLineGraphic p1 p2 = 
    dsizeF p1 >>= \pt1 ->
    dsizeF p2 >>= \pt2 ->
    openStroke $ primPath pt1 [absLineTo pt2]



-- | 'curveGraphic' : @ start_point * control_point1 * 
--        control_point2 * end_point -> Graphic @ 
-- 
-- Create a Bezier curve 'Graphic', all control points are 
-- supplied explicitly.
-- 
-- The line properties (colour, pen thickness, etc.) are taken 
-- from the implicit 'DrawingContext'.
-- 
curveGraphic :: (Fractional u, CtxSize u)
             => Point2 u -> Point2 u -> Point2 u -> Point2 u -> Graphic u
curveGraphic p0 p1 p2 p3 = 
    dsizeF p0 >>= \pt0 ->
    dsizeF p1 >>= \pt1 ->
    dsizeF p2 >>= \pt2 ->
    dsizeF p3 >>= \pt3 ->    
    openStroke $ primPath pt0 [absCurveTo pt1 pt2 pt3]


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
strokedCircle :: (Floating u, CtxSize u) => u -> LocGraphic u
strokedCircle r = promoteR1 $ \pt -> 
    dsize r   >>= \r1 ->
    dsizeF pt >>= \pt1 -> 
    closedStroke $ curvedPrimPath $ bezierCircle r1 pt1



-- | 'filledCircle' : @ radius -> LocGraphic @
--
-- Create a filled circle 'LocGraphic' - the implicit point is 
-- center. The circle is drawn with four Bezier curves. 
-- 
-- The fill colour is taken from the implicit 'DrawingContext'.
-- 
filledCircle :: (Floating u, CtxSize u) => u -> LocGraphic u
filledCircle r =  promoteR1 $ \pt -> 
    dsize r   >>= \r1 ->
    dsizeF pt >>= \pt1 -> 
    filledPath $ curvedPrimPath $ bezierCircle r1 pt1



-- | 'borderedCircle' : @ radius -> LocGraphic @
--
-- Create a bordered circle 'LocGraphic' - the implicit point is 
-- center. The circle is drawn with four Bezier curves. 
-- 
-- The background fill colour and the outline stroke properties 
-- are taken from the implicit 'DrawingContext'.
-- 
borderedCircle :: (Floating u, CtxSize u) => u -> LocGraphic u
borderedCircle r = promoteR1 $ \pt -> 
    dsize r   >>= \r1 ->
    dsizeF pt >>= \pt1 ->    
    borderedPath $ curvedPrimPath $ bezierCircle r1 pt1


--------------------------------------------------------------------------------
-- Ellipses

ptEllipse :: PsDouble u 
          => (PrimPath -> Graphic u) -> u -> u -> LocGraphic u 
ptEllipse fn rx ry = promoteR1 $ \pt ->
    fn $ curvedPrimPath $ bezierEllipse (toPsDouble rx) (toPsDouble ry) (dPoint pt)

rptEllipse :: PsDouble u 
           => (PrimPath -> Graphic u) -> u -> u -> LocThetaGraphic u 
rptEllipse fn rx ry = promoteR2 $ \pt ang ->
    fn $ curvedPrimPath $ rbezierEllipse (toPsDouble rx) (toPsDouble ry) ang (dPoint pt)


ctxEllipse :: CtxSize u 
           => (PrimPath -> Graphic u) -> u -> u -> LocGraphic u 
ctxEllipse fn rx ry = ctxStartPoint $ \pt ->
    dsize rx  >>= \rx1 ->
    dsize ry  >>= \ry1 ->
    fn $ curvedPrimPath $ bezierEllipse rx1 ry1 pt

rctxEllipse :: CtxSize u 
           => (PrimPath -> Graphic u) -> u -> u -> LocThetaGraphic u 
rctxEllipse fn rx ry = ctxStartPointTheta $ \pt ang ->
    dsize rx  >>= \rx1 ->
    dsize ry  >>= \ry1 ->
    fn $ curvedPrimPath $ rbezierEllipse rx1 ry1 ang pt



-- | 'strokedEllipse' : @ x_radius * y_radius -> LocGraphic @
--
-- Create a stroked ellipse 'LocGraphic' - the implicit point is 
-- center. The ellipse is drawn with four Bezier curves. 
-- 
-- The line properties (colour, pen thickness, etc.) are taken 
-- from the implicit 'DrawingContext'.
-- 
strokedEllipseAU :: PsDouble u => u -> u -> LocGraphic u
strokedEllipseAU = ptEllipse closedStroke

-- | Relative unit version of 'strokedEllipseAU'.
--
strokedEllipseRU :: CtxSize u => u -> u -> LocGraphic u
strokedEllipseRU = ctxEllipse closedStroke



-- | 'rstrokedEllipse' : @ x_radius * y_radius -> LocThetaGraphic @
--
-- Create a stroked ellipse 'LocThetaGraphic' - the implicit point
-- is center and the angle is rotation about the center. The 
-- ellipse is drawn with four Bezier curves. 
-- 
-- The line properties (colour, pen thickness, etc.) are taken 
-- from the implicit 'DrawingContext'.
-- 
rstrokedEllipseAU :: PsDouble u
                  => u -> u -> LocThetaGraphic u
rstrokedEllipseAU = rptEllipse closedStroke 

-- | Relative unit version of 'rstrokedEllipseAU'.
--
rstrokedEllipseRU :: CtxSize u
                  => u -> u -> LocThetaGraphic u
rstrokedEllipseRU = rctxEllipse closedStroke 



-- | 'filledEllipse' : @ x_radius * y_radius -> LocGraphic @
--
-- Create a filled ellipse 'LocGraphic' - the implicit point is 
-- center. The ellipse is drawn with four Bezier curves. 
-- 
-- The fill colour is taken from the implicit 'DrawingContext'.
-- 
filledEllipseAU :: PsDouble u => u -> u -> LocGraphic u
filledEllipseAU = ptEllipse filledPath

filledEllipseRU :: CtxSize u => u -> u -> LocGraphic u
filledEllipseRU = ctxEllipse filledPath


-- | 'rfilledEllipse' : @ x_radius * y_radius -> LocGraphic @
--
-- Create a filled ellipse 'LocThetaGraphic' - the implicit point
-- is center and the angle is rotation about the center. The 
-- ellipse is drawn with four Bezier curves.  
-- 
-- The fill colour is taken from the implicit 'DrawingContext'.
-- 
rfilledEllipseAU :: PsDouble u => u -> u -> LocThetaGraphic u
rfilledEllipseAU = rptEllipse filledPath

-- | Relative ...
-- 
rfilledEllipseRU :: CtxSize u => u -> u -> LocThetaGraphic u
rfilledEllipseRU = rctxEllipse filledPath



-- | 'borderedEllipse' : @ x_radius * y_radius -> LocGraphic @
--
-- Create a bordered ellipse 'LocGraphic' - the implicit point is 
-- center. The ellipse is drawn with four Bezier curves. 
-- 
-- The background fill colour and the outline stroke properties 
-- are taken from the implicit 'DrawingContext'.
-- 
borderedEllipseAU :: PsDouble u => u -> u -> LocGraphic u
borderedEllipseAU = ptEllipse borderedPath

-- | Relative ...
--
borderedEllipseRU :: CtxSize u => u -> u -> LocGraphic u
borderedEllipseRU = ctxEllipse borderedPath



-- | 'rborderedEllipse' : @ x_radius * y_radius -> LocGraphic @
--
-- Create a bordered ellipse 'LocThetaGraphic' - the implicit point
-- is center and the angle is rotation about the center. The 
-- ellipse is drawn with four Bezier curves.  
-- 
-- The background fill colour and the outline stroke properties 
-- are taken from the implicit 'DrawingContext'.
-- 
rborderedEllipseAU :: PsDouble u
                   => u -> u -> LocThetaGraphic u
rborderedEllipseAU = rptEllipse borderedPath

-- | Relative...
rborderedEllipseRU :: CtxSize u
                   => u -> u -> LocThetaGraphic u
rborderedEllipseRU = rctxEllipse borderedPath



-- Note - clipping to do...

--------------------------------------------------------------------------------
-- Rectangles



-- | Supplied point is /bottom-left/.
--
rectanglePath :: CtxSize u => u -> u -> Point2 u -> DrawingInfo PrimPath
rectanglePath w h bl = 
     dsize w >>= \w1 ->
     dsize h >>= \h1 -> 
     dsizeF bl >>= \bl1 -> 
     let br1 = bl1 .+^ hvec w1
         tr1 = br1 .+^ vvec h1
         tl1 = bl1 .+^ vvec h1 
     in return $ primPath bl1 [ absLineTo br1, absLineTo tr1, absLineTo tl1 ]

    


-- | 'strokedRectangle' : @ width * height -> LocGraphic @
--
-- Create a stroked rectangle 'LocGraphic' - the implicit point is 
-- bottom-left. 
-- 
-- The line properties (colour, pen thickness, etc.) are taken 
-- from the implicit 'DrawingContext'.
-- 
strokedRectangle :: CtxSize u => u -> u -> LocGraphic u
strokedRectangle w h = promoteR1 $ \pt -> 
    rectanglePath w h pt >>= closedStroke


-- | 'filledRectangle' : @ width * height -> LocGraphic @
--
-- Create a filled rectangle 'LocGraphic' - the implicit point is 
-- the bottom-left. 
-- 
-- The fill colour is taken from the implicit 'DrawingContext'.
-- 
filledRectangle :: CtxSize u => u -> u -> LocGraphic u
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
borderedRectangle :: CtxSize u => u -> u -> LocGraphic u
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
strokedDisk :: CtxSize u => u -> LocGraphic u
strokedDisk r = strokedEllipseDisk r r


-- | 'filledDiskAU' : @ radius -> LocGraphic @
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
filledDiskAU :: PsDouble u => u -> LocGraphic u
filledDiskAU r = filledEllipseDiskAU r r

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
filledDiskRU :: CtxSize u => u -> LocGraphic u
filledDiskRU r = filledEllipseDiskRU r r


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
borderedDisk :: CtxSize u => u -> LocGraphic u
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
strokedEllipseDisk :: CtxSize u => u -> u -> LocGraphic u
strokedEllipseDisk rx ry =
    promoteR1 $ \ pt -> 
      dsizeF pt >>= \pt1 -> 
      dsize  rx >>= \rx1 ->
      dsize  ry >>= \ry1 ->
      withStrokeAttr $ \rgb attr -> 
        graphicAns (strokeEllipse rgb attr rx1 ry1 pt1)


-- | 'filledEllipseDiskRU' : @ x_radius * y_radius -> LocGraphic @
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
filledEllipseDiskAU :: PsDouble u => u -> u -> LocGraphic u
filledEllipseDiskAU = ptDrawEllipse $ \rx ry pt -> 
    withFillAttr $ \rgb -> graphicAns (fillEllipse rgb rx ry pt)

-- | 'filledEllipseDiskRU' : @ x_radius * y_radius -> LocGraphic @
--
-- Relative unit version of 'filledEllispeDiskAU'.
--
filledEllipseDiskRU :: CtxSize u => u -> u -> LocGraphic u
filledEllipseDiskRU = ctxDrawEllipse $ \rx ry pt -> 
    withFillAttr $ \rgb -> graphicAns (fillEllipse rgb rx ry pt)


ptDrawEllipse :: PsDouble u 
               => (Double -> Double -> DPoint2 -> Graphic u) 
               -> u -> u -> LocGraphic u
ptDrawEllipse fn rx ry = promoteR1 $ \pt -> 
    fn (toPsDouble rx) (toPsDouble ry) (dPoint pt)


ctxDrawEllipse :: CtxSize u 
               => (Double -> Double -> DPoint2 -> Graphic u) 
               -> u -> u -> LocGraphic u
ctxDrawEllipse fn rx ry = ctxStartPoint $ \pt -> 
    dsize  rx >>= \rx1 ->
    dsize  ry >>= \ry1 ->
    fn rx1 ry1 pt    

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
borderedEllipseDisk :: CtxSize u => u -> u -> LocGraphic u
borderedEllipseDisk rx ry = 
    promoteR1 $ \pt -> 
      dsizeF pt >>= \pt1 -> 
      dsize  rx >>= \rx1 ->
      dsize  ry >>= \ry1 ->
      withBorderedAttr $ \frgb attr srgb -> 
        graphicAns (fillStrokeEllipse frgb attr srgb rx1 ry1 pt1)
