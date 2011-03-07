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

  -- * Empty graphics
  , emptyLocGraphicAU 
  , emptyLocGraphicRU
  , emptyLocThetaGraphic

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
  , rtextlineAU
  , rtextlineRU
  , escapedlineAU
  , escapedlineRU
  , rescapedlineAU
  , rescapedlineRU

  , KernChar
  , hkernlineAU
  , hkernlineRU
  , vkernlineAU
  , vkernlineRU

  -- * Lines
  , straightLineAU
  , straightLineRU
  , locStraightLineAU
  , locStraightLineRU
  , curvedLineAU
  , curvedLineRU

  -- * Circles
  , strokedCircleAU
  , strokedCircleRU
  , filledCircleAU
  , filledCircleRU
  , borderedCircleAU
  , borderedCircleRU

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
  , strokedRectangleAU
  , strokedRectangleRU
  , filledRectangleAU
  , filledRectangleRU
  , borderedRectangleAU
  , borderedRectangleRU

  -- * Disks  
  , strokedDiskAU
  , strokedDiskRU
  , filledDiskAU
  , filledDiskRU
  , borderedDiskAU
  , borderedDiskRU

  , strokedEllipseDiskAU
  , strokedEllipseDiskRU
  , filledEllipseDiskAU
  , filledEllipseDiskRU
  , borderedEllipseDiskAU
  , borderedEllipseDiskRU

  ) where

import Wumpus.Basic.Kernel.Base.BaseDefs
import Wumpus.Basic.Kernel.Base.ContextFun
import Wumpus.Basic.Kernel.Base.DrawingContext
import Wumpus.Basic.Kernel.Base.QueryDC
import Wumpus.Basic.Kernel.Objects.BaseObjects
import Wumpus.Basic.Kernel.Objects.Graphic

import Wumpus.Core                              -- package: wumpus-core


import Control.Applicative



-- Helper
--
makeAns :: Primitive  -> GraphicAns u 
makeAns p = imageAns (Const ()) p


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
-- Empty graphics

-- | 'emptyLocGraphic' : @ LocGraphic @
--
-- Build an empty 'LocGraphic' (i.e. a function 
-- /from Point to Graphic/). This is a path with a start point 
-- but no path segments. 
-- 
-- The 'emptyLocGraphic' is treated as a /null primitive/ by 
-- @Wumpus-Core@ and is not drawn, although it does generate a 
-- minimum bounding box at the implicit start point.
-- 
emptyLocGraphicAU :: PsDouble u => LocGraphic u
emptyLocGraphicAU = emptyLocPathAU >>= (lift0R1 . openStroke)


emptyLocGraphicRU :: CtxSize u => LocGraphic u
emptyLocGraphicRU = emptyLocPathRU >>= (lift0R1 . openStroke)


-- | 'emptyLocThetaGraphic' : @ LocThetaGraphic @
--
-- Build an empty 'LocThetaGraphic' (i.e. a function 
-- /from Point and Inclination to Graphic/). 
-- 
-- The 'emptyLocThetaGraphic' is treated as a /null primitive/ by 
-- @Wumpus-Core@ and is not drawn, although it does generate a 
-- minimum bounding box at the implicit start point.
-- 
emptyLocThetaGraphic :: CtxSize u => LocThetaGraphic u
emptyLocThetaGraphic = lift1R2 emptyLocGraphicRU



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
    strokeAttr >>= \(rgb,attr) -> return $ makeAns $ ostroke rgb attr pp


-- | 'closedStroke' : @ path -> Graphic @
--
-- This is the analogue to 'cstroke' in @Wumpus-core@, but the 
-- drawing properties (colour, line width, etc.) are taken from 
-- the implicit 'DrawingContext'.
--
closedStroke :: PrimPath -> Graphic u
closedStroke pp = 
    strokeAttr >>= \(rgb,attr) -> return $ makeAns $ cstroke rgb attr pp


-- | 'filledPath' : @ path -> Graphic @
-- 
-- This is the analogue to 'fill' in @Wumpus-core@, but the 
-- fill colour is taken from the implicit 'DrawingContext'.
--
--
filledPath :: PrimPath -> Graphic u
filledPath pp = fillAttr >>= \rgb -> return $ makeAns $ fill rgb pp
                 

-- | 'borderedPath' : @ path -> Graphic @
--
-- This is the analogue to 'fillStroke' in @Wumpus-core@, but the 
-- drawing properties (fill colour, border colour, line width, 
-- etc.) are taken from the implicit 'DrawingContext'.
--
--
borderedPath :: PrimPath -> Graphic u
borderedPath pp =
    borderedAttr >>= \(frgb,attr,srgb) -> 
      return $ makeAns $ fillStroke frgb attr srgb pp



--------------------------------------------------------------------------------
-- Text

ptText :: PsDouble u 
       => (DPoint2 -> Primitive) -> LocGraphic u
ptText fn = promoteR1 $ \pt -> return $ makeAns (fn $ dPoint pt)

rptText :: PsDouble u 
        => (Radian -> DPoint2 -> Primitive) -> LocThetaGraphic u
rptText fn = promoteR2 $ \pt theta -> 
    return $ makeAns (fn theta (dPoint pt))


ctxText :: CtxSize u 
        => (DPoint2 -> Primitive) -> LocGraphic u
ctxText fn = ctxStartPoint $ \pt -> return $ makeAns (fn pt)

rctxText :: CtxSize u 
         => (Radian -> DPoint2 -> Primitive) -> LocThetaGraphic u
rctxText fn = ctxStartPointTheta $ \pt theta -> 
    return $ makeAns (fn theta pt)


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
textlineAU ss = textAttr >>= \(rgb,attr) -> ptText (textlabel rgb attr ss)



-- | Relative unit version of 'textlineAU'.
--
textlineRU :: CtxSize u => String -> LocGraphic u
textlineRU ss = textAttr >>= \(rgb,attr) -> ctxText (textlabel rgb attr ss)




-- | 'rtextlineAU' : @ string -> LocThetaGraphic @
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
rtextlineAU :: PsDouble u => String -> LocThetaGraphic u
rtextlineAU ss = textAttr >>= \(rgb,attr) -> rptText (rtextlabel rgb attr ss)

-- | Relative unit version of 'rtextlineAU'.
--
rtextlineRU :: CtxSize u => String -> LocThetaGraphic u
rtextlineRU ss = textAttr >>= \(rgb,attr) -> rctxText (rtextlabel rgb attr ss)



-- | 'escapedlineAU' : @ escaped_text -> LocGraphic @
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
escapedlineAU :: PsDouble u => EscapedText -> LocGraphic u
escapedlineAU esc =           
    textAttr >>= \(rgb,attr) -> ptText (escapedlabel rgb attr esc)

-- | Relative unit version of 'escapedlineAU'.
--
escapedlineRU :: CtxSize u => EscapedText -> LocGraphic u
escapedlineRU esc = 
    textAttr >>= \(rgb,attr) -> ctxText (escapedlabel rgb attr esc)



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
rescapedlineAU :: PsDouble u => EscapedText -> LocThetaGraphic u
rescapedlineAU esc = 
    textAttr >>= \(rgb,attr) -> rptText (rescapedlabel rgb attr esc)

-- | | Relative unit version of 'rescapedlineAU'.
--
rescapedlineRU :: CtxSize u => EscapedText -> LocThetaGraphic u
rescapedlineRU esc = 
    textAttr >>= \(rgb,attr) -> rctxText (rescapedlabel rgb attr esc)


-- | Unit parametric version of KerningChar from Wumpus-Core.
--
type KernChar u = (u,EscapedChar)


type KernFun = RGBi -> FontAttr  -> [KerningChar] -> DPoint2  -> Primitive

kernAU :: PsDouble u => KernFun -> [KernChar u] -> LocGraphic u
kernAU fn xs =  
    textAttr >>= \(rgb,attr) -> ptText (fn rgb attr (map kconv xs))
  where
    kconv (u,ch) = (toPsDouble u,ch)


kernRU :: CtxSize u => KernFun -> [KernChar u] -> LocGraphic u
kernRU fn xs =  
    textAttr   >>= \(rgb,attr) -> 
    mapM mf xs >>= \xs1 ->
    ctxText (fn rgb attr xs1)
  where
    mf (u,ch) = dsize u >>= \u1 -> return (u1,ch)


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
hkernlineAU :: PsDouble u => [KernChar u] -> LocGraphic u
hkernlineAU = kernAU hkernlabel

hkernlineRU :: CtxSize u => [KernChar u] -> LocGraphic u
hkernlineRU = kernRU hkernlabel


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
vkernlineAU :: PsDouble u => [KernChar u] -> LocGraphic u
vkernlineAU = kernAU vkernlabel

vkernlineRU :: CtxSize u => [KernChar u] -> LocGraphic u
vkernlineRU = kernRU vkernlabel



--------------------------------------------------------------------------------
-- Lines

-- | 'straightLineAU' : @ start_point * end_point -> LocGraphic @ 
-- 
-- Create a straight line 'Graphic', the start and end point 
-- are supplied explicitly.
-- 
-- The line properties (colour, pen thickness, etc.) are taken 
-- from the implicit 'DrawingContext'.
-- 
straightLineAU :: PsDouble u => Point2 u -> Point2 u -> Graphic u
straightLineAU p1 p2 = openStroke $ vertexPathAU [p1,p2]

-- | Relative unit version of 'straightLineAU'.
--
straightLineRU :: CtxSize u => Point2 u -> Point2 u -> Graphic u
straightLineRU p1 p2 = vertexPathRU [p1,p2] >>= openStroke


-- | 'locStraightLineAU' : @ vec_to -> LocGraphic @ 
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
locStraightLineAU :: PsDouble u => Vec2 u -> LocGraphic u
locStraightLineAU v = locPathAU [v] >>= lift0R1 . openStroke

-- | Relative unit version of 'locStraightLineAU'.
--
locStraightLineRU :: CtxSize u => Vec2 u -> LocGraphic u
locStraightLineRU v = locPathRU [v] >>= lift0R1 . openStroke
          




-- | 'curveLineAU' : @ start_point * control_point1 * 
--        control_point2 * end_point -> Graphic @ 
-- 
-- Create a Bezier curve 'Graphic', all control points are 
-- supplied explicitly.
-- 
-- The line properties (colour, pen thickness, etc.) are taken 
-- from the implicit 'DrawingContext'.
-- 
curvedLineAU :: PsDouble u
             => Point2 u -> Point2 u -> Point2 u -> Point2 u -> Graphic u
curvedLineAU p0 p1 p2 p3 = openStroke $ curvedPathAU [p0,p1,p2,p3]


-- | Relative unit version of 'curvedLineAU'.
--
curvedLineRU :: CtxSize u
             => Point2 u -> Point2 u -> Point2 u -> Point2 u -> Graphic u
curvedLineRU p0 p1 p2 p3 = curvedPathRU [p0,p1,p2,p3] >>= openStroke


--------------------------------------------------------------------------------
-- Circles

-- | Helper for circle drawing.
--
ptCircle :: PsDouble u 
          => (PrimPath -> Graphic u)  -> u -> LocGraphic u 
ptCircle fn r = promoteR1 $ \pt ->
    fn $ curvedPrimPath $ bezierCircle (toPsDouble r) (dPoint pt)

-- | Helper for ellipse drawing.
--
ctxCircle :: CtxSize u 
           => (PrimPath -> Graphic u) -> u -> LocGraphic u 
ctxCircle fn r = ctxStartPoint $ \pt ->
    dsize r  >>= \r1 ->
    fn $ curvedPrimPath $ bezierCircle r1 pt


-- | 'strokedCircleAU' : @ radius -> LocGraphic @
--
-- Create a stroked circle 'LocGraphic' - the implicit point is 
-- center. The circle is drawn with four Bezier curves. 
-- 
-- The line properties (colour, pen thickness, etc.) are taken 
-- from the implicit 'DrawingContext'.
-- 
strokedCircleAU :: PsDouble u => u -> LocGraphic u
strokedCircleAU = ptCircle closedStroke


-- | Relative unit version of 'strokedCircleAU'.
--
strokedCircleRU :: CtxSize u => u -> LocGraphic u
strokedCircleRU = ctxCircle closedStroke



-- | 'filledCircleAU' : @ radius -> LocGraphic @
--
-- Create a filled circle 'LocGraphic' - the implicit point is 
-- center. The circle is drawn with four Bezier curves. 
-- 
-- The fill colour is taken from the implicit 'DrawingContext'.
-- 
filledCircleAU :: PsDouble u => u -> LocGraphic u
filledCircleAU = ptCircle filledPath 

-- | Relative unit version of 'filledCircleAU'.
--
filledCircleRU :: CtxSize u => u -> LocGraphic u
filledCircleRU = ctxCircle filledPath 



-- | 'borderedCircleAU' : @ radius -> LocGraphic @
--
-- Create a bordered circle 'LocGraphic' - the implicit point is 
-- center. The circle is drawn with four Bezier curves. 
-- 
-- The background fill colour and the outline stroke properties 
-- are taken from the implicit 'DrawingContext'.
-- 
borderedCircleAU :: PsDouble u => u -> LocGraphic u
borderedCircleAU = ptCircle borderedPath

-- | Relative unit version of 'borderedCircleAU'.
--
borderedCircleRU :: CtxSize u => u -> LocGraphic u
borderedCircleRU = ctxCircle borderedPath

--------------------------------------------------------------------------------
-- Ellipses

-- | Helper for ellipse drawing.
--
ptEllipse :: PsDouble u 
          => (PrimPath -> Graphic u) -> u -> u -> LocGraphic u 
ptEllipse fn rx ry = promoteR1 $ \pt ->
    fn $ curvedPrimPath $ bezierEllipse (toPsDouble rx) (toPsDouble ry) (dPoint pt)


-- | Helper for ellipse drawing.
--
rptEllipse :: PsDouble u 
           => (PrimPath -> Graphic u) -> u -> u -> LocThetaGraphic u 
rptEllipse fn rx ry = promoteR2 $ \pt ang ->
    fn $ curvedPrimPath $ rbezierEllipse (toPsDouble rx) (toPsDouble ry) ang (dPoint pt)


-- | Helper for ellipse drawing.
--
ctxEllipse :: CtxSize u 
           => (PrimPath -> Graphic u) -> u -> u -> LocGraphic u 
ctxEllipse fn rx ry = ctxStartPoint $ \pt ->
    dsize rx  >>= \rx1 ->
    dsize ry  >>= \ry1 ->
    fn $ curvedPrimPath $ bezierEllipse rx1 ry1 pt


-- | Helper for ellipse drawing.
--
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
ptRectangle :: PsDouble u 
            => (PrimPath -> Graphic u) -> u -> u -> LocGraphic u
ptRectangle fn w h = 
     locPathAU [hvec w, vvec h, hvec (-w)] >>= \pp -> lift0R1 (fn pp)



-- | Supplied point is /bottom-left/.
--
ctxRectangle :: CtxSize u 
             => (PrimPath -> Graphic u) -> u -> u -> LocGraphic u
ctxRectangle fn w h = 
     locPathRU [hvec w, vvec h, hvec (-w)] >>= \pp -> lift0R1 (fn pp)

    


-- | 'strokedRectangleAU' : @ width * height -> LocGraphic @
--
-- Create a stroked rectangle 'LocGraphic' - the implicit point is 
-- bottom-left. 
-- 
-- The line properties (colour, pen thickness, etc.) are taken 
-- from the implicit 'DrawingContext'.
-- 
strokedRectangleAU :: PsDouble u => u -> u -> LocGraphic u
strokedRectangleAU = ptRectangle closedStroke

-- | Relative unit version of 'strokedRectangleAU'.
--
strokedRectangleRU :: CtxSize u => u -> u -> LocGraphic u
strokedRectangleRU = ctxRectangle closedStroke


-- | 'filledRectangleAU' : @ width * height -> LocGraphic @
--
-- Create a filled rectangle 'LocGraphic' - the implicit point is 
-- the bottom-left. 
-- 
-- The fill colour is taken from the implicit 'DrawingContext'.
-- 
filledRectangleAU :: PsDouble u => u -> u -> LocGraphic u
filledRectangleAU = ptRectangle filledPath

-- | Relative unit version of 'filledRectangleAU'.
--
filledRectangleRU :: CtxSize u => u -> u -> LocGraphic u
filledRectangleRU = ctxRectangle filledPath


-- | 'borderedRectangle' : @ width * height -> LocGraphic @
--
-- Create a bordered rectangle 'LocGraphic' - the implicit point is 
-- bottom-left. 
-- 
-- The background fill colour and the outline stroke properties 
-- are taken from the implicit 'DrawingContext'.
-- 
borderedRectangleAU :: PsDouble u => u -> u -> LocGraphic u
borderedRectangleAU = ptRectangle borderedPath 


-- | Relative unit version of 'borderedRectangleAU'.
--
borderedRectangleRU :: CtxSize u => u -> u -> LocGraphic u
borderedRectangleRU = ctxRectangle borderedPath 

---------------------------------------------------------------------------


-- | Helper for ellipse drawing.
--
ptDrawEllipse :: PsDouble u 
              => (Double -> Double -> DPoint2 -> Primitive) 
              -> u -> u -> LocGraphic u
ptDrawEllipse fn rx ry = promoteR1 $ \pt -> 
    return $ makeAns $ fn (toPsDouble rx) (toPsDouble ry) (dPoint pt)



-- | Helper for ellipse drawing.
--
ctxDrawEllipse :: CtxSize u 
               => (Double -> Double -> DPoint2 -> Primitive) 
               -> u -> u -> LocGraphic u
ctxDrawEllipse fn rx ry = ctxStartPoint $ \pt -> 
    dsize  rx >>= \rx1 ->
    dsize  ry >>= \ry1 ->
    return $ makeAns $ fn rx1 ry1 pt    


-- | 'strokedDiskAU' : @ radius -> LocGraphic @
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
strokedDiskAU :: PsDouble u => u -> LocGraphic u
strokedDiskAU r = strokedEllipseDiskAU r r


-- | Relative unit version of 'strokedDiskAU'.
--
strokedDiskRU :: CtxSize u => u -> LocGraphic u
strokedDiskRU r = strokedEllipseDiskRU r r


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

-- | Relative unit version of 'filledDiskAU'.
--
filledDiskRU :: CtxSize u => u -> LocGraphic u
filledDiskRU r = filledEllipseDiskRU r r


-- | 'borderedDiskAU' : @ radius -> LocGraphic @
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
borderedDiskAU :: PsDouble u => u -> LocGraphic u
borderedDiskAU r = borderedEllipseDiskAU r r

-- | Relative unit version of 'borderedDiskAU'.
--
borderedDiskRU :: CtxSize u => u -> LocGraphic u
borderedDiskRU r = borderedEllipseDiskRU r r


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
strokedEllipseDiskAU :: PsDouble u => u -> u -> LocGraphic u
strokedEllipseDiskAU rx ry =
    strokeAttr >>= \(rgb,attr) -> ptDrawEllipse (strokeEllipse rgb attr) rx ry


-- | Relative unit version of 'strokedEllipseDiskAU'.
--
strokedEllipseDiskRU :: CtxSize u => u -> u -> LocGraphic u
strokedEllipseDiskRU rx ry =
    strokeAttr >>= \(rgb,attr) -> ctxDrawEllipse (strokeEllipse rgb attr) rx ry



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
filledEllipseDiskAU rx ry =
   fillAttr >>= \rgb -> ptDrawEllipse (fillEllipse rgb) rx ry

-- | Relative unit version of 'filledEllispeDiskAU'.
--
filledEllipseDiskRU :: CtxSize u => u -> u -> LocGraphic u
filledEllipseDiskRU rx ry = 
    fillAttr >>= \rgb -> ctxDrawEllipse (fillEllipse rgb) rx ry



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
borderedEllipseDiskAU :: PsDouble u => u -> u -> LocGraphic u
borderedEllipseDiskAU rx ry = 
    borderedAttr >>= \(frgb,attr,srgb) -> 
      ptDrawEllipse (fillStrokeEllipse frgb attr srgb) rx ry


-- | Relative unit version of 'borderedEllispeDiskAU'.
--
borderedEllipseDiskRU :: CtxSize u => u -> u -> LocGraphic u
borderedEllipseDiskRU rx ry = 
    borderedAttr >>= \(frgb,attr,srgb) -> 
      ctxDrawEllipse (fillStrokeEllipse frgb attr srgb) rx ry
