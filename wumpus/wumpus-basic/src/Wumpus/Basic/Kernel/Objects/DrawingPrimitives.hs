{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Kernel.Objects.DrawingPrimitives
-- Copyright   :  (c) Stephen Tetley 2011
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


  -- * Prim Paths
    locPP

  , emptyLocPP
  , vertexPP
  , curvePP

  , openStroke
  , closedStroke
  , filledPath
  , borderedPath

  -- * Text
  , textline
  , rtextline
  , escapedline
  , rescapedline

  , KernChar
  , hkernline
  , vkernline

  -- * Lines
  , straightLine
  , locStraightLine
  , curvedLine
  , straightConnector

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
import Wumpus.Basic.Kernel.Base.WrappedPrimitive
import Wumpus.Basic.Kernel.Objects.Basis
import Wumpus.Basic.Kernel.Objects.Connector
import Wumpus.Basic.Kernel.Objects.Image
import Wumpus.Basic.Kernel.Objects.LocImage
import Wumpus.Basic.Kernel.Objects.LocThetaImage

import Wumpus.Core                              -- package: wumpus-core

import Control.Applicative


-- Helpers


norm2 :: InterpretUnit u => u -> u -> Query (Double,Double)
norm2 a b = (,) <$> normalizeDC a <*> normalizeDC b

makeGraphic :: Query a -> (a -> Primitive) -> Graphic u
makeGraphic qy fn = qy >>= \a -> return $ Ans UNil (prim1 $ fn a)


makeLocGraphic :: InterpretUnit u 
               => Query a -> (a -> DPoint2 -> Primitive) -> LocGraphic u
makeLocGraphic qy fn = promoteR1 $ \pt -> 
    uconvertFDC pt >>= \dpt ->
    qy >>= \a -> return $ Ans UNil (prim1 $ fn a dpt)

makeLocThetaGraphic :: InterpretUnit u 
                    => Query a -> (a -> DPoint2 -> Radian -> Primitive) 
                    -> LocThetaGraphic u
makeLocThetaGraphic qy fn = promoteR2 $ \pt ang -> 
    uconvertFDC pt >>= \dpt ->
    qy >>= \a -> return $ Ans UNil (prim1 $ fn a dpt ang)



--------------------------------------------------------------------------------
-- Paths

-- Note - naming convention, the PP suffix is to avoid confusion 
-- with the Path data type in Wumpus-Drawing. These paths are
-- considered more /internal/.
--

-- | 'locPP' : @ [next_vector] -> LocQuery PrimPath @
--
-- Create a path 'LocQuery' - i.e. a functional type 
-- /from Point to PrimPath/.
-- 
-- This is the analogue to 'vectorPath' in @Wumpus-Core@, but the 
-- result is produced /within/ the 'DrawingContext'.
--
locPP :: InterpretUnit u => [Vec2 u] -> LocQuery u PrimPath
locPP vs = promoteR1 $ \ pt  ->
    vectorPrimPath <$> uconvertFDC pt <*> mapM uconvertFDC vs




-- | 'emptyLocPP' : @ (Point ~> PrimPath) @
--
-- Create an empty path 'LocQuery' - i.e. a functional type 
-- /from Point to PrimPath/.
--
-- This is the analogue to 'emptyPath' in @Wumpus-Core@, but the
-- result is produced /within/ the 'DrawingContext'.
--
emptyLocPP :: InterpretUnit u => LocQuery u PrimPath
emptyLocPP = locPP []




-- | 'vertexPP' : @ (Point ~> PrimPath) @
--
-- Create a PrimPath made of straight line segments joining the 
-- supplied points.
--
-- This is the analogue to 'vertexPrimPath' in @Wumpus-Core@, but 
-- it is polymorphic on unit.
--
vertexPP :: InterpretUnit u => [Point2 u] -> Query PrimPath
vertexPP xs = vertexPrimPath <$> mapM uconvertFDC xs



-- | 'curvePP' : @ (Point ~> PrimPath) @
--
-- Create a path made of straight line segments joining the 
-- supplied points.
--
-- This is the analogue to 'curvedPrimPath' in @Wumpus-Core@, but 
-- it is polymorphic on unit.
--
curvePP :: InterpretUnit u => [Point2 u] -> Query PrimPath
curvePP xs = curvedPrimPath <$> mapM uconvertFDC xs


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
openStroke pp = makeGraphic strokeAttr (\(rgb,attr) -> ostroke rgb attr pp)



-- | 'closedStroke' : @ path -> Graphic @
--
-- This is the analogue to 'cstroke' in @Wumpus-core@, but the 
-- drawing properties (colour, line width, etc.) are taken from 
-- the implicit 'DrawingContext'.
--
closedStroke :: PrimPath -> Graphic u
closedStroke pp = 
    makeGraphic strokeAttr (\(rgb,attr) -> cstroke rgb attr pp)


-- | 'filledPath' : @ path -> Graphic @
-- 
-- This is the analogue to 'fill' in @Wumpus-core@, but the 
-- fill colour is taken from the implicit 'DrawingContext'.
--
--
filledPath :: PrimPath -> Graphic u
filledPath pp = makeGraphic fillAttr (\rgb -> fill rgb pp)
                 

-- | 'borderedPath' : @ path -> Graphic @
--
-- This is the analogue to 'fillStroke' in @Wumpus-core@, but the 
-- drawing properties (fill colour, border colour, line width, 
-- etc.) are taken from the implicit 'DrawingContext'.
--
--
borderedPath :: PrimPath -> Graphic u
borderedPath pp =
    makeGraphic borderedAttr 
                (\(frgb,attr,srgb) -> fillStroke frgb attr srgb pp)



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
textline :: InterpretUnit u => String -> LocGraphic u
textline ss = 
    makeLocGraphic textAttr 
                   (\(rgb,attr) pt -> textlabel rgb attr ss pt)





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
rtextline :: InterpretUnit u => String -> LocThetaGraphic u
rtextline ss =
    makeLocThetaGraphic textAttr
                        (\(rgb,attr) pt ang -> rtextlabel rgb attr ss ang pt)


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
escapedline :: InterpretUnit u => EscapedText -> LocGraphic u
escapedline esc =           
    makeLocGraphic textAttr 
                   (\(rgb,attr) pt -> escapedlabel rgb attr esc pt)



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
rescapedline :: InterpretUnit u => EscapedText -> LocThetaGraphic u
rescapedline esc = 
    makeLocThetaGraphic textAttr
                        (\(rgb,attr) pt ang -> rescapedlabel rgb attr esc ang pt)



-- | Unit parametric version of KerningChar from Wumpus-Core.
--
type KernChar u = (u,EscapedChar)

uconvKernChar :: InterpretUnit u => [KernChar u] -> Query [KerningChar]
uconvKernChar = mapM mf
  where
    mf (u,ch) = (\u1 -> (u1,ch)) <$> normalizeDC u



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
hkernline :: InterpretUnit u => [KernChar u] -> LocGraphic u
hkernline ks = lift0R1 (uconvKernChar ks) >>= body   
  where
    body ans = makeLocGraphic textAttr
                  (\(rgb,attr) pt -> hkernlabel rgb attr ans pt)



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
vkernline :: InterpretUnit u => [KernChar u] -> LocGraphic u
vkernline ks = lift0R1 (uconvKernChar ks) >>= body
  where
    body ans = makeLocGraphic textAttr
                  (\(rgb,attr) pt -> vkernlabel rgb attr ans pt)

--------------------------------------------------------------------------------
-- Lines

-- | 'straightLine' : @ start_point * end_point -> LocGraphic @ 
-- 
-- Create a straight line 'Graphic', the start and end point 
-- are supplied explicitly.
-- 
-- The line properties (colour, pen thickness, etc.) are taken 
-- from the implicit 'DrawingContext'.
-- 
straightLine :: InterpretUnit u => Point2 u -> Point2 u -> Graphic u
straightLine p1 p2 = vertexPP [p1,p2] >>= openStroke


-- | 'locStraightLine' : @ vec_to -> LocGraphic @ 
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
locStraightLine :: InterpretUnit u => Vec2 u -> LocGraphic u
locStraightLine v = promoteR1 $ \pt -> 
    apply1R1 (locPP [v]) pt >>= openStroke



-- | 'curveLine' : @ start_point * control_point1 * 
--        control_point2 * end_point -> Graphic @ 
-- 
-- Create a Bezier curve 'Graphic', all control points are 
-- supplied explicitly.
-- 
-- The line properties (colour, pen thickness, etc.) are taken 
-- from the implicit 'DrawingContext'.
-- 
curvedLine :: InterpretUnit u
           => Point2 u -> Point2 u -> Point2 u -> Point2 u -> Graphic u
curvedLine p0 p1 p2 p3 = curvePP [p0,p1,p2,p3] >>= openStroke




-- | 'straightConnector' : @ start_point * end_point -> Connector @ 
-- 
-- Create a straight line 'Graphic', the start and end point 
-- are supplied implicitly.
-- 
-- The line properties (colour, pen thickness, etc.) are taken 
-- from the implicit 'DrawingContext'.
-- 
straightConnector :: InterpretUnit u => ConnectorGraphic u
straightConnector = promoteR2 $ \p0 p1 -> vertexPP [p0,p1] >>= openStroke



--------------------------------------------------------------------------------
-- Circles

-- | Helper for circle drawing.
--
circlePath :: InterpretUnit u 
         => u -> LocQuery u PrimPath
circlePath r = promoteR1 $ \pt  ->
    (\dr dpt -> curvedPrimPath $ bezierCircle dr dpt) 
      <$> normalizeDC r <*> normalizeFDC pt




-- | 'strokedCircle' : @ radius -> LocGraphic @
--
-- Create a stroked circle 'LocGraphic' - the implicit point is 
-- center. The circle is drawn with four Bezier curves. 
-- 
-- The line properties (colour, pen thickness, etc.) are taken 
-- from the implicit 'DrawingContext'.
-- 
strokedCircle :: InterpretUnit u => u -> LocGraphic u
strokedCircle r = promoteR1 $ \pt -> 
    apply1R1 (circlePath r) pt >>= openStroke




-- | 'filledCircle' : @ radius -> LocGraphic @
--
-- Create a filled circle 'LocGraphic' - the implicit point is 
-- center. The circle is drawn with four Bezier curves. 
-- 
-- The fill colour is taken from the implicit 'DrawingContext'.
-- 
filledCircle :: InterpretUnit u => u -> LocGraphic u
filledCircle r = promoteR1 $ \pt -> 
    apply1R1 (circlePath r) pt >>= filledPath 



-- | 'borderedCircle' : @ radius -> LocGraphic @
--
-- Create a bordered circle 'LocGraphic' - the implicit point is 
-- center. The circle is drawn with four Bezier curves. 
-- 
-- The background fill colour and the outline stroke properties 
-- are taken from the implicit 'DrawingContext'.
-- 
borderedCircle :: InterpretUnit u => u -> LocGraphic u
borderedCircle r = promoteR1 $ \pt -> 
    apply1R1 (circlePath r) pt >>= borderedPath


--------------------------------------------------------------------------------
-- Ellipses


-- | Helper for ellipse drawing.
--
ellipsePath :: InterpretUnit u 
            => u -> u -> LocQuery u PrimPath
ellipsePath rx ry = promoteR1 $ \pt ->
    (\drx dry dpt -> curvedPrimPath $ bezierEllipse drx dry dpt) 
      <$> normalizeDC rx <*> normalizeDC ry <*> normalizeFDC pt


-- | Helper for ellipse drawing.
--
rellipsePath :: InterpretUnit u 
            => u -> u -> LocThetaQuery u PrimPath
rellipsePath rx ry = promoteR2 $ \pt ang ->
    (\drx dry dpt -> curvedPrimPath $ rbezierEllipse drx dry ang dpt) 
      <$> normalizeDC rx <*> normalizeDC ry <*> normalizeFDC pt




-- | 'strokedEllipse' : @ x_radius * y_radius -> LocGraphic @
--
-- Create a stroked ellipse 'LocGraphic' - the implicit point is 
-- center. The ellipse is drawn with four Bezier curves. 
-- 
-- The line properties (colour, pen thickness, etc.) are taken 
-- from the implicit 'DrawingContext'.
-- 
strokedEllipse :: InterpretUnit u => u -> u -> LocGraphic u
strokedEllipse rx ry = promoteR1 $ \pt ->
   apply1R1 (ellipsePath rx ry) pt >>= closedStroke



-- | 'rstrokedEllipse' : @ x_radius * y_radius -> LocThetaGraphic @
--
-- Create a stroked ellipse 'LocThetaGraphic' - the implicit point
-- is center and the angle is rotation about the center. The 
-- ellipse is drawn with four Bezier curves. 
-- 
-- The line properties (colour, pen thickness, etc.) are taken 
-- from the implicit 'DrawingContext'.
-- 
rstrokedEllipse :: InterpretUnit u
                => u -> u -> LocThetaGraphic u
rstrokedEllipse rx ry = promoteR2 $ \pt ang -> 
    apply2R2 (rellipsePath rx ry) pt ang >>= closedStroke




-- | 'filledEllipse' : @ x_radius * y_radius -> LocGraphic @
--
-- Create a filled ellipse 'LocGraphic' - the implicit point is 
-- center. The ellipse is drawn with four Bezier curves. 
-- 
-- The fill colour is taken from the implicit 'DrawingContext'.
-- 
filledEllipse :: InterpretUnit u => u -> u -> LocGraphic u
filledEllipse rx ry = promoteR1 $ \pt -> 
    apply1R1 (ellipsePath rx ry) pt >>= filledPath


-- | 'rfilledEllipse' : @ x_radius * y_radius -> LocGraphic @
--
-- Create a filled ellipse 'LocThetaGraphic' - the implicit point
-- is center and the angle is rotation about the center. The 
-- ellipse is drawn with four Bezier curves.  
-- 
-- The fill colour is taken from the implicit 'DrawingContext'.
-- 
rfilledEllipse :: InterpretUnit u => u -> u -> LocThetaGraphic u
rfilledEllipse rx ry = promoteR2 $ \pt ang ->
    apply2R2 (rellipsePath rx ry) pt ang >>= filledPath



-- | 'borderedEllipse' : @ x_radius * y_radius -> LocGraphic @
--
-- Create a bordered ellipse 'LocGraphic' - the implicit point is 
-- center. The ellipse is drawn with four Bezier curves. 
-- 
-- The background fill colour and the outline stroke properties 
-- are taken from the implicit 'DrawingContext'.
-- 
borderedEllipse :: InterpretUnit u => u -> u -> LocGraphic u
borderedEllipse rx ry = promoteR1 $ \pt -> 
    apply1R1 (ellipsePath rx ry) pt >>= borderedPath



-- | 'rborderedEllipse' : @ x_radius * y_radius -> LocGraphic @
--
-- Create a bordered ellipse 'LocThetaGraphic' - the implicit point
-- is center and the angle is rotation about the center. The 
-- ellipse is drawn with four Bezier curves.  
-- 
-- The background fill colour and the outline stroke properties 
-- are taken from the implicit 'DrawingContext'.
-- 
rborderedEllipse :: InterpretUnit u
                 => u -> u -> LocThetaGraphic u
rborderedEllipse rx ry = promoteR2 $ \pt ang -> 
    apply2R2 (rellipsePath rx ry) pt ang >>= borderedPath



-- Note - clipping to do...

--------------------------------------------------------------------------------
-- Rectangles

-- | Supplied point is /bottom-left/.
--
rectanglePath :: InterpretUnit u 
              => u -> u -> LocQuery u PrimPath
rectanglePath w h = locPP [hvec w, vvec h, hvec (-w)]


-- | 'strokedRectangle' : @ width * height -> LocGraphic @
--
-- Create a stroked rectangle 'LocGraphic' - the implicit point is 
-- bottom-left. 
-- 
-- The line properties (colour, pen thickness, etc.) are taken 
-- from the implicit 'DrawingContext'.
-- 
strokedRectangle :: InterpretUnit u => u -> u -> LocGraphic u
strokedRectangle w h = promoteR1 $ \pt -> 
    apply1R1 (rectanglePath w h) pt >>= closedStroke


-- | 'filledRectangle' : @ width * height -> LocGraphic @
--
-- Create a filled rectangle 'LocGraphic' - the implicit point is 
-- the bottom-left. 
-- 
-- The fill colour is taken from the implicit 'DrawingContext'.
-- 
filledRectangle :: InterpretUnit u => u -> u -> LocGraphic u
filledRectangle w h = promoteR1 $ \pt -> 
    apply1R1 (rectanglePath w h) pt >>= filledPath


-- | 'borderedRectangle' : @ width * height -> LocGraphic @
--
-- Create a bordered rectangle 'LocGraphic' - the implicit point is 
-- bottom-left. 
-- 
-- The background fill colour and the outline stroke properties 
-- are taken from the implicit 'DrawingContext'.
-- 
borderedRectangle :: InterpretUnit u => u -> u -> LocGraphic u
borderedRectangle w h = promoteR1 $ \pt -> 
    apply1R1 (rectanglePath w h) pt >>= borderedPath 

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
strokedDisk :: InterpretUnit u => u -> LocGraphic u
strokedDisk radius = 
    normalizeDC radius >>= body
  where
    body r = makeLocGraphic strokeAttr
                (\(rgb,attr) pt -> strokeEllipse rgb attr r r pt)




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
filledDisk :: InterpretUnit u => u -> LocGraphic u
filledDisk radius = 
    normalizeDC radius >>= body
  where
    body r = makeLocGraphic fillAttr (\rgb pt -> fillEllipse rgb r r pt)


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
borderedDisk :: InterpretUnit u => u -> LocGraphic u
borderedDisk radius = 
    normalizeDC radius >>= body
  where
    body r = makeLocGraphic borderedAttr
                (\(frgb,attr,srgb) pt -> fillStrokeEllipse frgb attr srgb r r pt)


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
strokedEllipseDisk :: InterpretUnit u => u -> u -> LocGraphic u
strokedEllipseDisk rx ry = lift0R1 (norm2 rx ry) >>= body
  where
    body (drx,dry) = makeLocGraphic strokeAttr
                        (\(rgb,attr) pt -> strokeEllipse rgb attr drx dry pt)



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
filledEllipseDisk :: InterpretUnit u => u -> u -> LocGraphic u
filledEllipseDisk rx ry = lift0R1 (norm2 rx ry) >>= body
  where
    body (drx,dry) = makeLocGraphic fillAttr
                        (\rgb pt -> fillEllipse rgb drx dry pt)


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
borderedEllipseDisk :: InterpretUnit u => u -> u -> LocGraphic u
borderedEllipseDisk rx ry = lift0R1 (norm2 rx ry) >>= body
  where
    body (drx,dry) = makeLocGraphic borderedAttr
                          (\(frgb,attr,srgb) pt -> 
                                fillStrokeEllipse frgb attr srgb drx dry pt)

