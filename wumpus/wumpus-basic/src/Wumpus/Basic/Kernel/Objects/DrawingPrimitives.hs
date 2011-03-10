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

  -- * Empty graphics
    emptyLocGraphic
  , emptyLocThetaGraphic

  -- * Paths
  , locPath
  , emptyLocPath
  , vertexPath
  , curvedPath

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
import Wumpus.Basic.Kernel.Base.QueryDC
import Wumpus.Basic.Kernel.Objects.Query
import Wumpus.Basic.Kernel.Objects.Image
import Wumpus.Basic.Kernel.Objects.LocImage
import Wumpus.Basic.Kernel.Objects.LocThetaImage

import Wumpus.Core                              -- package: wumpus-core



-- Helper

scalarSize :: InterpretUnit u => u -> Query Double
scalarSize u = makeQuery point_size (\sz -> uconvertScalar sz u)


scalarPair :: InterpretUnit u => u -> u -> Query (Double,Double)
scalarPair rx ry = 
    makeQuery point_size (\sz -> (uconvertScalar sz rx, uconvertScalar sz ry))


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
emptyLocGraphic :: InterpretUnit u => LocGraphic u
emptyLocGraphic = withLocQuery emptyLocPath openStroke


-- | 'emptyLocThetaGraphic' : @ LocThetaGraphic @
--
-- Build an empty 'LocThetaGraphic' (i.e. a function 
-- /from Point and Inclination to Graphic/). 
-- 
-- The 'emptyLocThetaGraphic' is treated as a /null primitive/ by 
-- @Wumpus-Core@ and is not drawn, although it does generate a 
-- minimum bounding box at the implicit start point.
-- 
emptyLocThetaGraphic :: InterpretUnit u => LocThetaGraphic u
emptyLocThetaGraphic = ignoreTheta emptyLocGraphic


--------------------------------------------------------------------------------
-- Paths


-- | 'locPath' : @ [next_vector] -> LocQuery PrimPath @
--
-- Create a path 'LocQuery' - i.e. a functional type 
-- /from Point to PrimPath/.
-- 
-- This is the analogue to 'vectorPath' in @Wumpus-Core@, but the 
-- result is produced /within/ the 'DrawingContext'.
--
locPath :: InterpretUnit u => [Vec2 u] -> LocQuery u PrimPath
locPath vs = makeLocQuery point_size $ \sz pt  ->
    vectorPrimPath (uconvertExt sz pt) (map (uconvertExt sz) vs)




-- | 'emptyLocPath' : @ (Point ~> PrimPath) @
--
-- Create an empty path 'LocCF' - i.e. a functional type 
-- /from Point to PrimPath/.
--
-- This is the analogue to 'emptyPath' in @Wumpus-Core@, but the
-- result is produced /within/ the 'DrawingContext'.
--
emptyLocPath :: InterpretUnit u => LocQuery u PrimPath
emptyLocPath = locPath []




-- | 'vertexPath' : @ (Point ~> PrimPath) @
--
-- Create a path made of straight line segments joining the 
-- supplied points.
--
-- This is the analogue to 'vertexPrimPath' in @Wumpus-Core@, but 
-- it is polymorphic on unit.
--
vertexPath :: InterpretUnit u => [Point2 u] -> Query PrimPath
vertexPath xs = makeQuery point_size $ \sz -> 
    vertexPrimPath $ map (uconvertExt sz) xs



-- | 'curvedPath' : @ (Point ~> PrimPath) @
--
-- Create a path made of straight line segments joining the 
-- supplied points.
--
-- This is the analogue to 'curvedPrimPath' in @Wumpus-Core@, but 
-- it is polymorphic on unit.
--
curvedPath :: InterpretUnit u => [Point2 u] -> Query PrimPath
curvedPath xs = makeQuery point_size $ \sz -> 
    curvedPrimPath $ map (uconvertExt sz) xs


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
openStroke pp = makeGraphic stroke_attr (\(rgb,attr) -> ostroke rgb attr pp)



-- | 'closedStroke' : @ path -> Graphic @
--
-- This is the analogue to 'cstroke' in @Wumpus-core@, but the 
-- drawing properties (colour, line width, etc.) are taken from 
-- the implicit 'DrawingContext'.
--
closedStroke :: PrimPath -> Graphic u
closedStroke pp = 
    makeGraphic stroke_attr (\(rgb,attr) -> cstroke rgb attr pp)


-- | 'filledPath' : @ path -> Graphic @
-- 
-- This is the analogue to 'fill' in @Wumpus-core@, but the 
-- fill colour is taken from the implicit 'DrawingContext'.
--
--
filledPath :: PrimPath -> Graphic u
filledPath pp = makeGraphic fill_attr (\rgb -> fill rgb pp)
                 

-- | 'borderedPath' : @ path -> Graphic @
--
-- This is the analogue to 'fillStroke' in @Wumpus-core@, but the 
-- drawing properties (fill colour, border colour, line width, 
-- etc.) are taken from the implicit 'DrawingContext'.
--
--
borderedPath :: PrimPath -> Graphic u
borderedPath pp =
    makeGraphic bordered_attr 
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
    makeLocGraphic text_attr 
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
    makeLocThetaGraphic text_attr
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
    makeLocGraphic text_attr 
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
    makeLocThetaGraphic text_attr
                        (\(rgb,attr) pt ang -> rescapedlabel rgb attr esc ang pt)



-- | Unit parametric version of KerningChar from Wumpus-Core.
--
type KernChar u = (u,EscapedChar)

uconvKernChar :: InterpretUnit u => [KernChar u] -> Query [KerningChar]
uconvKernChar ks = 
    makeQuery point_size 
              (\sz -> map (\(u,ch) -> (uconvertScalar sz u,ch)) ks)




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
hkernline ks = 
    withLocQuery2 (uconvKernChar ks)
                  (\ans -> makeLocGraphic text_attr
                              (\(rgb,attr) pt -> hkernlabel rgb attr ans pt))


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
vkernline ks =
    withLocQuery2 (uconvKernChar ks)
                  (\ans -> makeLocGraphic text_attr
                              (\(rgb,attr) pt -> vkernlabel rgb attr ans pt))

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
straightLine p1 p2 = 
    withQuery (vertexPath [p1,p2]) openStroke


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
locStraightLine v = 
    withLocQuery (locPath [v]) openStroke



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
curvedLine p0 p1 p2 p3 = 
    withQuery (curvedPath [p0,p1,p2,p3]) openStroke




--------------------------------------------------------------------------------
-- Circles

-- | Helper for circle drawing.
--
circlePath :: InterpretUnit u 
         => u -> LocQuery u PrimPath
circlePath r = makeLocQuery point_size $ \sz pt  ->
    curvedPrimPath $ bezierCircle (uconvertScalar sz r) (uconvertExt sz pt) 




-- | 'strokedCircle' : @ radius -> LocGraphic @
--
-- Create a stroked circle 'LocGraphic' - the implicit point is 
-- center. The circle is drawn with four Bezier curves. 
-- 
-- The line properties (colour, pen thickness, etc.) are taken 
-- from the implicit 'DrawingContext'.
-- 
strokedCircle :: InterpretUnit u => u -> LocGraphic u
strokedCircle r = withLocQuery (circlePath r) openStroke




-- | 'filledCircle' : @ radius -> LocGraphic @
--
-- Create a filled circle 'LocGraphic' - the implicit point is 
-- center. The circle is drawn with four Bezier curves. 
-- 
-- The fill colour is taken from the implicit 'DrawingContext'.
-- 
filledCircle :: InterpretUnit u => u -> LocGraphic u
filledCircle r = withLocQuery (circlePath r) filledPath 



-- | 'borderedCircle' : @ radius -> LocGraphic @
--
-- Create a bordered circle 'LocGraphic' - the implicit point is 
-- center. The circle is drawn with four Bezier curves. 
-- 
-- The background fill colour and the outline stroke properties 
-- are taken from the implicit 'DrawingContext'.
-- 
borderedCircle :: InterpretUnit u => u -> LocGraphic u
borderedCircle r = withLocQuery (circlePath r) borderedPath


--------------------------------------------------------------------------------
-- Ellipses


-- | Helper for ellipse drawing.
--
ellipsePath :: InterpretUnit u 
            => u -> u -> LocQuery u PrimPath
ellipsePath rx ry = makeLocQuery point_size $ \sz pt  ->
    let drx = uconvertScalar sz rx
        dry = uconvertScalar sz ry 
    in curvedPrimPath $ bezierEllipse drx dry(uconvertExt sz pt) 



-- | Helper for ellipse drawing.
--
rellipsePath :: InterpretUnit u 
            => u -> u -> LocThetaQuery u PrimPath
rellipsePath rx ry = makeLocThetaQuery point_size $ \sz pt ang ->
    let drx = uconvertScalar sz rx
        dry = uconvertScalar sz ry 
    in curvedPrimPath $ rbezierEllipse drx dry ang (uconvertExt sz pt) 





-- | 'strokedEllipse' : @ x_radius * y_radius -> LocGraphic @
--
-- Create a stroked ellipse 'LocGraphic' - the implicit point is 
-- center. The ellipse is drawn with four Bezier curves. 
-- 
-- The line properties (colour, pen thickness, etc.) are taken 
-- from the implicit 'DrawingContext'.
-- 
strokedEllipse :: InterpretUnit u => u -> u -> LocGraphic u
strokedEllipse rx ry = withLocQuery (ellipsePath rx ry) closedStroke



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
rstrokedEllipse rx ry = withLocThetaQuery (rellipsePath rx ry) closedStroke




-- | 'filledEllipse' : @ x_radius * y_radius -> LocGraphic @
--
-- Create a filled ellipse 'LocGraphic' - the implicit point is 
-- center. The ellipse is drawn with four Bezier curves. 
-- 
-- The fill colour is taken from the implicit 'DrawingContext'.
-- 
filledEllipse :: InterpretUnit u => u -> u -> LocGraphic u
filledEllipse rx ry = withLocQuery (ellipsePath rx ry) filledPath


-- | 'rfilledEllipse' : @ x_radius * y_radius -> LocGraphic @
--
-- Create a filled ellipse 'LocThetaGraphic' - the implicit point
-- is center and the angle is rotation about the center. The 
-- ellipse is drawn with four Bezier curves.  
-- 
-- The fill colour is taken from the implicit 'DrawingContext'.
-- 
rfilledEllipse :: InterpretUnit u => u -> u -> LocThetaGraphic u
rfilledEllipse rx ry = withLocThetaQuery (rellipsePath rx ry) filledPath



-- | 'borderedEllipse' : @ x_radius * y_radius -> LocGraphic @
--
-- Create a bordered ellipse 'LocGraphic' - the implicit point is 
-- center. The ellipse is drawn with four Bezier curves. 
-- 
-- The background fill colour and the outline stroke properties 
-- are taken from the implicit 'DrawingContext'.
-- 
borderedEllipse :: InterpretUnit u => u -> u -> LocGraphic u
borderedEllipse rx ry = withLocQuery (ellipsePath rx ry) borderedPath



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
rborderedEllipse rx ry = withLocThetaQuery (rellipsePath rx ry) borderedPath



-- Note - clipping to do...

--------------------------------------------------------------------------------
-- Rectangles

-- | Supplied point is /bottom-left/.
--
rectanglePath :: InterpretUnit u 
              => u -> u -> LocQuery u PrimPath
rectanglePath w h = locPath [hvec w, vvec h, hvec (-w)]


-- | 'strokedRectangle' : @ width * height -> LocGraphic @
--
-- Create a stroked rectangle 'LocGraphic' - the implicit point is 
-- bottom-left. 
-- 
-- The line properties (colour, pen thickness, etc.) are taken 
-- from the implicit 'DrawingContext'.
-- 
strokedRectangle :: InterpretUnit u => u -> u -> LocGraphic u
strokedRectangle w h = withLocQuery (rectanglePath w h) closedStroke


-- | 'filledRectangle' : @ width * height -> LocGraphic @
--
-- Create a filled rectangle 'LocGraphic' - the implicit point is 
-- the bottom-left. 
-- 
-- The fill colour is taken from the implicit 'DrawingContext'.
-- 
filledRectangle :: InterpretUnit u => u -> u -> LocGraphic u
filledRectangle w h = withLocQuery (rectanglePath w h) filledPath


-- | 'borderedRectangle' : @ width * height -> LocGraphic @
--
-- Create a bordered rectangle 'LocGraphic' - the implicit point is 
-- bottom-left. 
-- 
-- The background fill colour and the outline stroke properties 
-- are taken from the implicit 'DrawingContext'.
-- 
borderedRectangle :: InterpretUnit u => u -> u -> LocGraphic u
borderedRectangle w h = withLocQuery (rectanglePath w h) borderedPath 



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
    withLocQuery2 (scalarSize radius)
                  (\r -> makeLocGraphic stroke_attr
                              (\(rgb,attr) pt -> strokeEllipse rgb attr r r pt))




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
    withLocQuery2 (scalarSize radius)
                  (\r -> makeLocGraphic fill_attr
                              (\rgb pt -> fillEllipse rgb r r pt))


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
    withLocQuery2 (scalarSize radius)
                  (\r -> makeLocGraphic bordered_attr
                           (\(frgb,attr,srgb) pt -> 
                               fillStrokeEllipse frgb attr srgb r r pt))


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
strokedEllipseDisk rx ry =
    withLocQuery2 (scalarPair rx ry)
                  (\(drx,dry) -> makeLocGraphic stroke_attr
                       (\(rgb,attr) pt -> strokeEllipse rgb attr drx dry pt))



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
filledEllipseDisk rx ry =
    withLocQuery2 (scalarPair rx ry)
                  (\(drx,dry) -> makeLocGraphic fill_attr
                       (\rgb pt -> fillEllipse rgb drx dry pt))


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
borderedEllipseDisk rx ry = 
    withLocQuery2 (scalarPair rx ry)
                  (\(drx,dry) -> makeLocGraphic bordered_attr
                       (\(frgb,attr,srgb) pt -> 
                            fillStrokeEllipse frgb attr srgb drx dry pt))
