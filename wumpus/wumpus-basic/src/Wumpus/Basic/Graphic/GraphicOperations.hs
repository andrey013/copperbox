{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Graphic.GraphicOperations
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  highly unstable
-- Portability :  GHC 
--
-- Elementary functions for the Graphic and LocGraphic types.
--
-- The functions here are generally analogeous to the Picture 
-- API in @Wumpus.Core@, but here they exploit the implicit 
-- @DrawingContext@.
--
--------------------------------------------------------------------------------

module Wumpus.Basic.Graphic.GraphicOperations
  (
    drawGraphic

  , openStroke
  , closedStroke
  , filledPath
  , borderedPath

  
  , textline
  , rtextline
  , singleLine
  , centermonoTextline
  , escapedline
  , rescapedline

  , textlineMulti
  , hkernline
  , vkernline


  , strokedEllipse
  , filledEllipse  
  , borderedEllipse

  , supplyPt
  , localPoint
  , vecdisplace
  , displace
  , hdisplace
  , vdisplace
  , parallelvec
  , perpendicularvec
  , displaceParallel
  , displacePerpendicular


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

import Wumpus.Basic.Graphic.Base
import Wumpus.Basic.Graphic.ContextFunction
import Wumpus.Basic.Graphic.DrawingContext
import Wumpus.Basic.Graphic.GraphicTypes
import Wumpus.Basic.Graphic.Query

import Wumpus.Core                              -- package: wumpus-core
import Wumpus.Core.Text.GlyphIndices

import Data.AffineSpace                         -- package: vector-space
import Data.VectorSpace

import Data.Char
import Data.Foldable ( foldrM )
import qualified Data.Map               as Map
import Data.Maybe 



drawGraphic :: (Real u, Floating u, FromPtSize u) 
            => DrawingContext -> Graphic u -> Picture u
drawGraphic ctx gf = frame [getPrimGraphic $ runGraphic ctx gf]




openStroke :: Num u => PrimPath u -> Graphic u
openStroke pp = 
    withStrokeAttr $ \rgb attr -> primGraphic $ ostroke rgb attr pp

closedStroke :: Num u => PrimPath u -> Graphic u
closedStroke pp = 
    withStrokeAttr $ \rgb attr -> primGraphic $ cstroke rgb attr pp

filledPath :: Num u => PrimPath u -> Graphic u
filledPath pp = withFillAttr $ \rgb -> primGraphic $ fill rgb pp
                 


borderedPath :: Num u => PrimPath u -> Graphic u
borderedPath pp =
    withBorderedAttr $ \frgb attr srgb -> 
                           primGraphic $ fillStroke frgb attr srgb pp


-- Note - clipping needs a picture as well as a path, so there is
-- no analogous @clippedPath@ function.


--------------------------------------------------------------------------------
-- 

locPrimGraphic :: (Point2 u -> Primitive u) -> (Point2 u -> PrimGraphic u)
locPrimGraphic fn = primGraphic . fn

thetaLocPrimGraphic :: (Point2 u -> Radian -> Primitive u) 
                    -> (Point2 u -> Radian -> PrimGraphic u) 
thetaLocPrimGraphic fn = \pt theta -> primGraphic (fn pt theta)




textline :: Num u => String -> LocGraphic u
textline ss =
    withTextAttr $ \rgb attr -> locPrimGraphic (textlabel rgb attr ss)



rtextline :: Num u => String -> LocThetaGraphic u
rtextline ss = 
    withTextAttr $ \rgb attr -> thetaLocPrimGraphic 
                                  (\pt ang -> rtextlabel rgb attr ss pt ang)


-- Needs descender depth? ...

singleLine :: (Ord u, FromPtSize u) => String -> LocImage u (BoundingBox u)
singleLine ss = 
    let cs = escapeString ss in 
    textVector cs     >>= \av    -> 
    glyphHeightRange  >>= \y_range ->
    intoLocImage (raise1 $ measuredTextBBox (advanceH av) y_range)
                 (escapedline cs)  


-- | Measured text box for left-to-right text.
-- 
-- Supplied point is baseline left.
-- 
measuredTextBBox :: (Num u, Ord u) => u -> (u,u) -> Point2 u -> BoundingBox u
measuredTextBBox w (ymin,ymax) (P2 x y) = 
    boundingBox (P2 x (y-ymin)) (P2 (x+w) (y+ymax))

textVector :: FromPtSize  u => EscapedText -> CF (AdvanceVec u)
textVector ss = let cs = getEscapedText ss in 
   foldrM (\c v -> charVector c >>= \cv -> return  (v ^+^ cv)) (vec 0 0) cs


charVector :: FromPtSize u => EscapedChar -> CF (AdvanceVec u)
charVector (CharLiteral c) = avLookupTable `situ1` (ord c)
charVector (CharEscInt i)  = avLookupTable `situ1` i
charVector (CharEscName s) = avLookupTable `situ1` ix
  where
    ix = fromMaybe (-1) $ Map.lookup s ps_glyph_indices




-- | As 'textline' but the supplied point is the /center/.
--
-- Centered is inexact - it is calculated with monospaced font
-- metrics.
-- 
centermonoTextline :: (Fractional u, Ord u, FromPtSize u) 
                   => String -> LocGraphic u
centermonoTextline ss = monoVecToCenter ss >>= \v ->
                          moveLoc (vecdisplace (negateV v)) (textline ss)



escapedline :: Num u => EscapedText -> LocGraphic u
escapedline ss =
    withTextAttr $ \rgb attr -> locPrimGraphic (escapedlabel rgb attr ss)


rescapedline :: Num u => EscapedText -> LocThetaGraphic u
rescapedline ss = 
    withTextAttr $ \rgb attr -> thetaLocPrimGraphic 
                                  (\pt ang -> rescapedlabel rgb attr ss pt ang)



-- | Point is the baseline left of the bottom line, text is 
-- left-aligned.
--
textlineMulti :: Fractional u => [String] -> LocGraphic u
textlineMulti xs = baselineSpacing >>= \dy -> 
    extrLocGraphic $ go (tmStep dy) xs
  where
    -- go /starts/ at the end of the list and works back.
    go fn []      = fn ""       -- not ideal, better than error
    go fn [s]     = fn s
    go fn (s:ss)  = let ans = go fn ss in ans `feedPt` fn s

-- LocImage u (Point2 u) deserved to be a new type synonym
-- as it models PostScript\'s @show@ 


tmStep :: Num u => u -> String -> LocImage u (Point2 u) 
tmStep dy str = intoLocImage (raise $ \pt -> pt .+^ vvec dy) (textline str)

feedPt :: LocImage u (Point2 u) -> LocImage u (Point2 u) -> LocImage u (Point2 u) 
feedPt = accumulate1 oplus

hkernline :: Num u => [KerningChar u] -> LocGraphic u
hkernline ks = 
    withTextAttr $ \rgb attr -> locPrimGraphic (hkernlabel rgb attr ks)
      

vkernline :: Num u => [KerningChar u] -> LocGraphic u
vkernline ks = 
    withTextAttr $ \rgb attr -> locPrimGraphic (vkernlabel rgb attr ks)
  


--------------------------------------------------------------------------------


strokedEllipse :: Num u => u -> u -> LocGraphic u
strokedEllipse hw hh =  
    withStrokeAttr $ \rgb attr -> locPrimGraphic (strokeEllipse rgb attr hw hh)
   

filledEllipse :: Num u => u -> u -> LocGraphic u
filledEllipse hw hh =  
    withFillAttr $ \rgb -> locPrimGraphic (fillEllipse rgb hw hh)
  

borderedEllipse :: Num u => u -> u -> LocGraphic u
borderedEllipse hw hh = 
    withBorderedAttr $ \frgb attr srgb -> 
      locPrimGraphic (fillStrokeEllipse frgb attr srgb hw hh)

--------------------------------------------------------------------------------


-- | Supplying a point to a 'CFGraphic' takes it to a regular 
-- 'Graphic'.
--
supplyPt :: Point2 u -> LocGraphic u -> Graphic u
supplyPt pt gf = fmap ($ pt) gf 



vecdisplace :: Num u => Vec2 u -> PointDisplace u
vecdisplace (V2 dx dy) (P2 x y) = P2 (x+dx) (y+dy)


displace :: Num u => u -> u -> PointDisplace u
displace dx dy (P2 x y) = P2 (x+dx) (y+dy)

hdisplace :: Num u => u -> PointDisplace u
hdisplace dx (P2 x y) = P2 (x+dx) y

vdisplace :: Num u => u -> PointDisplace u
vdisplace dy (P2 x y) = P2 x (y+dy)




parallelvec :: Floating u => u -> Radian -> Vec2 u
parallelvec d r         = avec (circularModulo r) d

perpendicularvec :: Floating u => u -> Radian -> Vec2 u
perpendicularvec d r    = avec (circularModulo $ (0.5*pi) + r) d

displaceParallel :: Floating u => u -> Radian -> PointDisplace u
displaceParallel d r pt = pt .+^ parallelvec d r

displacePerpendicular :: Floating u => u -> Radian -> PointDisplace u
displacePerpendicular d r pt = pt .+^ perpendicularvec d r


localPoint :: (Point2 u -> Point2 u) -> LocGraphic u -> LocGraphic u
localPoint = moveLoc



--------------------------------------------------------------------------------


straightLine :: Fractional u => Vec2 u -> LocGraphic u
straightLine v = 
    promote1 openStroke `cxpost1` (raise $ \pt -> path pt [lineTo $ pt .+^ v])

          

straightLineBetween :: Fractional u => Point2 u -> Point2 u -> Graphic u
straightLineBetween p1 p2 = openStroke $ path p1 [lineTo p2]



curveBetween :: Fractional u 
             => Point2 u -> Point2 u -> Point2 u -> Point2 u -> Graphic u
curveBetween sp cp1 cp2 ep = openStroke $ path sp [curveTo cp1 cp2 ep]



-- | Supplied point is /bottom-left/.
--
rectangle :: Num u => u -> u -> Point2 u -> PrimPath u
rectangle w h bl = path bl [ lineTo br, lineTo tr, lineTo tl ]
  where
    br = bl .+^ hvec w
    tr = br .+^ vvec h
    tl = bl .+^ vvec h 

-- > promote __ `cxpost1` (raise $ __) 
--
-- is a pattern captured by the cardinal' combinator.
-- 

drawWith :: (PrimPath u -> Graphic u) -> (Point2 u -> PrimPath u) -> LocGraphic u 
drawWith = cardinalprime

-- | Supplied point is /bottom left/.
--
strokedRectangle :: Fractional u => u -> u -> LocGraphic u
strokedRectangle w h = drawWith closedStroke (rectangle w h)


-- | Supplied point is /bottom left/.
--
filledRectangle :: Fractional u => u -> u -> LocGraphic u
filledRectangle w h = drawWith borderedPath (rectangle w h) 

-- | Supplied point is /bottom left/.
--
borderedRectangle :: Fractional u => u -> u -> LocGraphic u
borderedRectangle w h = drawWith borderedPath (rectangle w h) 




--------------------------------------------------------------------------------


-- | Supplied point is center. Circle is drawn with Bezier 
-- curves. 
--
strokedCircle :: Floating u => Int -> u -> LocGraphic u
strokedCircle n r = drawWith closedStroke (curvedPath . bezierCircle n r)



-- | Supplied point is center. Circle is drawn with Bezier 
-- curves. 
--
filledCircle :: Floating u => Int -> u -> LocGraphic u
filledCircle n r = drawWith filledPath (curvedPath . bezierCircle n r)



-- | Supplied point is center. Circle is drawn with Bezier 
-- curves. 
--
borderedCircle :: Floating u => Int -> u -> LocGraphic u
borderedCircle n r = drawWith borderedPath (curvedPath . bezierCircle n r)



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


filledDisk :: Num u => u -> LocGraphic u
filledDisk radius = filledEllipse radius radius

borderedDisk :: Num u => u -> LocGraphic u
borderedDisk radius = borderedEllipse radius radius

