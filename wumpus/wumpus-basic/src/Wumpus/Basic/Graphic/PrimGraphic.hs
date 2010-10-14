{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Graphic.PrimGraphic
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

module Wumpus.Basic.Graphic.PrimGraphic
  (
    drawGraphic

  , openStroke
  , closedStroke
  , filledPath
  , borderedPath
  
  , textline
  , rtextline
  , centermonoTextline
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
import Wumpus.Basic.Graphic.DrawingContext
import Wumpus.Basic.Graphic.Query

import Wumpus.Core                              -- package: wumpus-core

import Data.AffineSpace                         -- package: vector-space
import Data.VectorSpace

import Control.Applicative
import Data.Foldable ( foldrM )


drawGraphic :: (Real u, Floating u, FromPtSize u) 
            => DrawingContext -> Graphic u -> Picture u
drawGraphic ctx gf = frame [getPrimGraphic $ runGraphic ctx gf]




-- having the same names is actually not so useful...

openStroke :: Num u => PrimPath u -> Graphic u
openStroke pp = 
    withStrokeAttr $ \rgb attr -> wrapPrim $ ostroke rgb attr pp

closedStroke :: Num u => PrimPath u -> Graphic u
closedStroke pp = 
    withStrokeAttr $ \rgb attr -> wrapPrim $ cstroke rgb attr pp

filledPath :: Num u => PrimPath u -> Graphic u
filledPath pp = withFillAttr $ \rgb -> wrapPrim $ fill rgb pp
                 


borderedPath :: Num u => PrimPath u -> Graphic u
borderedPath pp = 
    withBorderedAttr $ \frgb attr srgb -> wrapPrim $ fillStroke frgb attr srgb pp


-- Note - clipping needs a picture as well as a path, so there is
-- no analogous @clippedPath@ function.


--------------------------------------------------------------------------------
-- 



textline :: Num u => String -> LocGraphic u
textline ss baseline_left =
    withTextAttr $ \rgb attr -> wrapPrim $ textlabel rgb attr ss baseline_left

rtextline :: Num u => String -> ThetaLocGraphic u
rtextline ss theta baseline_left =
    withTextAttr $ \rgb attr -> 
      wrapPrim $ rtextlabel rgb attr ss theta baseline_left

-- | As 'textline' but the supplied point is the /center/.
--
-- Centered is inexact - it is calculated with monospaced font
-- metrics.
-- 
centermonoTextline :: (Fractional u, Ord u, FromPtSize u) 
                   => String -> LocGraphic u
centermonoTextline ss pt = monoVecToCenter ss  >>= \v ->
                           textline ss (vecdisplace (negateV v) pt)





-- | Point is the baseline left of the bottom line, text is 
-- left-aligned.
--
textlineMulti :: Fractional u => [String] -> LocGraphic u
textlineMulti xs baseline_left =  
    baselineSpacing >>= \dy -> 
    foldrM (foldStep dy) (baseline_left,[]) xs >>= \(_,gs) ->
    return (wrapPrim $ primGroup gs)
  where
    foldStep dy str (pt,ac) = (\a -> (pt .+^ vvec dy, (getPrimGraphic a) : ac)) 
                                <$> textline str pt
                                


hkernline :: Num u => [KerningChar u] -> LocGraphic u
hkernline ks baseline_left = 
    withTextAttr $ \rgb attr -> wrapPrim $ hkernlabel rgb attr ks baseline_left
      

vkernline :: Num u => [KerningChar u] -> LocGraphic u
vkernline ks baseline_left = 
    withTextAttr $ \rgb attr -> wrapPrim $ vkernlabel rgb attr ks baseline_left
  


--------------------------------------------------------------------------------


strokedEllipse :: Num u => u -> u -> LocGraphic u
strokedEllipse hw hh pt =  
    withStrokeAttr $ \rgb attr -> wrapPrim $ strokeEllipse rgb attr hw hh pt
   

filledEllipse :: Num u => u -> u -> LocGraphic u
filledEllipse hw hh pt =  
    withFillAttr $ \rgb -> wrapPrim $ fillEllipse rgb hw hh pt
  

borderedEllipse :: Num u => u -> u -> LocGraphic u
borderedEllipse hw hh pt = 
    withBorderedAttr $ \frgb attr srgb -> 
      wrapPrim $ fillStrokeEllipse frgb attr srgb hw hh pt

--------------------------------------------------------------------------------


-- | Supplying a point to a 'CFGraphic' takes it to a regular 
-- 'Graphic'.
--
supplyPt :: Point2 u -> LocGraphic u -> Graphic u
supplyPt pt gf = gf pt 

vecdisplace :: Num u => Vec2 u -> Point2 u -> Point2 u
vecdisplace (V2 dx dy) (P2 x y) = P2 (x+dx) (y+dy)


displace :: Num u => u -> u -> Point2 u -> Point2 u
displace dx dy (P2 x y) = P2 (x+dx) (y+dy)

hdisplace :: Num u => u -> Point2 u -> Point2 u
hdisplace dx (P2 x y) = P2 (x+dx) y

vdisplace :: Num u => u -> Point2 u -> Point2 u
vdisplace dy (P2 x y) = P2 x (y+dy)


localPoint :: (Point2 u -> Point2 u) -> LocGraphic u -> LocGraphic u
localPoint upd gf = \pt -> gf (upd pt)


--------------------------------------------------------------------------------


straightLine :: Fractional u => Vec2 u -> LocGraphic u
straightLine v = \pt -> openStroke $ path pt [lineTo $ pt .+^ v]
          

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



-- | Supplied point is /bottom left/.
--
strokedRectangle :: Fractional u => u -> u -> LocGraphic u
strokedRectangle w h = closedStroke . rectangle w h



-- | Supplied point is /bottom left/.
--
filledRectangle :: Fractional u => u -> u -> LocGraphic u
filledRectangle w h = filledPath . rectangle w h
  

-- | Supplied point is /bottom left/.
--
borderedRectangle :: Fractional u => u -> u -> LocGraphic u
borderedRectangle w h = borderedPath . rectangle w h

--------------------------------------------------------------------------------


-- | Supplied point is center. Circle is drawn with Bezier 
-- curves. 
--
strokedCircle :: Floating u => Int -> u -> LocGraphic u
strokedCircle n r = closedStroke . curvedPath . bezierCircle n r



-- | Supplied point is center. Circle is drawn with Bezier 
-- curves. 
--
filledCircle :: Floating u => Int -> u -> LocGraphic u
filledCircle n r = filledPath . curvedPath . bezierCircle n r


-- | Supplied point is center. Circle is drawn with Bezier 
-- curves. 
--
borderedCircle :: Floating u => Int -> u -> LocGraphic u
borderedCircle n r = borderedPath . curvedPath . bezierCircle n r


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
