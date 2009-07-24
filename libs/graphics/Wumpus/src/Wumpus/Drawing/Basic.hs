{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Drawing.Basic
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Basic shapes etc.
--
--------------------------------------------------------------------------------


module Wumpus.Drawing.Basic where

import Wumpus.Core.Colour
import Wumpus.Core.Curve
import Wumpus.Core.Line
import Wumpus.Core.Point
import Wumpus.Core.Pointwise
import Wumpus.Core.Polygon
import Wumpus.Core.Transformations
import Wumpus.Core.Vector

import Wumpus.Drawing.PostScript
import Wumpus.Drawing.PSSkeletons

import Data.AffineSpace

import Data.Monoid

type Radius = Double
type Origin = DPoint2


type LineBag     = DPoint2 -> [DLineSegment2]  -- unjoined lines


data Circle = Circle Origin Radius
  deriving (Eq,Show)


instance Pointwise LineBag where
  type Pt LineBag = DPoint2
  pointwise f pf = pf . f


instance Pointwise Circle where
  type Pt Circle = DPoint2
  pointwise f (Circle o r) = Circle (f o) r


--------------------------------------------------------------------------------
-- Picture data type - composing PostScript drawings

-- Acknowledgment - the Image data type is modelled diratcly after the 
-- Image type in Antony Courtney's Haven.

newtype Picture = Picture { 
       getPicture   :: DPoint2 -> (WumpusM (), DBoundingBox)
    }


psDraw :: Picture -> PostScript
psDraw pic = runWumpus st0 (fst $ (getPicture pic) zeroPt)


place :: Picture -> DPoint2 -> Picture
place pic p2 = Picture $ \_ -> (getPicture pic) p2

 
 
picPolygon :: DCoPolygon -> Picture
picPolygon pf = Picture $ \pt -> (strokePolygon $ pf pt, boundingBox $ pf pt) 


picColour :: DRGB -> Picture -> Picture
picColour c pic = Picture $ 
    \pt -> let (mf,bb) = (getPicture pic) pt in (withColour c mf,bb)




over :: Picture -> Picture -> Picture
over pic1 pic2 = Picture $ 
    \pt -> let (mf1,bb1) = (getPicture pic1) pt
               (mf2,bb2) = (getPicture pic2) pt 
           in (mf1 >> mf2, bb1 `mappend` bb2) 

below :: Picture -> Picture -> Picture
below pic1 pic2 = Picture $ 
    \pt -> let (mf1,bb1) = (getPicture pic1) pt
               (_,bb)    = (getPicture pic2) pt
               v1        = centerTop bb .-. centerBottom bb1
               (mf2,bb2) = (getPicture pic2) $ pt .-^ v1 
           in (mf1 >> mf2, bb1 `mappend` bb2) 

centered :: Picture -> Picture 
centered pic = Picture $ \pt -> let (_,bb) = (getPicture pic) pt
                                    pt'    = center bb
                                in (getPicture pic) pt'



--------------------------------------------------------------------------------







--------------------------------------------------------------------------------


transOrigin :: (Floating a, Pointwise sh, Pt sh ~ Point2 a) 
            => sh -> Point2 a -> sh
transOrigin z = \(P2 x y) -> pointwise (translate x y) z




drawLine :: DLineSegment2 -> WumpusM ()
drawLine (LS (P2 x1 y1) (P2 x2 y2)) = strokePathSkel $ do 
    ps_moveto x1 y1
    ps_lineto x2 y2

drawPoint :: DPoint2 -> WumpusM ()
drawPoint = polygon . unitSquare

-- should this generate a DPolygon or its path?
-- unitSquare :: Point -> DPolygon
unitSquare :: DPoint2 -> [DPoint2]
unitSquare p = usqr where 
    usqr = [p, p .+^ (V2 0 1), p .+^ (V2 1 1), p .+^ (V2 1 0)]


-- to do - differentiate between draw / stroke / fill / clip
drawPolygon :: DPolygon -> WumpusM ()
drawPolygon = strokePolygon


strokePolygon :: DPolygon -> WumpusM ()
strokePolygon (Polygon [])            = return ()
strokePolygon (Polygon ((P2 x y):ps)) = saveExecRestore $ do 
    ps_newpath
    ps_moveto x y
    mapM_ (\(P2 a b) -> ps_lineto a b) ps 
    ps_closepath
    ps_stroke


drawLineBag :: [DLineSegment2] -> WumpusM ()
drawLineBag []  = return ()
drawLineBag xs  = strokeOpenPathSkel $ mapM_ step xs
  where
    step (LS (P2 x1 y1) (P2 x2 y2)) = do 
      ps_moveto x1 y1
      ps_lineto x2 y2

setRgbColour :: RgbColour -> WumpusM ()
setRgbColour (RGB3 r g b) = ps_setrgbcolor r g b



diamond :: Double -> Double -> DCoPolygon
diamond w h = \o -> Polygon $ map (o .+^) xs 
  where
    xs = [vh,vv, reflectX vh, reflectY vv]
    vh = hvec (w/2)
    vv = vvec (h/2)


--------------------------------------------------------------------------------
-- arcs and ellipses

circle :: (Double,Double) -> Double -> Circle
circle (x,y) r  = Circle (P2 x y) r

drawCircle  :: Circle -> WumpusM ()
drawCircle (Circle (P2 x y) r) = strokePathSkel $ 
  ps_arc x y r 0 360 

data Disk = Disk Origin Radius
  deriving (Eq,Show)
   
disk :: (Double,Double) -> Double -> Disk
disk (x,y) r = Disk (P2 x y) r

drawDisk  :: Disk -> WumpusM ()
drawDisk (Disk (P2 x y) r) = fillPathSkel $ do
  ps_arc x y r 0 360


drawCurve :: DCurve -> WumpusM ()
drawCurve (Curve (P2 x0 y0) (P2 x1 y1) (P2 x2 y2) (P2 x3 y3)) = 
  strokeOpenPathSkel $  do 
    ps_moveto x0 y0
    ps_curveto x1 y1 x2 y2 x3 y3


-- also draw control points
drawBezier :: DCurve -> WumpusM ()
drawBezier (Curve (P2 x0 y0) (P2 x1 y1) (P2 x2 y2) (P2 x3 y3)) = 
  strokeOpenPathSkel $  do 
    ps_moveto x1 y1             -- start from point1
    ps_lineto x0 y0
    ps_curveto x1 y1 x2 y2 x3 y3
    ps_lineto x2 y2



--------------------------------------------------------------------------------
-- dots


dotSquare :: DCoPolygon
dotSquare = square 2

dotPentagon :: DCoPolygon
dotPentagon = regularPolygon 5 2

dotPlus :: LineBag
dotPlus = sequence [lv,lh]
  where
    lh  = translate (-2) 0 . line (hvec 4::Vec2 Double)
    lv  = translate 0 (-2) . line (vvec 4)
   

dotX :: LineBag
dotX = sequence [ls1,ls2]
  where
    ls1 o = rotateAbout (pi/6) o $ translate 0 (-2) $ line (vvec 4 :: Vec2 Double) o
    ls2 o = rotateAbout (5*pi/3) o $ ls1 o


-- old dots...

dotAsterisk :: DPoint2 -> [DLineSegment2]
dotAsterisk (P2 x y) = map (translate x y) $ circular (replicate 5 ls1)
  where
   ls1 = vline 2 zeroPt


dotTriangle :: DPoint2 -> DPolygon
dotTriangle = polyDot (P2 0 2) 3



polyDot :: DPoint2 -> Int -> DPoint2 -> DPolygon
polyDot pt1 n (P2 x y) = Polygon xs
  where
    xs = map (translate x y) $ circular $ replicate n pt1 

dotDiamond :: DPoint2 -> DPolygon
dotDiamond (P2 x y) = Polygon (map (translate x y) [p1,p2,p3,p4])
  where
   p1 = P2 0 1.5
   p2 = P2 (-1) 0
   p3 = reflectY p1
   p4 = reflectX p2

 

drawLineSegment :: DLineSegment2 -> WumpusM ()
drawLineSegment (LS p p') = strokeOpenPathSkel $  do 
    movetoPt p
    linetoPt p'