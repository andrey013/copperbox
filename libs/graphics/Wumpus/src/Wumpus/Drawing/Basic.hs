{-# LANGUAGE TypeFamilies               #-}
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

import Wumpus.Core.BoundingBox
import Wumpus.Core.Colour
import Wumpus.Core.Curve
import Wumpus.Core.Fun
import Wumpus.Core.Line
import Wumpus.Core.Point
import Wumpus.Core.Pointwise
import Wumpus.Core.Polygon
import Wumpus.Core.Transformations
import Wumpus.Core.Vector

import Wumpus.Drawing.PostScript
import Wumpus.Drawing.PSSkeletons

import Data.AffineSpace

import Data.List
import Data.Monoid

type Radius = Double
type Origin = DPoint2



data Circle = Circle Origin Radius
  deriving (Eq,Show)



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
psDraw pic = runWumpus env0 (fst $ (getPicture pic) zeroPt)


place :: Picture -> DPoint2 -> Picture
place pic p2 = Picture $ \_ -> (getPicture pic) p2

picEmpty :: Picture
picEmpty = Picture $ \pt -> (return (), BBox pt pt) 
 
picPolygon :: (DPoint2 -> DPolygon) -> Picture
picPolygon pf = Picture $ \pt -> (strokePolygon $ pf pt, boundingBox $ pf pt) 


picColour :: DRGB -> Picture -> Picture
picColour c pic = Picture $ 
    \pt -> let (mf,bb) = (getPicture pic) pt in (withRgbColour c mf,bb)

picLines :: [DPoint2 -> DLineSegment2] -> Picture
picLines xs = Picture $ \pt -> 
    (mapM_ drawLine $ sequence xs pt, bounds $ sequence xs pt)


-- | Repeat a picture @n@ times, at each iteration displace by the 
-- vector @disp@.
multiput :: Int -> DVec2 -> Picture -> Picture
multiput n disp pic = Picture $ \pt ->
    foldl' fn (return(),mempty) (mkPoints pt)
  where
    mkPoints pt = scanl (.+^) pt (replicate n disp)
    fn (mf,bb) pt = prod (mf >>) (bb `mappend`) $ (getPicture pic) pt


type BBTransf a = BoundingBox a -> BoundingBox a -> Vec2 a


composePics :: BBTransf Double -> Picture -> Picture -> Picture
composePics bbDif pic base = Picture $ 
    \pt -> let (mfb,bxb) = (getPicture base) pt
               (_,bb)    = (getPicture pic) pt
               v1        = bbDif bb bxb
               (mfa,bxa) = (getPicture pic) $ pt .+^ v1 
           in (mfb >> mfa, bxb `mappend` bxa) 



below :: Picture -> Picture -> Picture
below = composePics (\a b -> south b .-. north a)

above :: Picture -> Picture -> Picture
above = composePics (\a b -> north b .-. south a)

-- Note composePics composes @a@ wrt @b@,  here we need 
-- to compose @b@ wrt @a@.
(<+>) :: Picture -> Picture -> Picture
(<+>) = flip $ composePics (\a b -> east b .-. west a)

-- Note composePics composes @a@ wrt @b@,  here we need 
-- to compose @b@ wrt @a@.
(</>) :: Picture -> Picture -> Picture
(</>) = flip $ composePics (\a b -> south b .-. north a)



-- center picture a inside picture b
centeredInside :: Picture -> Picture -> Picture
centeredInside = composePics (\a b -> center b .-. center a)


hcat :: [Picture] -> Picture
hcat = foldl' (<+>) picEmpty

vcat :: [Picture] -> Picture
vcat = foldl' (</>) picEmpty


--------------------------------------------------------------------------------

-- | Generate a list of points [pt, f pt, f (f pt), ...] while the 
-- predicate @p@ holds. 
genPoints :: (Point2 a -> Bool) -> (Point2 a -> Point2 a) -> Point2 a -> [Point2 a]
genPoints p f = unfoldr phi where
  phi pt | p pt      = Just (pt,f pt)
         | otherwise = Nothing





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
strokePolygon (Polygon ((P2 x y):ps)) = do 
    ps_newpath
    ps_moveto x y
    mapM_ (\(P2 a b) -> ps_lineto a b) ps 
    ps_closepath
    ps_stroke


clipPolygon :: DPolygon -> WumpusM a -> WumpusM a
clipPolygon (Polygon [])            mf = mf
clipPolygon (Polygon ((P2 x y):ps)) mf = do 
    ps_newpath
    ps_moveto x y
    mapM_ (\(P2 a b) -> ps_lineto a b) ps 
    ps_closepath
    ps_clip
    mf




drawLineBag :: [DLineSegment2] -> WumpusM ()
drawLineBag []  = return ()
drawLineBag xs  = strokeOpenPathSkel $ mapM_ step xs
  where
    step (LS (P2 x1 y1) (P2 x2 y2)) = do 
      ps_moveto x1 y1
      ps_lineto x2 y2



diamond :: Double -> Double -> (DPoint2 -> DPolygon)
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


dotSquare :: Picture
dotSquare = Picture $ \pt -> 
    let sq = square 4 pt in (strokePolygon sq, boundingBox sq)


dotPlus :: Picture
dotPlus = picLines [lh,lv] where
  lh  = translate (-2) 0 . line (hvec 4::Vec2 Double)
  lv  = translate 0 (-2) . line (vvec 4)


dotX :: Picture 
dotX = picLines [ls1,ls2] where
  ls1 o = rotateAbout (pi/6) o $ translate 0 (-2) $ line (vvec 4 :: Vec2 Double) o
  ls2 o = rotateAbout (5*pi/3) o $ ls1 o




dotAsterisk :: Picture
dotAsterisk = Picture $ \(P2 x y) -> 
    let ls =  map (translate x y) $ circular (replicate 5 $ ls1)
    in (mapM_ drawLine ls, bounds $ ls)
  where
   ls1 = vline 2 zeroPt



polyDot :: Int -> Picture
polyDot sides = Picture $ \pt -> 
    let pgon = regularPolygon sides 2 pt in (strokePolygon pgon, boundingBox pgon)


dotTriangle :: Picture
dotTriangle = polyDot 3

dotDiamond :: Picture
dotDiamond = polyDot 4


dotPentagon :: Picture
dotPentagon = polyDot 5
 

drawLineSegment :: DLineSegment2 -> WumpusM ()
drawLineSegment (LS p p') = strokeOpenPathSkel $  do 
    movetoPt p
    linetoPt p'