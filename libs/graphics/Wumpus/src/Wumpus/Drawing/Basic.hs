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
import Wumpus.Core.Frame
import Wumpus.Core.Fun
import Wumpus.Core.Line
import Wumpus.Core.Point
import Wumpus.Core.Pointwise
import Wumpus.Core.Polygon
import Wumpus.Core.Transformations
import Wumpus.Core.Vector

import Wumpus.Drawing.PostScript

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

{-

--------------------------------------------------------------------------------
-- Picture data type - composing PostScript drawings

-- Acknowledgment - the Picture data type is modelled directly on the 
-- Picture/Image types in Antony Courtney's Haven.

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

-}

--------------------------------------------------------------------------------

newtype Picture = Picture { 
       getPicture   :: DFrame2 -> (WumpusM (), DBoundingBox)
    }


infixr 6 <++>
infixr 5 <//>

(<++>) :: Picture -> Picture -> Picture
picL <++> picR = Picture $ \frm -> 
    let (cmdl,bbl) = (getPicture picL) frm
        (_,bbtemp) = (getPicture picR) frm
        vdiff      = east bbl .-. west bbtemp
        (cmdr,bbr) = (getPicture picR) (displaceOrigin vdiff frm)
    in (cmdl >> cmdr, bbl `mappend` bbr) 


(<//>) :: Picture -> Picture -> Picture
picT <//> picB = Picture $ \frm -> 
    let (cmdt,bbt) = (getPicture picT) frm
        (_,bbtemp) = (getPicture picB) frm
        vdiff      = south bbt .-. north bbtemp
        (cmdb,bbb) = (getPicture picB) (displaceOrigin vdiff frm)
    in (cmdt >> cmdb, bbt `mappend` bbb) 


centered :: Picture -> Picture -> Picture
picT `centered` picB = Picture $ \frm -> 
    let (cmdt,bbt) = (getPicture picT) frm
        (_,bbtemp) = (getPicture picB) frm
        vdiff      = center bbt .-. center bbtemp
        (cmdb,bbb) = (getPicture picB) (displaceOrigin vdiff frm)
    in (cmdt >> cmdb, bbt `mappend` bbb) 


picEmpty :: Picture
picEmpty = Picture $ \frm -> let o = origin frm in (return (), BBox o o) 



hcat :: [Picture] -> Picture
hcat = foldl' (<++>) picEmpty

vcat :: [Picture] -> Picture
vcat = foldl' (<//>) picEmpty


-- | Repeat a picture @n@ times, at each iteration displace by the 
-- vector @disp@.
multiput :: Int -> DVec2 -> Picture -> Picture
multiput n disp pic = Picture $ \frm ->
    foldl' (fn frm) (return(),mempty) vecs
  where
    vecs :: [DVec2]
    vecs  = scanl (+) disp (replicate (n-1) disp)

    fn :: DFrame2 -> (WumpusM (), DBoundingBox) -> DVec2 -> (WumpusM (), DBoundingBox)
    fn frm (mf,bb) (V2 x y) = 
       prod (mf >>) (bb `mappend`) $ (getPicture $ displace x y pic) frm


picPolygon :: DPolygon -> Picture
picPolygon p = Picture $ \frm -> 
  let p' = withinFrame frm p in (strokePolygon p', boundingBox p')


picLines :: [DLineSegment2] -> Picture
picLines xs = Picture $ \frm -> 
    let xs' = map (withinFrame frm) xs
    in (mapM_ drawLine xs', bounds xs')


displace :: Double -> Double -> Picture -> Picture
displace x y p = Picture $ \frm -> (getPicture p) $ displaceOrigin (V2 x y) frm

picColour :: DRGB -> Picture -> Picture
picColour c pic = Picture $ \frm ->
    let (mf,bb) = (getPicture pic) frm in (withRgbColour c mf,bb)


psDraw :: Picture -> PostScript
psDraw pic = runWumpus env0 (fst $ (getPicture pic) (ortho zeroPt))


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

--------------------------------------------------------------------------------


data PathCmd = Stroke | Fill | Clip
  deriving (Eq,Show) 

-- can only stroke an /open/ path... (fill has odd behaviour)
strokeOpenPathSkel :: WumpusM a -> WumpusM a
strokeOpenPathSkel m = do
  ps_newpath
  a <- m
  ps_stroke
  return a

closedPathSkel :: PathCmd -> WumpusM a -> WumpusM a
closedPathSkel pcmd m = do
    ps_newpath
    a <- m
    ps_closepath
    drawCmd pcmd
    return a
  where
    drawCmd Stroke = ps_stroke
    drawCmd Fill   = ps_fill   
    drawCmd Clip   = ps_clip

fillPathSkel :: WumpusM a -> WumpusM a
fillPathSkel = closedPathSkel Fill

strokePathSkel :: WumpusM a -> WumpusM a
strokePathSkel = closedPathSkel Stroke

clipPathSkel :: WumpusM a -> WumpusM a
clipPathSkel = closedPathSkel Clip



drawLine :: DLineSegment2 -> WumpusM ()
drawLine (LS (P2 x1 y1) (P2 x2 y2)) = strokePathSkel $ do 
    ps_moveto x1 y1
    ps_lineto x2 y2

drawPoint :: DPoint2 -> WumpusM ()
drawPoint = strokePolygon . unitSquare



strokePolygon :: DPolygon -> WumpusM ()
strokePolygon (Polygon [])            = return ()
strokePolygon (Polygon ((P2 x y):ps)) =
    strokePathSkel $ ps_moveto x y >> mapM_ (\(P2 a b) -> ps_lineto a b) ps 

fillPolygon :: DPolygon -> WumpusM ()
fillPolygon (Polygon [])            = return ()
fillPolygon (Polygon ((P2 x y):ps)) =
    fillPathSkel $ ps_moveto x y >> mapM_ (\(P2 a b) -> ps_lineto a b) ps 


clipPolygon :: DPolygon -> WumpusM a -> WumpusM a
clipPolygon (Polygon [])            mf = mf
clipPolygon (Polygon ((P2 x y):ps)) mf = clip >> mf 
  where
    clip = clipPathSkel $ ps_moveto x y >> mapM_ (\(P2 a b) -> ps_lineto a b) ps




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
dotSquare = Picture $ \frm -> 
    let sq = (withinFrame frm $ square 4 zeroPt) in (strokePolygon sq, boundingBox sq)


dotPlus :: Picture
dotPlus = Picture $ \frm -> 
    (mapM_ drawLine [lh frm,lv frm], bounds [lh frm,lv frm])
  where
    lh  = translate (-2) 0 . line (hvec 4::Vec2 Double) . origin
    lv  = translate 0 (-2) . line (vvec 4) . origin


dotX :: Picture 
dotX = Picture $ \frm -> 
     (mapM_ drawLine [ls1 (origin frm), ls2 (origin frm)], 
      bounds [ls1 (origin frm), ls2 (origin frm)])
  where
    ls1 o = rotateAbout (pi/6) o $ translate 0 (-2) $ line (vvec 4 :: Vec2 Double) o
    ls2 o = rotateAbout (5*pi/3) o $ ls1 o




dotAsterisk :: Picture
dotAsterisk = Picture $ \frm -> 
    let ls =  trans frm $ circular (replicate 5 $ ls1)
    in (mapM_ drawLine ls, bounds ls)
  where
   ls1 = vline 2 zeroPt
   trans (Frame2 o vx vy) xs = map (translate x y) xs where
     (P2 x y) = (o .+^ vx) .+^ vy 



polyDot :: Int -> Picture
polyDot sides = Picture $ \frm -> 
    let pgon = regularPolygon sides 2 (origin frm) in (strokePolygon pgon, boundingBox pgon)


dotTriangle :: Picture
dotTriangle = polyDot 3

dotDiamond :: Picture
dotDiamond = polyDot 4


dotPentagon :: Picture
dotPentagon = polyDot 5
 
