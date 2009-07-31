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
import Wumpus.Core.Path
import Wumpus.Core.Point
import Wumpus.Core.Pointwise
import Wumpus.Core.Polygon
import Wumpus.Core.Transformations
import Wumpus.Core.Vector

import Wumpus.Drawing.GraphicsState
import Wumpus.Drawing.PostScript

import Data.AffineSpace

import Data.List
import Data.Monoid

--------------------------------------------------------------------------------

newtype Picture = Picture { 
       getPicture   :: DFrame2 -> (WumpusM (), DBoundingBox)
    }

-- | Draw a picture, generating PostScript output.
psDraw :: Picture -> PostScript
psDraw pic = runWumpus env0 (fst $ (getPicture pic) (ortho zeroPt))

writePicture :: FilePath -> Picture -> IO ()
writePicture filepath pic = writeFile filepath $ psDraw pic

-- | Create an empty picture.
picEmpty :: Picture
picEmpty = Picture $ \frm -> let o = origin frm in (return (), BBox o o) 


infixr 7 <..>
infixr 6 <++>
infixr 5 <//>



-- | Neutral composition (i.e. union) of two pictures. Neither 
-- picture is moved - the composition just groups the pictures in 
-- a common bounding box.
(<..>) :: Picture -> Picture -> Picture 
pic1 <..> pic2 = Picture $ \frm -> 
    let (cmd1,bb1) = (getPicture pic1) frm
        (cmd2,bb2) = (getPicture pic2) frm
    in (cmd1 >> cmd2, bb1 `mappend` bb2) 


-- | Horizontal composition of pictures. (c.f pretty-print
-- combinator (<+>) ) 
(<++>) :: Picture -> Picture -> Picture
picL <++> picR = Picture $ \frm -> 
    let (cmdl,bbl) = (getPicture picL) frm
        (_,bbtemp) = (getPicture picR) frm
        vdiff      = east bbl .-. west bbtemp
        (cmdr,bbr) = (getPicture picR) (displaceOrigin vdiff frm)
    in (cmdl >> cmdr, bbl `mappend` bbr) 

-- | Vertical composition of pictures. 
(<//>) :: Picture -> Picture -> Picture
picT <//> picB = Picture $ \frm -> 
    let (cmdt,bbt) = (getPicture picT) frm
        (_,bbtemp) = (getPicture picB) frm
        vdiff      = south bbt .-. north bbtemp
        (cmdb,bbb) = (getPicture picB) (displaceOrigin vdiff frm)
    in (cmdt >> cmdb, bbt `mappend` bbb) 

-- | Center the second picture at the center point of the first picture. 
centered :: Picture -> Picture -> Picture
centered pic1 pic2 = Picture $ \frm -> 
    let (cmd1,bb1) = (getPicture pic1) frm
        (_,bbtemp) = (getPicture pic2) frm
        vdiff      = center bb1 .-. center bbtemp
        (cmd2,bb2) = (getPicture pic2) (displaceOrigin vdiff frm)
    in (cmd1 >> cmd2, bb1 `mappend` bb2) 

-- | Align the picture so that it's bottom-left corner is at the 
-- origin of the frame.
alignAtOrigin :: Picture -> Picture
alignAtOrigin pic = Picture $ \frm ->
  let (_,bbtemp) = (getPicture pic) frm
      vdiff      = southEast bbtemp .-. (origin frm)
  in (getPicture pic) (displaceOrigin vdiff frm)




-- | Concatenate all the pictures with (<..>) preserving there 
-- original positions.
cat :: [Picture] -> Picture
cat = foldl' (<..>) picEmpty


-- | Concatenate all the pictures horizontally with (<++>).
hcat :: [Picture] -> Picture
hcat = foldl' (<++>) picEmpty

-- | Concatenate all the pictures vertically with (<//>).
vcat :: [Picture] -> Picture
vcat = foldl' (<//>) picEmpty

-- | Concatenate all the pictures horizontally separated by a 
-- space of @xsep@ units.
hcatSep :: Double -> [Picture] -> Picture 
hcatSep _    []     = picEmpty
hcatSep xsep (x:xs) = foldl' fn x xs
  where
    fn acc a = acc <++> sep <++> a
    sep      = blankPicture xsep 0 


-- | Concatenate all the pictures vertically separated by a 
-- space of @ysep@ units.
vcatSep :: Double -> [Picture] -> Picture 
vcatSep _    []      = picEmpty
vcatSep ysep (x:xs)  = foldl' fn x xs
  where
    fn acc a = acc <//> sep <//> a
    sep      = blankPicture 0 ysep


-- | Create a blank (empty) Picture of width @w@ and height @h@.
blankPicture :: Double -> Double -> Picture
blankPicture w h = Picture $ \frm -> let rect = withinFrame frm (rectangle w h zeroPt) 
                                     in (return (), boundingBox rect)

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

picLine :: DLineSegment2 -> Picture
picLine ln = Picture $ \frm -> 
    let ln' = withinFrame frm ln
    in (drawLine ln', bounds ln')

picPath :: DPath -> Picture
picPath p = Picture $ \frm ->
   let p' = withinFrame frm p in (drawPath p', bounds p')

displace :: Double -> Double -> Picture -> Picture
displace x y p = Picture $ \frm -> (getPicture p) $ displaceOrigin (V2 x y) frm


vdisplace :: DVec2 -> Picture -> Picture
vdisplace v p = Picture $ \frm -> (getPicture p) $ displaceOrigin v frm



withRgbColour :: DRGB -> Picture -> Picture
withRgbColour c pic = Picture $ \frm ->
    let (mf,bb) = (getPicture pic) frm in (localRgbColour c mf,bb)

withGray :: Double -> Picture -> Picture
withGray n pic = Picture $ \frm ->
    let (mf,bb) = (getPicture pic) frm in (localGray n mf,bb)


withFont :: Font -> Picture -> Picture
withFont ft pic = Picture $ \frm ->
    let (mf,bb) = (getPicture pic) frm in (localFont ft mf,bb)




--------------------------------------------------------------------------------



helvetica :: Int -> Font
helvetica us = Font "Helvetica" us

timesRoman :: Int -> Font
timesRoman us = Font "Times-Roman" us


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


drawPath :: DPath -> WumpusM ()
drawPath path = strokeOpenPathSkel $ do 
    ps_moveto x y
    mapM_ draw1 $ unPath path
  where
    (P2 x y) = pathStart path
    draw1 (Left (LS _ (P2 x1 y1)))                            = ps_lineto x1 y1
    draw1 (Right (Curve _ (P2 x1 y1) (P2 x2 y2) (P2 x3 y3) )) = 
        ps_curveto x1 y1 x2 y2 x3 y3


diamond :: Double -> Double -> (DPoint2 -> DPolygon)
diamond w h = \o -> Polygon $ map (o .+^) xs 
  where
    xs = [vh,vv, reflectX vh, reflectY vv]
    vh = hvec (w/2)
    vv = vvec (h/2)


--------------------------------------------------------------------------------
-- arcs and ellipses

-- TODO - circle and disk should be dots (for dots drawing directly with 
-- PostScript's arc command will be efficient).
-- More general circles (that support translation) should then use Core.Curve.

type Radius = Double
type Origin = DPoint2



data Circle = Circle Origin Radius
  deriving (Eq,Show)



instance Pointwise Circle where
  type Pt Circle = DPoint2
  pointwise f (Circle o r) = Circle (f o) r



circle :: (Double,Double) -> Double -> Circle
circle (x,y) r  = Circle (P2 x y) r

drawCircle  :: Circle -> WumpusM ()
drawCircle (Circle (P2 x y) r) = strokePathSkel $ 
  ps_arc x y r 0 360 

data Disk = Disk Origin Radius
  deriving (Eq,Show)

instance Pointwise Disk where
  type Pt Disk = DPoint2
  pointwise f (Disk o r) = Disk (f o) r

diskBB :: Disk -> DBoundingBox
diskBB (Disk o r) = boundingBox $ Polygon [s,e,n,w] 
  where
    n = o .+^ vvec r
    s = o .+^ vvec (-r)
    e = o .+^ hvec r
    w = o .+^ hvec (-r)

circleBB :: Circle -> DBoundingBox
circleBB (Circle o r) = boundingBox $ Polygon [s,e,n,w] 
  where
    n = o .+^ vvec r
    s = o .+^ vvec (-r)
    e = o .+^ hvec r
    w = o .+^ hvec (-r)

   
disk :: (Double,Double) -> Double -> Disk
disk (x,y) r = Disk (P2 x y) r

drawDisk  :: Disk -> WumpusM ()
drawDisk (Disk (P2 x y) r) = fillPathSkel $ do
  ps_arc x y r 0 360


picDisk :: Disk -> Picture
picDisk p = Picture $ \frm -> 
  let p' = pointwise (coord frm) p in (drawDisk p', diskBB p')


picCircle :: Circle -> Picture
picCircle p = Picture $ \frm -> 
  let p' = pointwise (coord frm) p in (drawCircle p', circleBB p')



picCurve :: DCurve -> Picture
picCurve c = Picture $ \frm -> 
  let c' = pointwise (coord frm) c in (drawCurve c', bounds c')

drawCurve :: DCurve -> WumpusM ()
drawCurve (Curve (P2 x0 y0) (P2 x1 y1) (P2 x2 y2) (P2 x3 y3)) = 
  strokeOpenPathSkel $  do 
    ps_moveto x0 y0
    ps_curveto x1 y1 x2 y2 x3 y3


picBezier :: DCurve -> Picture
picBezier c = Picture $ \frm -> 
  let c' = pointwise (coord frm) c in (drawBezier c', bounds c')

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
 
