{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Alt.PointPicture
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  experimental
-- Portability :  GHC only
--
--
--------------------------------------------------------------------------------

module Wumpus.Alt.PointPicture where

import Wumpus.Alt.Geometry
import Wumpus.Drawing.PostScript

import Data.AffineSpace
import Data.VectorSpace

import Data.List ( foldl' )
import Data.Groupoid


data Picture v a = Empty
                 | Single v a
                 | Picture v (Picture v a) (Picture v a)
  deriving (Eq,Show) 


data BoundingBox a = BBox { blcorner :: Point2 a, trcorner :: Point2 a }
  deriving (Eq,Show)


type DBoundingBox = BoundingBox Double

-- Measure = (_relative_ displacement x bounding box)
type Measure a = (Vec2 a, BoundingBox a) 

type DMeasure = Measure Double

newtype Polygon a = Polygon { vertexList :: [Point2 a] }
  deriving (Eq,Show)

type DPolygon = Polygon Double



--------------------------------------------------------------------------------

unionBounds :: Ord a => BoundingBox a -> BoundingBox a -> BoundingBox a
unionBounds (BBox pmin pmax) (BBox pmin' pmax') = 
    BBox (umin pmin pmin') (umax pmax pmax')
  where
    umin (P2 x y) (P2 x' y') = P2 (min x x') (min y y')
    umax (P2 x y) (P2 x' y') = P2 (max x x') (max y y')


-- We don't consider BBox to have a zero, hence the Groupoid 
-- instance

instance Ord a => Groupoid (BoundingBox a) where
  gappend = unionBounds


--------------------------------------------------------------------------------

-- Trace the polygon finding the /extremity/...
bbPolygon :: (Num a, Ord a) => Polygon a -> BoundingBox a
bbPolygon (Polygon [])     = error $ "malformed Polygon - empty list"
bbPolygon (Polygon (p:ps)) = foldl' fn (BBox p p) ps
  where
    fn (BBox (P2 xmin ymin) (P2 xmax ymax)) (P2 x y) = 
        BBox (P2 (min xmin x) (min ymin y)) (P2 (max xmax x) (max ymax y))


type MPicture t a = Picture (Measure a) (t a)

picPolygon :: (Num a, Ord a) => Polygon a -> Picture (Measure a) (Polygon a)
picPolygon p = Single (zeroV,bbPolygon p) p


(<>) :: (Num a, Ord a) => MPicture t a -> MPicture t a -> MPicture t a
Empty  <> a      = a
a      <> Empty  = a
a      <> b      = Picture (zeroV,br) a b' 
                   where 
                     (P2 right _) = trcorner $ picBounds a
                     (P2 left _)  = blcorner $ picBounds b
                     b'           = move b (V2 (right-left) 0)
                     br           = unionBounds (picBounds a) (picBounds b')


overlay :: (Num a, Ord a) => MPicture t a -> MPicture t a -> MPicture t a
Empty `overlay` a     = a
a     `overlay` Empty = a
a     `overlay` b     = Picture (zeroV,br) a b
                        where br = unionBounds (picBounds a) (picBounds b)


picBounds :: (Num a, Ord a) => MPicture t a -> BoundingBox a
picBounds Empty                = error $ "picBounds Empty"
picBounds (Single (_,bb) _)    = bb
picBounds (Picture (_,bb) _ _) = bb

picDisplacement :: Num a => MPicture t a -> Vec2 a
picDisplacement Empty               = zeroV
picDisplacement (Single (v,_) _)    = v
picDisplacement (Picture (v,_) _ _) = v 


bbWidth :: Num a => BoundingBox a -> a
bbWidth (BBox (P2 xmin _) (P2 xmax _)) = xmax - xmin


move :: Num a => MPicture t a -> Vec2 a -> MPicture t a
move Empty                 _ = Empty
move (Single  (v0,br) p)   v = Single (v0 ^+^ v, pointwise (.+^ v) br) p
move (Picture (v0,br) a b) v = Picture (v0 ^+^ v, pointwise (.+^ v) br) a b
  

center :: (Fractional a, Ord a) => MPicture t a -> Point2 a
center Empty = zeroPt
center p     = fn $ picBounds p
  where
    fn (BBox bl tr) = bl .+^ (0.5 *^ (tr .-. bl))


transformPicture :: (Num a, Ord a) 
                 => (Point2 a -> Point2 a) 
                 -> MPicture Polygon a 
                 -> MPicture Polygon a
transformPicture _ Empty            = Empty
transformPicture f (Single (o,_) a) = Single (o,br') a'
                                      where a'  = pointwise f a
                                            br' = bbPolygon a'
transformPicture _ _                = error $ "composite Picture todo"

instance Pointwise (Polygon a) where
  type Pt (Polygon a) = Point2 a
  pointwise f (Polygon xs) = Polygon $ map f xs



instance Pointwise (BoundingBox a) where
  type Pt (BoundingBox a) = Point2 a
  pointwise f (BBox bl tr) = BBox (f bl) (f tr)


--------------------------------------------------------------------------------



 
writePicture :: FilePath -> MPicture Polygon Double -> IO ()
writePicture filepath pic = writeFile filepath $ psDraw pic


-- | Draw a picture, generating PostScript output.
psDraw :: MPicture Polygon Double -> PostScript
psDraw pic = prologue ++ runWumpus env0 (drawPicture zeroV pic) ++ epilogue
  where
    prologue = unlines $ [ "%!PS-Adobe-2.0"
                         , "%%Pages: 1"
                         , "%%EndComments"
                         , "%%Page: 1 1"
                         , ""
                         , "200 400 translate"
                         ]

                   
    epilogue = unlines $ [ "showpage", "", "%%EOF", ""]

drawPicture :: Vec2 Double -> MPicture Polygon Double -> WumpusM ()
drawPicture _ Empty                 = return ()
drawPicture v0 (Single (v1,_) p) = do
    drawPoly (v0 ^+^ v1) p
drawPicture v0 (Picture (v1,_) a b) = do
  drawPicture (v0 ^+^ v1) a
  drawPicture (v0 ^+^ v1) b


drawPoly :: Vec2 Double -> Polygon Double -> WumpusM ()
drawPoly _          (Polygon [])          = return ()
drawPoly (V2 vx vy) (Polygon (P2 x y:xs)) = do  
    ps_newpath
    ps_moveto (x+vx) (y+vy)
    mapM_ drawPt xs
    ps_closepath
    ps_stroke  
  where
    drawPt (P2 px py) = ps_lineto (px+vx) (py+vy)
