{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Alt.Picture
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  experimental
-- Portability :  GHC only
--
--
--------------------------------------------------------------------------------

module Wumpus.Alt.Picture where

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


data BoundingBox a = BBox { bottomLeft :: Point2 a, topRight :: Point2 a }
  deriving (Eq,Show)


type DBoundingBox = BoundingBox Double

-- Measure = (_current_ frame x bounding box)
type Measure a = (Frame2 a, BoundingBox a) 

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


-- We don't consider BBox to have a (nice) zero, hence 
-- the Groupoid instance

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
picPolygon p = Single (ortho zeroPt,bbPolygon p) p


(<>) :: (Num a, Ord a) => MPicture t a -> MPicture t a -> MPicture t a
Empty  <> a      = a
a      <> Empty  = a
a      <> b      = Picture (ortho zeroPt,br) a b' 
                   where 
                     (P2 right _) = topRight $ picBounds a
                     (P2 left _)  = bottomLeft $ picBounds b
                     b'           = move b (V2 (right-left) 0)
                     br           = unionBounds (picBounds a) (picBounds b')

(</>) :: (Num a, Ord a) => MPicture t a -> MPicture t a -> MPicture t a
Empty  </> a      = a
a      </> Empty  = a
a      </> b      = Picture (ortho zeroPt,br) a b' 
                    where 
                      (P2 _ bottom) = bottomLeft $ picBounds a
                      (P2 _ top)    = topRight $ picBounds b
                      b'            = move b (V2 0 (bottom-top))
                      br            = unionBounds (picBounds a) (picBounds b')



overlay :: (Num a, Ord a) => MPicture t a -> MPicture t a -> MPicture t a
Empty `overlay` a     = a
a     `overlay` Empty = a
a     `overlay` b     = Picture (ortho zeroPt,br) a b
                        where br = unionBounds (picBounds a) (picBounds b)


picBounds :: (Num a, Ord a) => MPicture t a -> BoundingBox a
picBounds Empty                = error $ "picBounds Empty"
picBounds (Single (_,bb) _)    = bb
picBounds (Picture (_,bb) _ _) = bb


bbWidth :: Num a => BoundingBox a -> a
bbWidth (BBox (P2 xmin _) (P2 xmax _)) = xmax - xmin

bbHeight :: Num a => BoundingBox a -> a
bbHeight (BBox (P2 _ ymin) (P2 _ ymax)) = ymax - ymin



move :: Num a => MPicture t a -> Vec2 a -> MPicture t a
move Empty                 _ = Empty
move (Single  (fr,br) p)   v = Single (displaceOrigin v fr, pointwise (.+^ v) br) p
move (Picture (fr,br) a b) v = Picture (displaceOrigin v fr, pointwise (.+^ v) br) a b
  

center :: (Fractional a, Ord a) => MPicture t a -> Point2 a
center Empty = zeroPt
center p     = fn $ picBounds p
  where
    fn (BBox bl tr) = bl .+^ (0.5 *^ (tr .-. bl))


{-
-- Actually this isn't so nice, it doesn't transform the picture
-- but its components, thus a rotation on a sequence of crosses
-- + + + will rotate each one x x x  
transformPicture :: (Num a, Ord a) 
                 => (Point2 a -> Point2 a) 
                 -> MPicture Polygon a 
                 -> MPicture Polygon a
transformPicture _ Empty                = Empty
transformPicture f (Single (fr,_) a)    = Single (fr,br') a' 
  where 
    a'  = pointwise f a
    br' = bbPolygon a'
transformPicture f (Picture (fr,_) a b) = Picture (fr,br') a' b'
  where 
    a'  = transformPicture f a
    b'  = transformPicture f b
    br' = picBounds a' `unionBounds` picBounds b'
-}


-- This is a rotation about the origin...
rotatePicture :: (Real a, Floating a)
              => Radian -> MPicture Polygon a -> MPicture Polygon a
rotatePicture _   Empty                 = Empty
rotatePicture ang (Single (fr,bb) a)    = 
    Single (rotateFrame ang fr, rotateBBox ang bb) a
rotatePicture ang (Picture (fr,bb) a b) =
    Picture (rotateFrame ang fr, rotateBBox ang bb) a b

rotateFrame :: (Real a, Floating a) => Radian -> Frame2 a -> Frame2 a
rotateFrame ang (Frame2 o vx vy) = Frame2 o (rotate ang vx) (rotate ang vy)    

rotateBBox :: (Real a, Floating a) 
             => Radian -> BoundingBox a -> BoundingBox a
rotateBBox ang (BBox bl@(P2 x0 y0) tr@(P2 x1 y1)) = 
    bbPolygon $ Polygon $ map (rotate ang) [bl, br, tr, tl]
  where
    br = P2 x1 y0
    tl = P2 x0 y1



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
psDraw pic = prologue ++ runWumpus env0 (drawPicture k pic) ++ epilogue
  where
    k        = pointInFrame `flip` (ortho zeroPt)
    prologue = unlines $ [ "%!PS-Adobe-2.0"
                         , "%%Pages: 1"
                         , "%%EndComments"
                         , "%%Page: 1 1"
                         , ""
                         , "200 400 translate"
                         ]

                   
    epilogue = unlines $ [ "showpage", "", "%%EOF", ""]

drawPicture :: (Point2 Double -> Point2 Double) 
            -> MPicture Polygon Double 
            -> WumpusM ()
drawPicture _ Empty                 = return ()
drawPicture f (Single (g,_) p) = do
    drawPoly (f . (pointInFrame `flip` g)) p
drawPicture f (Picture (g,_) a b) = do
    drawPicture (f . (pointInFrame `flip` g)) a
    drawPicture (f . (pointInFrame `flip` g)) b


drawPoly :: (Point2 Double -> Point2 Double) -> Polygon Double -> WumpusM ()
drawPoly _          (Polygon [])          = return ()
drawPoly f (Polygon (pt:pts)) = let P2 x y = f pt in do  
    ps_newpath
    ps_moveto x y
    mapM_ drawPt pts
    ps_closepath
    ps_stroke  
  where
    drawPt p = let P2 x y = f p in ps_lineto x y
