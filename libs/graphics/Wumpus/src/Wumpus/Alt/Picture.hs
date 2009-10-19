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

import Wumpus.Alt.BoundingBox hiding ( center )
import Wumpus.Alt.Geometry
import Wumpus.Drawing.PostScript

import Data.AffineSpace
import Data.VectorSpace


data Picture u = Empty
               | Single (Measure u) (Polygon u)
               | Picture (Measure u) (Picture u) (Picture u)
  deriving (Eq,Show) 


-- Measure = (_current_ frame x bounding box)
type Measure u = (Frame2 u, BoundingBox u) 

type DMeasure = Measure Double

newtype Polygon u = Polygon { vertexList :: [Point2 u] }
  deriving (Eq,Show)

type DPolygon = Polygon Double


--------------------------------------------------------------------------------


picPolygon :: (Num a, Ord a) => Polygon a -> Picture a
picPolygon p = Single (ortho zeroPt,bbPolygon p) p

bbPolygon :: (Num a, Ord a) => Polygon a -> BoundingBox a
bbPolygon (Polygon xs)     = trace xs 

arrange :: (Num a, Ord a)
        => (Picture a -> Picture a -> Vec2 a) 
        -> Picture a 
        -> Picture a 
        -> Picture a
arrange _ a     Empty = a
arrange _ Empty b     = b
arrange f a     b     = Picture (ortho zeroPt, bb) a b' where
  v  = f a b
  b' = move b v
  bb = union (picBounds a) (picBounds b')
   

(<>) :: (Num a, Ord a) => Picture a -> Picture a -> Picture a
(<>) = arrange (\a b -> 
    let x = (rightPlane $ picBounds a) - (leftPlane $ picBounds b) in V2 x 0)
   


(</>) :: (Num a, Ord a) => Picture a -> Picture a -> Picture a
(</>) = arrange (\a b -> 
    let y = (lowerPlane $ picBounds a) - (upperPlane $ picBounds b) in V2 0 y)

overlay :: (Num a, Ord a) => Picture a -> Picture a -> Picture a
overlay = arrange (\_ _ -> V2 0 0)


picBounds :: (Num a, Ord a) => Picture a -> BoundingBox a
picBounds Empty                = error $ "picBounds Empty"
picBounds (Single (_,bb) _)    = bb
picBounds (Picture (_,bb) _ _) = bb




move :: Num a => Picture a -> Vec2 a -> Picture a
move Empty                 _ = Empty
move (Single  (fr,bb) p)   v = Single (displaceOrigin v fr, pointwise (.+^ v) bb) p
move (Picture (fr,bb) a b) v = Picture (displaceOrigin v fr, pointwise (.+^ v) bb) a b
  

center :: (Fractional a, Ord a) => Picture a -> Point2 a
center Empty = zeroPt
center p     = fn $ picBounds p
  where
    fn (BBox bl tr) = bl .+^ (0.5 *^ (tr .-. bl))


-- This is a rotation about the origin...
rotatePicture :: (Real a, Floating a) => Radian -> Picture a -> Picture a
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


--------------------------------------------------------------------------------


 
writePicture :: FilePath -> Picture Double -> IO ()
writePicture filepath pic = writeFile filepath $ psDraw pic


-- | Draw a picture, generating PostScript output.
psDraw :: Picture Double -> PostScript
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
            -> Picture Double 
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
