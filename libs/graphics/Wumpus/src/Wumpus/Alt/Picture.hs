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

import Data.FunctionExtras
import Data.AffineSpace
import Data.VectorSpace



data Picture u = Empty
               | Single (Measure u) (Path u)
               | Picture (Measure u) (Picture u) (Picture u)
  deriving (Eq,Show) 


-- Measure = (_current_ frame x bounding box)
type Measure u = (Frame2 u, BoundingBox u) 

type DMeasure = Measure Double

data DrawProp = Fill | Stroke | Crop 
  deriving (Eq,Show)


data PathSeg u = Cs (Point2 u) (Point2 u) (Point2 u)
               | Ls (Point2 u)
  deriving (Eq,Show)

data Path u = Path DrawProp (Point2 u) [PathSeg u]
  deriving (Eq,Show)

newtype Polygon u = Polygon { vertexList :: [Point2 u] }
  deriving (Eq,Show)

type DPolygon = Polygon Double


--------------------------------------------------------------------------------

-- | Create a regular polygon with @n@ sides and /radius/ @r@ 
-- centered at the origin.
regularPolygon :: (Floating u, Real u)
               => Int -> u -> Polygon u
regularPolygon n r = Polygon $ circular $ replicate n (zeroPt .+^ (V2 0 r)) 


straightLinePath :: [Point2 u] -> (Point2 u, [PathSeg u])
straightLinePath []     = error "polygonPath - empty Polygon"
straightLinePath (x:xs) = (x, map Ls xs)
  
picPolygon :: (Num a, Ord a) => DrawProp -> Polygon a -> Picture a
picPolygon dp (Polygon xs) = Single (ortho zeroPt,trace xs) path
  where 
    path         = Path dp start segs
    (start,segs) = straightLinePath xs

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
    b' = move b (f a b)
    bb = union (picBounds a) (picBounds b')
   
   

(<>) :: (Num a, Ord a) => Picture a -> Picture a -> Picture a
(<>) = arrange $ twine fn (rightPlane . picBounds) (leftPlane . picBounds)
  where fn = hvec `oo` (-) 


(</>) :: (Num a, Ord a) => Picture a -> Picture a -> Picture a
(</>) = arrange $ twine fn (lowerPlane . picBounds) (upperPlane . picBounds)
  where fn = vvec `oo` (-)

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
center p     = fn $ picBounds p where
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
    trace $ map (rotate ang) [bl, br, tr, tl]
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
drawPicture _ Empty               = return ()
drawPicture f (Single (g,_) p)    = drawPath (f . (pointInFrame `flip` g)) p
drawPicture f (Picture (g,_) a b) = do
    drawPicture (f . (pointInFrame `flip` g)) a
    drawPicture (f . (pointInFrame `flip` g)) b


drawPath :: (Point2 Double -> Point2 Double) -> Path Double -> WumpusM ()
drawPath f (Path dp pt xs) = let P2 x y = f pt in do  
    ps_newpath
    ps_moveto x y
    mapM_ drawLineSeg xs
    ps_closepath
    finalize dp   
  where
    drawLineSeg (Ls p)        = let P2 x y = f p in ps_lineto x y
    drawLineSeg (Cs p1 p2 p3) = let P2 x1 y1 = f p1
                                    P2 x2 y2 = f p2
                                    P2 x3 y3 = f p2
                                in ps_curveto x1 y1 x2 y2 x3 y3
    finalize Stroke = ps_stroke
    finalize Fill   = ps_fill
    finalize Crop   = ps_clip
