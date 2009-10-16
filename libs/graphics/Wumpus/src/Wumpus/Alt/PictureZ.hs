{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Alt.PictureZ
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  experimental
-- Portability :  GHC only
--
--
--------------------------------------------------------------------------------

module Wumpus.Alt.PictureZ where

import Wumpus.Alt.Geometry
import Wumpus.Drawing.PostScript

import Data.VectorSpace

import Data.List ( foldl' )
import Data.Monoid


data Picture v a = Empty
                 | Single v a
                 | Picture v (Picture v a) (Picture v a)
  deriving (Eq,Show) 


data BoundingRect a = BRect { brw :: a, brh :: a }
  deriving (Eq,Show)


type DBoundingRect = BoundingRect Double

-- Measure = (_relative_ displacement x bounding rect)
type Measure a = (Vec2 a, BoundingRect a) 

type DMeasure = Measure Double

newtype Polygon a = Polygon { vertexList :: [Vec2 a] }
  deriving (Eq,Show)

type DPolygon = Polygon Double


--------------------------------------------------------------------------------

unionBounds :: Ord a => BoundingRect a -> BoundingRect a -> BoundingRect a
BRect w h `unionBounds` BRect w' h' = BRect (max w w') (max h h')

-- Remember a bounding rectangle exists in a coordinate free 
-- world modeling /width/ and /height/ not alternate corners. 
-- Thus is can have a simple zero instance (rather than a 
-- complicated one with positive and negative infinities).

instance (Ord a, Num a) => Monoid (BoundingRect a) where
  mempty = BRect 0 0 
  mappend = unionBounds

-- No functor instance for Picture - applying a function to the 
-- /polygon/ may change them measure. E.g. a scale would also
-- change the bounding box

--------------------------------------------------------------------------------


-- 
brPolygon :: (Num a, Ord a) => Polygon a -> BoundingRect a
brPolygon (Polygon [])            = BRect 0 0
brPolygon (Polygon (V2 x0 y0:vs)) = post $ foldl' fn ((x0,x0),(y0,y0)) vs
  where
    post ((xmin,xmax),(ymin,ymax)) = BRect (xmax-xmin) (ymax-ymin)
    fn ((xmin,xmax),(ymin,ymax)) (V2 x y) = 
        ((min xmin x,max xmax x),(min ymin y,max ymax y))

type MPicture t a = Picture (Measure a) (t a)

picPolygon :: (Num a, Ord a) => Polygon a -> Picture (Measure a) (Polygon a)
picPolygon p = Single (zeroV,brPolygon p) p


(<>) :: (Num a, Ord a) => MPicture t a -> MPicture t a -> MPicture t a
Empty  <> b      = b
a      <> Empty  = a
a      <> b      = Picture (zeroV,br) a (move b v) 
                   where br0 = picBounds a 
                         v   = V2 (brw br0) 0
                         br  = brNextTo br0 (picBounds b)

overlay :: (Num a, Ord a) => MPicture t a -> MPicture t a -> MPicture t a
Empty `overlay` a     = a
a     `overlay` Empty = a
a     `overlay` b     = Picture (zeroV,br) a b
                        where br = unionBounds (picBounds a) (picBounds b)

picBounds :: (Num a, Ord a) => MPicture t a -> BoundingRect a
picBounds Empty                = mempty
picBounds (Single (_,br) _)    = br
picBounds (Picture (_,br) _ _) = br

picDisplacement :: Num a => MPicture t a -> Vec2 a
picDisplacement Empty               = mempty
picDisplacement (Single (v,_) _)    = v
picDisplacement (Picture (v,_) _ _) = v 

brNextTo :: (Num a, Ord a) => BoundingRect a -> BoundingRect a -> BoundingRect a
brNextTo a b = BRect (brw a + brw b) (max (brh a) (brh b))

move :: Num a => MPicture t a -> Vec2 a -> MPicture t a
move Empty                 _ = Empty
move (Single  (v0,br) p)   v = Single (v0 ^+^ v,br) p
move (Picture (v0,br) a b) v = Picture (v0 ^+^ v,br) a b


center :: (Fractional a, Ord a) => MPicture t a -> Vec2 a
center = fn . picBounds
  where
    fn (BRect w h) = V2 (w/2) (h/2)


transformPicture :: (Num a, Ord a) 
                 => (Vec2 a -> Vec2 a) 
                 -> MPicture Polygon a 
                 -> MPicture Polygon a
transformPicture _ Empty              = Empty
transformPicture f (Single (o,br) a)  = Single (o,br') a'
                                        where a'  = pointwise f a
                                              br' = brPolygon a'
transformPicture _ _                  = error $ "composite Picture todo"

instance Pointwise (Polygon a) where
  type Pt (Polygon a) = Vec2 a
  pointwise f (Polygon xs) = Polygon $ map f xs

--------------------------------------------------------------------------------

 
writePicture :: FilePath -> MPicture Polygon Double -> IO ()
writePicture filepath pic = writeFile filepath $ psDraw pic


-- | Draw a picture, generating PostScript output.
psDraw :: MPicture Polygon Double -> PostScript
psDraw pic = prologue ++ runWumpus env0 (drawPicture (0,0) pic) ++ epilogue
  where
    prologue = unlines $ [ "%!PS-Adobe-2.0"
                         , "%%Pages: 1"
                         , "%%EndComments"
                         , "%%Page: 1 1"
                         , ""
                         , "200 200 translate"
                         ]

                   
    epilogue = unlines $ [ "showpage", "", "%%EOF", ""]

drawPicture :: (Double,Double) -> MPicture Polygon Double -> WumpusM ()
drawPicture _ Empty                 = return ()
drawPicture (x,y) (Single (V2 dx dy,_) p) = do
    drawPoly (x+dx,y+dy) p
drawPicture (x,y) (Picture (V2 dx dy,_) a b) = do
  drawPicture (x+dx,y+dy) a
  drawPicture (x+dx,y+dy) b


drawPoly :: (Double,Double) -> Polygon Double -> WumpusM ()
drawPoly (x,y) (Polygon xs) = do 
    ps_newpath
    ps_moveto x y
    mapM_ (\(V2 a b) -> ps_lineto (x+a) (y+b)) xs
    ps_closepath
    ps_stroke  

