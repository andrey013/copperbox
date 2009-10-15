{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# OPTIONS -Wall #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Alt.Picture
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  experimental
-- Portability :  GHC only
--
--
-----------------------------------------------------------------------------

module Wumpus.Alt.Picture where

import Wumpus.Drawing.PostScript

import Data.VectorSpace

import Data.List ( foldl' )
import Data.Monoid


data Picture v a = Empty
                 | Single v a
                 | Picture v (Picture v a) (Picture v a)
  deriving (Eq,Show) 

data Vec2 a = V2 !a !a
  deriving (Eq,Show)

type DVec2 = Vec2 Double

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

unionBRect :: Ord a => BoundingRect a -> BoundingRect a -> BoundingRect a
BRect w h `unionBRect` BRect w' h' = BRect (max w w') (max h h')

-- Remember a bounding rectangle exists in a coordinate free 
-- world modeling /width/ and /height/ not alternate corners. 
-- Thus is can have a simple zero instance (rather than a 
-- complicated one with positive and negative infinities).

instance (Ord a, Num a) => Monoid (BoundingRect a) where
  mempty = BRect 0 0 
  mappend = unionBRect

-- Vectors have a sensible Monoid instance as addition

instance Num a => Monoid (Vec2 a) where
  mempty = V2 0 0
  V2 a b `mappend` V2 x y = V2 (a+x) (b+y) 


--------------------------------------------------------------------------------
-- Vector space and related instances

instance Num a => AdditiveGroup (Vec2 a) where
  zeroV = V2 0 0 
  V2 a b ^+^ V2 x y = V2 (a+x) (b+y) 
  negateV (V2 a b) = V2 (-a) (-b) 


instance Num a => VectorSpace (Vec2 a) where
  type Scalar (Vec2 a) = a
  s *^ (V2 a b) = V2 (s*a) (s*b)


-- scalar (dot / inner) product via the class InnerSpace

instance (Num a, InnerSpace a, Scalar a ~ a) 
    => InnerSpace (Vec2 a) where
  (V2 a b) <.> (V2 a' b') = (a <.> a') ^+^ (b <.> b')


--------------------------------------------------------------------------------

-- Trace the polygon finding the /extremity/...
bbPolygon :: (Num a, Ord a) => Polygon a -> BoundingRect a
bbPolygon (Polygon vs) = post $ foldl' fn ((0,0), V2 0 0) vs
  where
    post ((xmax,ymax),_) = BRect xmax ymax
    fn ((x0,y0), vc) v = ((max x0 x,max y0 y), vc') 
                             where vc'@(V2 x y) = vc ^+^ v

type MPicture t a = Picture (Measure a) (t a)

picPolygon :: (Num a, Ord a) => Polygon a -> Picture (Measure a) (Polygon a)
picPolygon p = Single (zeroV,bbPolygon p) p


(<>) :: (Num a, Ord a) => MPicture t a -> MPicture t a -> MPicture t a
Empty  <> a      = a
a      <> Empty  = a
a      <> b      = Picture (zeroV,br) a (move b v) 
                   where br0 = picBounds a 
                         v   = V2 (brw br0) 0
                         br  = brNextTo br0 (picBounds b)


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
    mapM_ (\(V2 a b) -> ps_rlineto a b) xs
    ps_closepath
    ps_stroke  

