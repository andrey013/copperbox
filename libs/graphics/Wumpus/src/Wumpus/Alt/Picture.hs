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
import Data.Groupoid

import Data.AffineSpace
import Data.VectorSpace

import Control.Monad ( zipWithM_ )
import Data.List ( foldl' )

data Picture u = Empty
               | Single   (Measure u) (Path u)
               | Multi    (Measure u) [Path u]
               | TLabel   (Measure u) (Label u)
               | Picture  (Measure u) (Picture u) (Picture u)
  deriving (Eq,Show) 


-- Measure = (_current_ frame x bounding box)
type Measure u = (Frame2 u, BoundingBox u) 

type DMeasure = Measure Double

data DrawProp = OStroke | CStroke | CFill | CCrop 
  deriving (Eq,Show)


data PathSeg u = Cs (Point2 u) (Point2 u) (Point2 u)
               | Ls (Point2 u)
  deriving (Eq,Show)

type DPathSeg = PathSeg Double

data Path u = Path DrawProp (Point2 u) [PathSeg u]
  deriving (Eq,Show)

type DPath = Path Double


newtype Polygon u = Polygon { vertexList :: [Point2 u] }
  deriving (Eq,Show)

type DPolygon = Polygon Double

data Label u = Label {
      labelFontSize :: u,
      labelRowDisp  :: u,
      labelText     :: [String] 
    }
  deriving (Eq,Show)


--------------------------------------------------------------------------------

instance Groupoid (Path u) where
  Path dp st xs `gappend` Path _ st' xs' = Path dp st (xs ++ (Ls st' : xs'))


-- | Create a regular polygon with @n@ sides and /radius/ @r@ 
-- centered at the origin.
regularPolygon :: (Floating u, Real u)
               => Int -> u -> Polygon u
regularPolygon n r = Polygon $ circular $ replicate n (zeroPt .+^ (V2 0 r)) 


-- | Create an isosceles rectangle with bottom-left corner at the 
-- origin, the base on the horizontal plane with width @bw@. The 
-- height is @h@.
isoscelesTriangle :: Fractional u => u -> u -> Polygon u
isoscelesTriangle bw h = Polygon $ sequence [id,f2,f3] zeroPt where
  f2 = (.+^ hvec bw)
  f3 = (.+^ V2 (bw/2) h)



straightLinePath :: [Point2 u] -> (Point2 u, [PathSeg u])
straightLinePath []     = error "polygonPath - empty Polygon"
straightLinePath (x:xs) = (x, map Ls xs)

  
picPolygon :: (Num u, Ord u) => DrawProp -> Polygon u -> Picture u
picPolygon dp (Polygon xs) = Single (ortho zeroPt,trace xs) path
  where 
    path         = Path dp start segs
    (start,segs) = straightLinePath xs


picPath :: (Num u, Ord u) => Path u -> Picture u
picPath p = Single (ortho zeroPt, tracePath p) p

picMultiPath :: (Num u, Ord u) => [Path u] -> Picture u
picMultiPath ps = Multi (ortho zeroPt, gconcat (map tracePath ps)) ps


picLabel :: (Num u, Ord u) => u -> u -> u -> u -> String -> Picture u
picLabel fonth vdisp w h text = TLabel (ortho zeroPt, bb) label where
  bb    = trace [zeroPt, P2 w h]
  label = Label fonth vdisp (lines text) 


tracePath :: (Num u, Ord u) => Path u -> BoundingBox u
tracePath = trace . extractPoints

extractPoints :: Path u -> [Point2 u]
extractPoints (Path _ st xs) = st : foldr f [] xs where
    f (Ls p)        acc = p : acc
    f (Cs p1 p2 p3) acc = p1 : p2 : p3 : acc 

extractPolygonPath :: Polygon u -> Path u
extractPolygonPath p = Path CStroke p0 ps where
  (p0,ps) = straightLinePath $ vertexList p 



bbPolygon :: (Num u, Ord u) => Polygon u -> BoundingBox u
bbPolygon (Polygon xs)     = trace xs 

arrange :: (Num u, Ord u)
        => (Picture u -> Picture u -> Vec2 u) 
        -> Picture u
        -> Picture u 
        -> Picture u
arrange _ a     Empty = a
arrange _ Empty b     = b
arrange f a     b     = Picture (ortho zeroPt, bb) a b' where
    b' = move (f a b) b
    bb = union (picBounds a) (picBounds b')
   
   

(<>) :: (Num u, Ord u) => Picture u -> Picture u -> Picture u
(<>) = arrange $ twine fn (rightPlane . picBounds) (leftPlane . picBounds)
  where fn = hvec `oo` (-) 


(</>) :: (Num u, Ord u) => Picture u -> Picture u -> Picture u
(</>) = arrange $ twine fn (lowerPlane . picBounds) (upperPlane . picBounds)
  where fn = vvec `oo` (-)

overlay :: (Num u, Ord u) => Picture u -> Picture u -> Picture u
overlay = arrange (\_ _ -> V2 0 0)

overlays :: (Num u, Ord u) => [Picture u] -> Picture u
overlays []     = error "overlays - empty list"
overlays (x:xs) = foldl' overlay x xs

picBounds :: (Num u, Ord u) => Picture u -> BoundingBox u
picBounds Empty                = error $ "picBounds Empty"
picBounds (Single  (_,bb) _)   = bb
picBounds (Multi   (_,bb) _)   = bb
picBounds (TLabel  (_,bb) _)   = bb
picBounds (Picture (_,bb) _ _) = bb


mapMeasure :: (Measure u -> Measure u) -> Picture u -> Picture u
mapMeasure _ Empty            = Empty
mapMeasure f (Single  m p)    = Single (f m) p
mapMeasure f (Multi   m ps)   = Multi (f m) ps
mapMeasure f (TLabel  m l)    = TLabel (f m) l 
mapMeasure f (Picture m a  b) = Picture (f m) a b


move :: Num u => Vec2 u -> Picture u -> Picture u
move v = mapMeasure (moveMeasure v) 

  
moveMeasure :: Num u => Vec2 u -> Measure u -> Measure u
moveMeasure v (fr,bb) = (displaceOrigin v fr, pointwise (.+^ v) bb) 

center :: (Fractional u, Ord u) => Picture u -> Point2 u
center Empty = zeroPt
center p     = fn $ picBounds p where
    fn (BBox bl tr) = bl .+^ (0.5 *^ (tr .-. bl))


-- This is a rotation about the origin...
rotatePicture :: (Real u, Floating u) => Radian -> Picture u -> Picture u
rotatePicture ang = mapMeasure $ prod (rotateFrame ang) (rotateBBox ang)


rotateFrame :: (Real u, Floating u) => Radian -> Frame2 u -> Frame2 u
rotateFrame ang (Frame2 o vx vy) = Frame2 o (rotate ang vx) (rotate ang vy)    

rotateBBox :: (Real u, Floating u) 
           => Radian -> BoundingBox u -> BoundingBox u
rotateBBox ang (BBox bl@(P2 x0 y0) tr@(P2 x1 y1)) = 
    trace $ map (rotate ang) [bl, br, tr, tl]
  where
    br = P2 x1 y0
    tl = P2 x0 y1



instance Pointwise (Polygon a) where
  type Pt (Polygon a) = Point2 a
  pointwise f (Polygon xs) = Polygon $ map f xs

instance Pointwise (Path u) where
  type Pt (Path u) = Point2 u
  pointwise f (Path dp st xs) = Path dp (f st) (map (pointwise f) xs)

instance Pointwise (PathSeg u) where
  type Pt (PathSeg u) = Point2 u
  pointwise f (Ls p)        = Ls (f p)
  pointwise f (Cs p1 p2 p3) = Cs (f p1) (f p2) (f p3)

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
                         --- these are temporary...
                         , "/Times-Roman findfont"
                         , "10 scalefont"
                         , "setfont"
                         , "200 400 translate"
                         ]

                   
    epilogue = unlines $ [ "showpage", "", "%%EOF", ""]

drawPicture :: (Point2 Double -> Point2 Double) 
            -> Picture Double 
            -> WumpusM ()
drawPicture _ Empty                = return ()
drawPicture f (Single (fr,_) p)    = drawPath (composeFrames f fr) p
drawPicture f (Multi (fr,_) ps)    = 
    mapM_ (drawPath (composeFrames f fr)) ps
drawPicture f (TLabel (fr,bb) l)   = 
    drawLabel (composeFrames f fr) (bottomLeft bb) (height bb) l
drawPicture f (Picture (fr,_) a b) = do
    drawPicture (composeFrames f fr) a
    drawPicture (composeFrames f fr) b


composeFrames :: Num u 
              => (Point2 u -> Point2 u) -> Frame2 u -> (Point2 u -> Point2 u)
composeFrames f fr = f . (pointInFrame `flip` fr)


drawPath :: (Point2 Double -> Point2 Double) -> Path Double -> WumpusM ()
drawPath f (Path dp pt xs) = let P2 x y = f pt in do  
    ps_newpath
    ps_moveto x y
    mapM_ drawLineSeg xs
    finalize dp   
  where
    drawLineSeg (Ls p)        = let P2 x y = f p in ps_lineto x y
    drawLineSeg (Cs p1 p2 p3) = let P2 x1 y1 = f p1
                                    P2 x2 y2 = f p2
                                    P2 x3 y3 = f p3
                                in ps_curveto x1 y1 x2 y2 x3 y3
    finalize OStroke = ps_stroke
    finalize CStroke = ps_closepath >> ps_stroke
    finalize CFill   = ps_closepath >> ps_fill
    finalize CCrop   = ps_closepath >> ps_clip


drawLabel :: (Point2 Double -> Point2 Double) 
          -> Point2 Double 
          -> Double 
          -> Label Double 
          -> WumpusM ()
drawLabel fn (P2 x y) totalh (Label fonth rowdisp xs) = do
    ps_moveto x y
    ps_gsave
    ps_concat a b c d e f
    zipWithM_ drawTextLine xs vecs
    ps_grestore
  where
    drawTextLine str (V2 vx vy) = ps_moveto (x+vx) (y+vy) >> ps_show str
    vecs          = labelDisplacements fonth totalh rowdisp (length xs) 
    (a,b,c,d,e,f) = makeCTM fn

makeCTM :: Num u => (Point2 u -> Point2 u) -> (u,u,u,u,u,u)
makeCTM f = (x0-o0, x1-o1, y0-o0, y1-o1, o0, o1) where
   P2 x0 x1 = f (P2 1 0)
   P2 y0 y1 = f (P2 0 1)
   P2 o0 o1 = f (P2 0 0)


labelDisplacements :: Fractional u => u -> u -> u -> Int -> [Vec2 u]
labelDisplacements fonth totalh rowdisp nrows = 
    reverse $ take nrows $ iterate (^+^ vvec (fonth+rowdisp)) (vvec start)
  where
    n         = fromIntegral nrows
    internalh = fonth*n + rowdisp*(n-1)
    start     = 0.5 * (totalh-internalh)