{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Drawing.Picture
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Picture type
--
--------------------------------------------------------------------------------


module Wumpus.Drawing.Picture where

import Wumpus.Core.BoundingBox
import Wumpus.Core.Geometric
import Wumpus.Core.Point
import Wumpus.Core.Polygon
import Wumpus.Core.Vector

import Wumpus.Drawing.Path
import Wumpus.Drawing.PostScript

import Data.AffineSpace

import qualified Data.Foldable as F
import Data.Monoid


-- Reminder - there is some merit in changing the representation
-- in Core.Polygon of a Polygon to be a list of vectors rather 
-- than points.


data Picture = Picture { bbox :: DBoundingBox, paths :: [DPath] }
  deriving (Show)

instance Monoid Picture where
  mempty = Picture mempty []
  Picture bb ps `mappend` Picture bb' ps' = Picture (bb `mappend` bb')
                                                    (ps ++ ps') 


extractCoordinate :: (DBoundingBox -> DPoint2) -> Picture -> DPoint2
extractCoordinate f = f . bbox


infixr 7 <..>
infixr 6 <++>
infixr 5 <//>

(<..>) :: Picture -> Picture -> Picture
(<..>) = mappend

(<++>) :: Picture -> Picture -> Picture
(<++>) p1 p2 = let v = (east $ bbox p1) .-. (west $ bbox p2) 
               in p1 `mappend` (displacePicture v p2)  


(<//>) :: Picture -> Picture -> Picture
(<//>) p1 p2 = let v = (south $ bbox p1) .-. (north $ bbox p2) 
               in p1 `mappend` (displacePicture v p2)  


displacePicture :: DVec2 -> Picture -> Picture
displacePicture v (Picture (BBox bl tr) ps) = 
    Picture (BBox (bl .+^ v) (tr .+^ v)) (map (displacePath v) ps)

at :: Picture -> (Double,Double) -> Picture
at p (x,y) = displacePicture (V2 x y) p


picPolygon :: DPolygon -> Picture
picPolygon pgon = Picture (getBoundingBox pgon) [path]
  where
    path = closePath . tracePoints . extractPoints $ pgon

picPath :: DPath -> Picture
picPath p = Picture (bounds p)  [p]


polyDot :: Int -> Picture 
polyDot n = picPolygon $ regularPolygon n 2 zeroPt


dotTriangle :: Picture
dotTriangle = polyDot 3

dotDiamond :: Picture
dotDiamond = polyDot 4



writePicture :: FilePath -> Picture -> IO ()
writePicture filepath pic = writeFile filepath $ psDraw pic
 
-- | Draw a picture, generating PostScript output.
psDraw :: Picture -> PostScript
psDraw pic = runWumpus env0 (drawPicture pic)


drawPicture :: Picture -> WumpusM ()
drawPicture (Picture _ ps) = mapM_ drawPath ps 

drawPath :: DPath -> WumpusM ()
drawPath (Path p0@(P2 x0 y0) end sp) = do 
    ps_newpath
    ps_moveto x0 y0
    F.foldlM step p0 sp 
    endPath end
  where
    endPath (PathClosed _) = ps_closepath >> ps_stroke
    endPath _              = ps_stroke
    
    step p (RMoveTo v)         = let p'@(P2 x y) = p .+^ v
                                 in ps_moveto x y >> return p'
    step p (RLineTo v)         = let p'@(P2 x y) = p .+^ v
                                 in ps_lineto x y >> return p'

    step p (RCurveTo v1 v2 v3) = let p1@(P2 x1 y1) = p  .+^ v1
                                     p2@(P2 x2 y2) = p1 .+^ v2
                                     p3@(P2 x3 y3) = p2 .+^ v3
                                 in ps_curveto x1 y1 x2 y2 x3 y3 >> return p3

