{-# OPTIONS -Wall #-}

module QuadTreeDraw where

import Wumpus.Core.Colour
import Wumpus.Core.Fun
import Wumpus.Core.Geometric 
import Wumpus.Core.Line
import Wumpus.Core.Matrix
import Wumpus.Core.Point

import Wumpus.Drawing.QuadTree
import Wumpus.Drawing.Basic

import qualified Data.Foldable as F


demo1 :: IO ()
demo1 = writePicture "quadtree1.ps" (drawQuadTree demo_qt) where


drawQuadTree :: QuadTree (DPoint2,DPoint2) DPoint2 -> Picture
drawQuadTree tree = drawing1 where
  drawing1 = displace 20 20 (picLines (treeLines tree) <..> points)
  points = withRgbColour wumpusRed $ 
             cat $ map (\(P2 x y) -> displace x y dotPlus) $ treePoints tree




-- Note a square of 400x400 PS points will fit on an A4 page


sqlines :: (DPoint2,DPoint2) -> [DLineSegment2]
sqlines (P2 x0 y0, P2 x1 y1) = [t,r,b,l]  -- clockwise
  where
    t = lineTo (P2 x0 y1) (P2 x1 y1)
    r = lineTo (P2 x1 y1) (P2 x1 y0)
    b = lineTo (P2 x1 y0) (P2 x0 y0)
    l = lineTo (P2 x0 y0) (P2 x0 y1)

midlines :: (DPoint2,DPoint2) -> [DLineSegment2]
midlines (p1@(P2 x0 y0), p2@(P2 x1 y1)) = [v,h]  -- clockwise
  where
    P2 x' y' = midpoint p1 p2
    v        = lineTo (P2 x' y0) (P2 x' y1)
    h        = lineTo (P2 x0 y') (P2 x1 y')


treeLines :: QuadTree (DPoint2,DPoint2) DPoint2 -> [DLineSegment2]
treeLines (Empty _sq)             = [] -- sqlines sq
treeLines (Leaf _sq _)            = [] -- sqlines sq
treeLines (Quad _ sq nw ne se sw) =
    sqlines sq' ++ midlines sq' ++ (concat $ map treeLines [nw,ne,se,sw])
  where
    sq' = scaleSq sq


treePoints :: QuadTree (DPoint2,DPoint2) DPoint2 -> [DPoint2]
treePoints = F.foldr fn [] where
  fn a = (:) (scalePt a)


scaleSq :: (DPoint2,DPoint2) -> (DPoint2,DPoint2)
scaleSq = prod scalePt scalePt

scalePt :: DPoint2 -> DPoint2 
scalePt = (tM *#)
  where
   tM = scalingMatrix 40 40  

transPt :: DPoint2 -> DPoint2 
transPt =  (tM *#)
  where
   tM = translationMatrix 40 40  


-----

demo_qt :: QuadTree (DPoint2,DPoint2) DPoint2
demo_qt = build (P2 0 0, P2 10 10) [P2 1 5, P2 7 4, P2 9 9, P2 8.5 8.5]

demo_qt' :: QuadTree (DPoint2,DPoint2) DPoint2
demo_qt' = build (P2 0 0, P2 10 10) [P2 8.5 8.5, P2 9 9]

