

module QuadTreeDraw where

import Wumpus.Core.Instances
import Wumpus.Core.Matrix
import Wumpus.Core.Point
import Wumpus.Core.Wumpus hiding ( concat )
import Wumpus.Drawing.QuadTree
import Wumpus.Drawing.Basic

import qualified Data.Foldable as F

-- Note a square of 400x400 PS points will fit on an A4 page


sqlines :: (DPoint2,DPoint2) -> [Line]
sqlines (P2 x0 y0, P2 x1 y1) = [t,r,b,l]  -- clockwise
  where
    t = Line (P2 x0 y1) (P2 x1 y1)
    r = Line (P2 x1 y1) (P2 x1 y0)
    b = Line (P2 x1 y0) (P2 x0 y0)
    l = Line (P2 x0 y0) (P2 x0 y1)

midlines :: (DPoint2,DPoint2) -> [Line]
midlines (p1@(P2 x0 y0), p2@(P2 x1 y1)) = [v,h]  -- clockwise
  where
    P2 x' y' = midpoint p1 p2
    v        = Line (P2 x' y0) (P2 x' y1)
    h        = Line (P2 x0 y') (P2 x1 y')

-- drawIt sq = mapM_ drawLine $ sqlines sq


demo1 :: IO ()
demo1 = outputQuadTree "tree1.ps" demo_qt

outputQuadTree :: FilePath -> QuadTree (DPoint2,DPoint2) DPoint2 -> IO ()
outputQuadTree name tree =  writePS name $ runWumpus st0 $ drawing1 where
  drawing1 = do { translate 20 20
                ; mapM_ drawLine $ treelines tree
                ; setrgbcolor 1 0 0
                ; mapM_ drawPoint $ treepoints tree
                }

treelines :: QuadTree (DPoint2,DPoint2) DPoint2 -> [Line]
treelines (Empty sq)  = [] -- sqlines sq
treelines (Leaf sq _) = [] -- sqlines sq
treelines (Quad _ sq nw ne se sw) =
    sqlines sq' ++ midlines sq' ++ (concat $ map treelines [nw,ne,se,sw])
  where
    sq' = scaleSq sq


treepoints :: QuadTree (DPoint2,DPoint2) DPoint2 -> [DPoint2]
treepoints = F.foldr fn [] where
  fn a = (:) (scalePt a)

main = demo1

scaleSq :: (DPoint2,DPoint2) -> (DPoint2,DPoint2)
scaleSq = prod scalePt

prod f (a,b) = (f a, f b) 

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

