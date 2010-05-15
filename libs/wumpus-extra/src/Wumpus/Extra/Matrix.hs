{-# LANGUAGE EmptyDataDecls             #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Extra.Matrix
-- Copyright   :  (c) Stephen Tetley 2009-2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC with TypeFamilies and more
--
-- Drawing in a matrix grid, inspired by TikZ... 
-- 
--------------------------------------------------------------------------------

module Wumpus.Extra.Matrix
  (
  
    NodeId
  , grid

  , nil 
  , node
  , blank
  , ( & )

  , connect
  
  , cell0
  , cell1
  , cell2
  , cell3
  , cell4
  , cell5
  , cell6
  , cell7
  , cell8
  , cell9
  , cell10

  , matrixPicture

  ) where

import Wumpus.Core hiding ( blank )

import Wumpus.Extra.Arrows
import Wumpus.Extra.CoreAdditions ( zeroPicture, centeredAt )
import Wumpus.Extra.Utils

import qualified Data.Map as Map


-- The level naturals
data Z
data S a


type GridCoord = Point2 Int

type CellValues u = Map.Map GridCoord (CellValue u)


data CellValue u = CellPic (Picture u)
                 | CellNode SimpleLabel
  deriving (Eq,Show)

type SimpleLabel = String

type NodeId = GridCoord

-- likely to need annotating eg. for arrowheads, curviture...
data Connector = Conn GridCoord GridCoord
  deriving (Eq,Show)

data GridSt = GridSt { posn :: GridCoord, line_count :: Int }
  deriving (Show)

type GridTrace u = (CellValues u, [Connector])

-- Note - rows always start with nil which increments the y-pos
-- oblivious to its current value. If the initial state was
-- (0,0) the initial nil would move the coord to (0,1), so we
-- have to start the initial coor at (0,-1).
--
grid_state_zero :: GridSt
grid_state_zero = GridSt { posn = P2 0 (-1), line_count = 0 }
  

-- sh - shape
-- u  - unit (usually Double)

data GridM sh u a = GridM { 
          unGridM :: GridSt -> GridTrace u -> (a,GridSt,GridTrace u) }


runGridM :: GridM sh u a -> (a, GridSt, GridTrace u)
runGridM (GridM f) =  f grid_state_zero (Map.empty,[])



grid :: GridM sh u a -> ((), GridSt, GridTrace u)
grid = post . runGridM  
  where 
    post (_,s,w) = ((),s,w)


instance Functor (GridM sh u) where
  fmap f (GridM g) = GridM $ \s w -> let (a,s',w') =  g s w in (f a,s',w')


instance Monad (GridM sh u) where
  return a  = GridM $ \s w -> (a,s,w)
  (GridM f) >>= mf  = GridM $ \s w -> 
    let (a,s',w') = f s w in (unGridM . mf) a s' w'


nextCol :: GridSt -> GridSt
nextCol = pstar upf posn where 
    upf (P2 x y) s = s { posn=(P2 (x+1) y) }


nextRow :: GridSt -> GridSt
nextRow = pstar upf posn where
  upf (P2 _ y) s = s { posn=(P2 0 (y+1)) }

tellNode :: String -> GridCoord -> GridTrace u -> GridTrace u
tellNode name loc (cells,ls) = (Map.insert loc (CellNode name) cells,ls)

tellLink :: GridCoord -> GridCoord ->  GridTrace u -> GridTrace u
tellLink a b (cells,ls) = (cells, Conn a b : ls)


--------------------------------------------------------------------------------
-- constructing \'matrices\'


nil :: GridM Z u ()
nil = GridM $ \s w -> ((), nextRow s, w)

blank :: GridM sh u a -> GridM (S sh) u a
blank (GridM mf) = GridM $ \s w -> 
    let (acc,s',w') = mf s w in (acc, nextCol s', w')


node :: SimpleLabel -> GridM sh u a -> GridM (S sh) u (a,NodeId)
node name (GridM mf) = GridM $ \s w -> 
    let (acc,s',w') = mf s w 
        loc         = posn s'
    in ((acc,loc), nextCol s', tellNode name loc w')



infixl 5 &
(&) :: GridM sh u a -> (GridM sh u a -> GridM (S sh) u b) -> GridM (S sh) u b
tl & hf = hf tl


connect :: NodeId -> NodeId -> GridM sh u ()
connect from to = GridM $ \s w -> ((),s, tellLink from to w)

--------------------------------------------------------------------------------
-- An arity family of cell \'selectors\'


cell0 :: GridM sh u () -> GridM sh u ()
cell0 (GridM mf) = GridM $ \s w -> mf s w

cell1 :: GridM sh u ((),a) -> GridM sh u a
cell1 (GridM mf) = GridM $ \s w -> 
    let (((),a),s',w') = mf s w in (a,s',w')

cell2 :: GridM sh u (((),a),a) -> GridM sh u (a,a)
cell2 (GridM mf) = GridM $ \s w -> 
    let ((((),a),b),s',w') = mf s w in ((a,b),s',w')

cell3 :: GridM sh u ((((),a),a),a) -> GridM sh u (a,a,a)
cell3 (GridM mf) = GridM $ \s w -> 
    let (((((),a),b),c),s',w') = mf s w in ((a,b,c),s',w')

cell4 :: GridM sh u (((((),a),a),a),a) -> GridM sh u (a,a,a,a)
cell4 (GridM mf) = GridM $ \s w -> 
    let ((((((),a),b),c),d),s',w') = mf s w in ((a,b,c,d),s',w')

cell5 :: GridM sh u ((((((),a),a),a),a),a) -> GridM sh u (a,a,a,a,a)
cell5 (GridM mf) = GridM $ \s w -> 
    let (((((((),a),b),c),d),e),s',w') = mf s w in ((a,b,c,d,e),s',w')

cell6 :: GridM sh u (((((((),a),a),a),a),a),a) -> GridM sh u (a,a,a,a,a,a)
cell6 (GridM mf) = GridM $ \s w -> 
    let ((((((((),a),b),c),d),e),f),s',w') = mf s w in ((a,b,c,d,e,f),s',w')

cell7 :: GridM sh u ((((((((),a),a),a),a),a),a),a) 
      -> GridM sh u (a,a,a,a,a,a,a)
cell7 (GridM mf) = GridM $ \s w -> 
    let (((((((((),a),b),c),d),e),f),g),s',w') = mf s w 
    in ((a,b,c,d,e,f,g),s',w')

cell8 :: GridM sh u (((((((((),a),a),a),a),a),a),a),a) 
      -> GridM sh u (a,a,a,a,a,a,a,a)
cell8 (GridM mf) = GridM $ \s w -> 
    let ((((((((((),a),b),c),d),e),f),g),h),s',w') = mf s w 
    in ((a,b,c,d,e,f,g,h),s',w')

cell9 :: GridM sh u ((((((((((),a),a),a),a),a),a),a),a),a)
      -> GridM sh u (a,a,a,a,a,a,a,a,a)
cell9 (GridM mf) = GridM $ \s w -> 
    let (((((((((((),a),b),c),d),e),f),g),h),i),s',w') = mf s w 
    in ((a,b,c,d,e,f,g,h,i),s',w')

cell10 :: GridM sh u (((((((((((),a),a),a),a),a),a),a),a),a),a)
      -> GridM sh u (a,a,a,a,a,a,a,a,a,a)
cell10 (GridM mf) = GridM $ \s w -> 
    let ((((((((((((),a),b),c),d),e),f),g),h),i),j),s',w') = mf s w 
    in ((a,b,c,d,e,f,g,h,i,j),s',w')

--------------------------------------------------------------------------------
-- creating a picture

-- This is just a dummy and isn't very good at all...
--
-- Need to be cleverer about centering cells, bounding boxes, 
-- etc.
--

{-
matrixPicture :: (Floating u, Fractional u, Real u, Ord u)
            => Vec2 u -> (GridSt,GridTrace u) -> Picture u -> Picture u
matrixPicture vscale (st,(cells,ls)) p = all_conns `over` all_nodes
  where
    all_nodes = foldr f p $ Map.toList cells
    all_conns = foldr g (blankPicture (boundary all_nodes)) ls

    f (pt, (CellNode s)) pic = pic `over` (mkLabel s `at` (remapCoord h vscale pt))
    f _                  pic = pic

    g (Conn a b)         pic = pic `over`
       arrowTri' () (remapCoord h vscale a) (remapCoord h vscale b)
 
    (P2 _ h)  = posn st
-}

-- Note TikZ shapes (ch. 46 Shape Library) support anchors on 
-- the boundary of shapes, rather than the boundary of the BBox
-- 
-- It would be nice to have shape boundaries...
--
type Border u = BoundingBox u

-- maybe add the scaling function so missing cells can be 'found'...
type BorderMap u = Map.Map GridCoord (Border u)

matrixPicture :: (Floating u, Fractional u, Real u, Ord u)
               => Vec2 u -> (GridSt,GridTrace u) -> Picture u
matrixPicture vscale (st,(cells,ls)) = p2
  where
    scalef  = remapCoord (columnCount st) vscale
    (p1,bm) = processCells scalef cells
    p2      = processConnectors bm ls p1

processCells :: (Fractional u, Ord u) 
             => (GridCoord -> Point2 u) 
             -> CellValues u 
             -> (Picture u, BorderMap u)
processCells f cvs = foldr step (zeroPicture,Map.empty) xs
  where 
    xs = Map.toAscList cvs 
    step (coord,CellPic p)  acc     = work coord p acc
    step (coord,CellNode s) acc     = work coord (mkLabel s) acc

    work coord p (ps,bm) = (p' `over` ps, Map.insert coord bb bm)
      where
        p' = p `centeredAt` (f coord)  -- displace center??
        bb = boundary p'

processConnectors :: (Floating u, Real u, Ord u) 
                  => BorderMap u -> [Connector] -> Picture u -> Picture u
processConnectors bm xs p0 = foldr fn p0 xs 
  where
    fn (Conn a b) p = case (findBorder a bm, findBorder b bm) of
        (Just b1, Just b2) -> let arr = arrowTri' () (boundaryPoint C b1) 
                                                     (boundaryPoint C b2) 
                              in arr `over` p
        _                  -> p


mkLabel :: (Fractional u, Ord u) => String -> Picture u
mkLabel s = frame $ ztextlabel zeroPt s 


findBorder :: GridCoord -> BorderMap u -> Maybe (Border u)
findBorder = Map.lookup


columnCount :: GridSt -> Int 
columnCount (GridSt { posn=(P2 _ h)}) = h

-- /Grid/ coordinates have origin top-left, they are remapped to
-- have the origin at the bottom right.
remapCoord :: Num u => Int -> (Vec2 u) -> GridCoord -> Point2 u
remapCoord h (V2 sx sy)  (P2 x y) = 
    P2 (sx * fromIntegral x) (sy * fromIntegral (h - y))
