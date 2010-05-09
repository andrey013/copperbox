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

import Wumpus.Extra.Utils

import qualified Data.Map as Map


-- The level naturals
data Z
data S a

instance Ord a => Ord (Point2 a) where
  compare (P2 x y) (P2 x' y') = (x,y) `compare` (x',y')

type Coord = Point2 Int

type CellValues u = Map.Map Coord (CellValue u)

type CellValue u = Either SimpleLabel (Picture u)

type SimpleLabel = String

type NodeId = Coord

{-
data GridElement = Node NodeId Coord
                 | Edge NodeId NodeId          
  deriving (Eq,Show)
-}


data GridSt = GridSt { posn :: Coord, line_count :: Int }
  deriving (Show)

type GridTrace u = CellValues u

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
runGridM (GridM f) =  f grid_state_zero Map.empty



grid :: GridM sh u a -> ((), GridSt, CellValues u)
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

tellNode :: String -> Coord -> GridTrace u -> GridTrace u
tellNode name loc cells = Map.insert loc (Left name) cells


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

matrixPicture :: (Fractional u, Ord u)
            => Vec2 u -> (GridSt,CellValues u) -> Picture u -> Picture u
matrixPicture (V2 sx sy) (st,cells) p = foldr fn p $ Map.toList cells
  where
    fn (pt, (Left s)) pic = pic `over` (mkLabel s `at` (remapCoord sx sy h pt))
    fn _              pic = pic
 
    (P2 _ h)  = posn st



mkLabel :: (Fractional u, Ord u) => String -> Picture u
mkLabel s = frame $ ztextlabel zeroPt s 


-- /Grid/ coordinates have origin top-left, they are remapped to
-- have the origin at the bottom right.
remapCoord :: Num u => u -> u -> Int -> Coord -> Point2 u
remapCoord sx sy h (P2 x y) = 
    P2 (sx * fromIntegral x) (sy * fromIntegral (h - y))


