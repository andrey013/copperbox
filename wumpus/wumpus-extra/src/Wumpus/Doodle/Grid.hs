{-# LANGUAGE EmptyDataDecls             #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Doodle.Grid
-- Copyright   :  (c) Stephen Tetley 2009-2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC with TypeFamilies and more
--
-- Grids, just a doodle...
-- 
--------------------------------------------------------------------------------

module Wumpus.Doodle.Grid 
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

import Data.Monoid

data Z
data S a


type Coord = Point2 Int

type NodeId = String

data GridElement = Node NodeId Coord
                 | Edge NodeId NodeId          
  deriving (Eq,Show)


data GridSt = GridSt { posn :: Coord }
  deriving (Show)

type GridTrace = H GridElement

-- Note - rows always start with nil which increments the y-pos
-- oblivious to its current value. If the initial state was
-- (0,0) the initial nil would move the coord to (0,1), so we
-- have to start the initial coor at (0,-1).
--
grid_state_zero :: GridSt
grid_state_zero = GridSt { posn = P2 0 (-1) }
  


data GridM sh a = GridM { 
          unGridM :: GridSt -> GridTrace -> (a,GridSt,GridTrace) }


runGridM :: GridM sh a -> (a,GridSt,GridTrace)
runGridM (GridM f) =  f grid_state_zero mempty 



grid :: GridM sh a -> (a,[GridElement])
grid = post . runGridM  
  where 
    post (a,_,t) = (a,toListH t)


instance Functor (GridM sh) where
  fmap f (GridM g) = GridM $ \s w -> let (a,s',w') =  g s w in (f a,s',w')


instance Monad (GridM sh) where
  return a  = GridM $ \s w -> (a,s,w)
  (GridM f) >>= mf  = GridM $ \s w -> 
    let (a,s',w') = f s w in (unGridM . mf) a s' w'


nextCol :: GridSt -> GridSt
nextCol = pstar upf posn where 
    upf (P2 x y) s = s { posn=(P2 (x+1) y) }


nextRow :: GridSt -> GridSt
nextRow = pstar upf posn where
  upf (P2 _ y) s = s { posn=(P2 0 (y+1)) }

tellNode :: String -> Coord -> GridTrace -> GridTrace
tellNode name loc hf = consH (Node name loc) hf


--------------------------------------------------------------------------------
-- constructing \'matrices\'


nil :: GridM Z ()
nil = GridM $ \s w -> ((), nextRow s, w)

blank :: GridM sh a -> GridM (S sh) a
blank (GridM mf) = GridM $ \s w -> 
    let (acc,s',w') = mf s w in (acc, nextCol s', w')


node :: NodeId -> GridM sh a -> GridM (S sh) (a,NodeId)
node name (GridM mf) = GridM $ \s w -> 
    let (acc,s',w') = mf s w 
        loc         = posn s'
    in ((acc,name), nextCol s', tellNode name loc w')



infixl 5 &
(&) :: GridM sh a -> (GridM sh a -> GridM (S sh) b) -> GridM (S sh) b
tl & hf = hf tl


--------------------------------------------------------------------------------
-- An arity family of cell \'selectors\'


cell0 :: GridM sh () -> GridM sh ()
cell0 (GridM mf) = GridM $ \s w -> mf s w

cell1 :: GridM sh ((),a) -> GridM sh a
cell1 (GridM mf) = GridM $ \s w -> 
    let (((),a),s',w') = mf s w in (a,s',w')

cell2 :: GridM sh (((),a),a) -> GridM sh (a,a)
cell2 (GridM mf) = GridM $ \s w -> 
    let ((((),a),b),s',w') = mf s w in ((a,b),s',w')

cell3 :: GridM sh ((((),a),a),a) -> GridM sh (a,a,a)
cell3 (GridM mf) = GridM $ \s w -> 
    let (((((),a),b),c),s',w') = mf s w in ((a,b,c),s',w')

cell4 :: GridM sh (((((),a),a),a),a) -> GridM sh (a,a,a,a)
cell4 (GridM mf) = GridM $ \s w -> 
    let ((((((),a),b),c),d),s',w') = mf s w in ((a,b,c,d),s',w')

cell5 :: GridM sh ((((((),a),a),a),a),a) -> GridM sh (a,a,a,a,a)
cell5 (GridM mf) = GridM $ \s w -> 
    let (((((((),a),b),c),d),e),s',w') = mf s w in ((a,b,c,d,e),s',w')

cell6 :: GridM sh (((((((),a),a),a),a),a),a) -> GridM sh (a,a,a,a,a,a)
cell6 (GridM mf) = GridM $ \s w -> 
    let ((((((((),a),b),c),d),e),f),s',w') = mf s w in ((a,b,c,d,e,f),s',w')

cell7 :: GridM sh ((((((((),a),a),a),a),a),a),a) -> GridM sh (a,a,a,a,a,a,a)
cell7 (GridM mf) = GridM $ \s w -> 
    let (((((((((),a),b),c),d),e),f),g),s',w') = mf s w 
    in ((a,b,c,d,e,f,g),s',w')

cell8 :: GridM sh (((((((((),a),a),a),a),a),a),a),a) 
      -> GridM sh (a,a,a,a,a,a,a,a)
cell8 (GridM mf) = GridM $ \s w -> 
    let ((((((((((),a),b),c),d),e),f),g),h),s',w') = mf s w 
    in ((a,b,c,d,e,f,g,h),s',w')

cell9 :: GridM sh ((((((((((),a),a),a),a),a),a),a),a),a)
      -> GridM sh (a,a,a,a,a,a,a,a,a)
cell9 (GridM mf) = GridM $ \s w -> 
    let (((((((((((),a),b),c),d),e),f),g),h),i),s',w') = mf s w 
    in ((a,b,c,d,e,f,g,h,i),s',w')

cell10 :: GridM sh (((((((((((),a),a),a),a),a),a),a),a),a),a)
      -> GridM sh (a,a,a,a,a,a,a,a,a,a)
cell10 (GridM mf) = GridM $ \s w -> 
    let ((((((((((((),a),b),c),d),e),f),g),h),i),j),s',w') = mf s w 
    in ((a,b,c,d,e,f,g,h,i,j),s',w')

--------------------------------------------------------------------------------
-- creating a picture

matrixPicture :: (Fractional u, Ord u)
            => u -> u -> Int -> [GridElement] -> Picture u -> Picture u
matrixPicture sx sy h xs p = foldr fn p xs where
  fn (Node s pt) pic = pic `over` (mkLabel s `at` (remapCoord sx sy h pt))
  fn _           pic = pic


mkLabel :: (Fractional u, Ord u) => String -> Picture u
mkLabel s = frame $ ztextlabel zeroPt s 


-- /Grid/ coordinates have origin top-left, they are remapped to
-- have the origin at the bottom right.
remapCoord :: Num u => u -> u -> Int -> Coord -> Point2 u
remapCoord sx sy h (P2 x y) = 
    P2 (sx * fromIntegral x) (sy * fromIntegral (h - y))


