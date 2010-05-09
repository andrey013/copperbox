{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE RankNTypes                 #-}
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

module Wumpus.Doodle.Grid where

import Wumpus.Core

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


elem0 :: GridM sh () -> GridM sh ()
elem0 (GridM mf) = GridM $ \s w -> mf s w



elem1 :: GridM sh ((),a) -> GridM sh a
elem1 (GridM mf) = GridM $ \s w -> 
    let (((),a),s',w') = mf s w in (a,s',w')


elem2 :: GridM sh (((),a),b) -> GridM sh (a,b)
elem2 (GridM mf) = GridM $ \s w -> 
    let ((((),a),b),s',w') = mf s w in ((a,b),s',w')



elem3 :: GridM sh ((((),a),b),c) -> GridM sh (a,b,c)
elem3 (GridM mf) = GridM $ \s w -> 
    let (((((),a),b),c),s',w') = mf s w in ((a,b,c),s',w')


nodePicture :: (Fractional u, Ord u)
            => u -> u -> Int -> [GridElement] -> Picture u -> Picture u
nodePicture sx sy h xs p = foldr fn p xs where
  fn (Node s pt) pic = pic `over` (mkLabel s `at` (remapCoord sx sy h pt))
  fn _           pic = pic


mkLabel :: (Fractional u, Ord u) => String -> Picture u
mkLabel s = frame $ ztextlabel zeroPt s 


-- /Grid/ coordinates have origin top-left, they are remapped to
-- have the origin at the bottom right.
remapCoord :: Num u => u -> u -> Int -> Coord -> Point2 u
remapCoord sx sy h (P2 x y) = 
    P2 (sx * fromIntegral x) (sy * fromIntegral (h - y))


{-

mapPosn :: (Coord -> Coord) -> GridSt -> GridSt
mapPosn f = GridSt . f . posn

tell :: WriterM m (H i) => i -> m () 
tell = put . consH

-- | Move to next row...
row :: GridM ()
row = sets_ (mapPosn f) where f (P2 _ y) = P2 0 (succ y)

nextcol :: GridM ()
nextcol = sets_ (mapPosn f) where f (P2 x y) = P2 (succ x) y


node :: String -> GridM NodeId
node name = do 
    a <- sets (\s -> (posn s, mapPosn f s))
    tell $ Node name a
    return name
  where
    f (P2 x y) = P2 (succ x) y 

    

nodePicture :: (Fractional u, Ord u)
            => u -> u -> Int -> [GridElement] -> Picture u -> Picture u
nodePicture sx sy h xs p = foldr fn p xs where
  fn (Node s pt) pic = pic `over` (mkLabel s `at` (remapCoord sx sy h pt))
  fn _           pic = pic


mkLabel :: (Fractional u, Ord u) => String -> Picture u
mkLabel s = frame $ ztextlabel zeroPt s 


-- /Grid/ coordinates have origin top-left, they are remapped to
-- have the origin at the bottom right.
remapCoord :: Num u => u -> u -> Int -> Coord -> Point2 u
remapCoord sx sy h (P2 x y) = 
    P2 (sx * fromIntegral x) (sy * fromIntegral (h - y))

-}