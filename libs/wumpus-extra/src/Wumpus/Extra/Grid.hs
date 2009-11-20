{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Extra.Grid
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  experimental
-- Portability :  GHC only
--
-- Grids, just a doodle...
-- 
--------------------------------------------------------------------------------

module Wumpus.Extra.Grid where

import Wumpus.Core


import MonadLib

import qualified Data.DList as DL

-- import MonadLib.Monads


type Coord = Point2 Int

type NodeId = String

data GridElement = Node NodeId Coord
                 | Edge NodeId NodeId          
  deriving (Eq,Show)


data GridSt = GridSt { posn :: Coord }
  deriving (Show)

type GridTrace = DL.DList GridElement


grid_state_zero :: GridSt
grid_state_zero = GridSt { posn = zeroPt }


newtype GridT m a = GridT { 
          unGridT :: StateT GridSt (WriterT GridTrace m) a
        }  

type GridM a = GridT Id a


runGridT :: Monad m => GridT m a -> m ((a,GridSt),GridTrace)
runGridT m = runWriterT $ runStateT grid_state_zero (unGridT m) 



grid :: GridM a -> (a,[GridElement])
grid = fn . runId . runGridT  where fn ((a,_),t) = (a,DL.toList t)


instance Monad m => Functor (GridT m) where
  fmap f (GridT mf) = GridT $ fmap f mf 

instance Monad m => Monad (GridT m) where
  return a  = GridT $ return a
  ma >>= f  = GridT $ unGridT ma >>= unGridT . f

instance Monad m => WriterM (GridT m) GridTrace where
  put = GridT . put

instance Monad m => StateM (GridT m) GridSt where
  get = GridT $ get
  set = GridT . set

instance MonadT GridT where
  lift = GridT . lift . lift





mapPosn :: (Coord -> Coord) -> GridSt -> GridSt
mapPosn f = GridSt . f . posn

tell :: WriterM m (DL.DList i) => i -> m () 
tell = put . DL.singleton

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
