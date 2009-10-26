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
-- Grids...
-- 
--------------------------------------------------------------------------------

module Wumpus.Extra.Grid where

import Wumpus.Core.Geometry
import Wumpus.Core.Picture 

import Data.Map ( Map )
import qualified Data.Map as Map 


type Coord = Point2 Int

data Node = NamedNode String
          | AnonNode  Int
  deriving (Eq,Ord,Show)

type PlacedNode = (Node,Coord)

data Grid = Grid { unGrid :: [PlacedNode] }


type PlacedLabel u = (String,Point2 u)


-- /Grid/ coordinates have origin top-left, they are remapped to
-- have the origin at the bottom right.
remapCoord :: Num u => u -> u -> Int -> Coord -> Point2 u
remapCoord sx sy h (P2 x y) = 
    P2 (sx * fromIntegral x) (sy * fromIntegral (h - y))


type NodeMap u = Map Node (Point2 u)

nodeMap :: Num u => u -> u -> Grid -> NodeMap u
nodeMap sx sy (Grid xs) = foldr fn Map.empty xs
  where
    fn (n,c) mp = Map.insert n (remapCoord sx sy height c) mp
    height      = foldr (\(_,P2 _ y) h -> max y h) 0 xs


mkLabel :: (Num u, Ord u) => String -> Picture u
mkLabel s = picLabel 10 1 15 26 s 

nodePicture :: (Num u, Ord u) => NodeMap u -> Picture u
nodePicture = Map.foldWithKey fn picEmpty where
  fn (NamedNode s) pt pic = pic `overlay` (mkLabel s `at` pt)
  fn _             _  pic = pic