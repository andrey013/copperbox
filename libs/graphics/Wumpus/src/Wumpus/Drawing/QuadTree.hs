{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Drawing.QuadTree
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Quad Tree data structure
--
--------------------------------------------------------------------------------


module Wumpus.Drawing.QuadTree where

import Wumpus.Core.Geometric
import Wumpus.Core.Instances ()
import Wumpus.Core.Point

import Data.Foldable hiding ( foldr )
import Data.Monoid



-- cache height
data QuadTree square a 
      = Empty square
      | Leaf square a 
      | Quad { tree_height  :: Int 
             , quad_square  :: square
             , quad_nw      :: QuadTree square a
             , quad_ne      :: QuadTree square a
             , quad_se      :: QuadTree square a
             , quad_sw      :: QuadTree square a
             }
  deriving (Eq,Show)


instance Functor (QuadTree square) where
  fmap _ (Empty sq)              = Empty sq
  fmap f (Leaf sq a)             = Leaf sq (f a)
  fmap f (Quad h sq nw ne se sw) = Quad h sq (fmap f nw) (fmap f ne) (fmap f se) (fmap f sw)



instance Foldable (QuadTree square) where
  foldMap _ (Empty _)              = mempty
  foldMap f (Leaf _ a)             = f a
  foldMap f (Quad _ _ nw ne se sw) = foldMap f nw `mappend` 
                                     foldMap f ne `mappend` 
                                     foldMap f se `mappend` foldMap f sw


max4 :: Ord a => a -> a -> a -> a -> a
max4 a b c d = max a $ max b $ max c d

height :: QuadTree square a -> Int
height (Empty _)              = 0
height (Leaf _ _)             = 1
height (Quad {tree_height=h}) = h


-- The representation of a squares area must support subdivision to 
-- generate the areas of the quadrants
class Quadrants sq where
  quadrants :: sq -> (sq,sq,sq,sq)

class Within pt sq where
  within :: pt -> sq -> Bool


build :: (Quadrants sq, Within a sq, Eq a) => sq -> [a] -> QuadTree sq a
build sq []    = Empty sq
build sq [a]   = Leaf sq a
build sq [a,b] | a == b = Leaf sq a
build sq xs    = Quad h sq nw ne se sw where
    h                 = 1 + max4 (height nw) (height ne) (height se) (height sw)
    (qnw,qne,qse,qsw) = quadrants sq
    (nws,nes,ses,sws) = foldr locate ([],[],[],[]) xs
    
    nw                = build qnw nws
    ne                = build qne nes
    se                = build qse ses
    sw                = build qsw sws

    locate x (as,bs,cs,ds)
      | x `within` qnw = (x:as,bs,cs,ds)
      | x `within` qne = (as,x:bs,cs,ds)
      | x `within` qse = (as,bs,x:cs,ds)
      | otherwise      = (as,bs,cs,x:ds)


instance Quadrants (DPoint2,DPoint2) where
  quadrants (p1@(P2 x0 y0),p2@(P2 x1 y1)) = (nw,ne,se,sw) 
    where
      P2 xmid ymid  = midpoint p1 p2
      nw            = (P2 x0 ymid, P2 xmid y1)
      ne            = (P2 xmid ymid, p2)
      se            = (P2 xmid y0,   P2 x1 ymid)
      sw            = (p1,           P2 xmid ymid)  

instance Within DPoint2 (DPoint2,DPoint2) where
  within (P2 x y)  (P2 x0 y0,P2 x1 y1) = 
      x0 <= x && x <= x1 && y0 <= y && y <= y1 

