{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.ZBitmap.Traverse
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Traverse
--
--------------------------------------------------------------------------------

module Graphics.ZBitmap.Traverse where

import Data.Array.IArray ( IArray, bounds )
import Data.Ix
import Data.List ( foldl' )

class Ix i => Rowwise a i | a -> i where
  rowBounds :: a -> (i,i)


  
rowHeight :: (Rowwise a i, Num i) => a -> i
rowHeight = (1+) . fn . rowBounds where
  fn (rmin,rmax) = rmax-rmin
  
rowIndexes :: (Rowwise a i, Enum i) => a -> [i]  
rowIndexes = fn . rowBounds where
  fn (rmin,rmax) = [rmin..rmax]

forEachRow :: (Rowwise a i, Enum i) => a -> (i -> b -> b) -> b -> b
forEachRow a f b0 = foldl' fun b0 $ rowIndexes a where
  fun b i = f i b 


--------------------------------------------------------------------------------
-- column wise
  
class Ix i => Colwise a i | a -> i where
  colBounds :: a -> (i,i)

colWidth :: (Colwise a i, Num i) => a -> i
colWidth = (1+) . fn . colBounds where
  fn (cmin,cmax) = cmax-cmin
  
colIndexes :: (Colwise a i, Enum i) => a -> [i]  
colIndexes = fn . colBounds where
  fn (cmin,cmax) = [cmin..cmax]

forEachCol :: (Colwise a i, Enum i) => (i -> a -> b -> b) -> a -> b -> b
forEachCol f a b0 = foldl' fun b0 $ colIndexes a where
  fun b i = f i a b 



forEachRowCol :: (Rowwise a i, Colwise a i , Enum i) 
              => a -> ((i,i) -> b -> b) -> b -> b
forEachRowCol a f b0 = foldl' fun b0 idxs where
    fun b (r,c) = f (r,c) b 
    idxs        = [(r,c) | r <- rowIndexes a,  c <- colIndexes a]
        
--------------------------------------------------------------------------------
-- Wrapper types and instances


-- C arrays are addressed [row][col] - K&R 2nd Ed. page 112
 
newtype RowColArr a i e = RowColArr { getRowColArr :: a i e }

instance (Ix i, IArray a e) => Rowwise (RowColArr a (i,i) e) i where
  rowBounds a = let ((rmin,_),(rmax,_)) = bounds $ getRowColArr a in (rmin,rmax)

instance (Ix i, IArray a e) => Colwise (RowColArr a (i,i) e) i where
  colBounds a = let ((_,cmin),(_,cmax)) = bounds $ getRowColArr a in (cmin,cmax)


-- Cartesian coords would have a bounds ((xmin,ymin),(xmax,ymax)) in Haskell 
-- notation, so the number of rows is ymax-ymin

newtype Cartesian a i e = Cartesian { getCartesian :: a i e }

instance (Ix i, IArray a e) => Rowwise (Cartesian a (i,i) e) i where
  rowBounds a = let ((_,ymin),(_,ymax)) = bounds $ getCartesian a in (ymin,ymax)

instance (Ix i, IArray a e) => Colwise (Cartesian a (i,i) e) i where
  colBounds a = let ((xmin,_),(xmax,_)) = bounds $ getCartesian a in (xmin,xmax)

cartesianBounds :: (Colwise (RowColArr a i e) i, Rowwise (RowColArr a i e) i)
                => (Ix i, IArray a e) => RowColArr a i e -> ((i,i),(i,i))  
cartesianBounds a = 
    let (y0,y1) = rowBounds a; (x0,x1) = colBounds a
    in ((x0,y0),(x1,y1))

rowColBounds :: (Colwise (Cartesian a i e) i, Rowwise (Cartesian a i e) i)
             => Cartesian a i e -> ((i,i),(i,i))  
rowColBounds a = 
    let (r0,r1) = rowBounds a; (c0,c1) = colBounds a
    in ((r0,c0),(r1,c1))
    
    