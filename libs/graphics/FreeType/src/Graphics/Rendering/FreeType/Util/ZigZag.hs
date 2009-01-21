{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.FreeType.Util.ZigZag
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Array traversal
--
--------------------------------------------------------------------------------

module Graphics.Rendering.FreeType.Util.ZigZag where

import Data.Array.IArray ( IArray, Ix, bounds, (!) )


--------------------------------------------------------------------------------
-- traverse as a zig-zag i.e. left-to-right along a row, then jump to left zero
-- on the next row and traverse left-to-right again. 

zigZag :: (IArray arr e, Num i, Ix i) => 
              (e -> a -> a) -> a -> arr (i,i) e -> a
zigZag f a0 uarr = step (i0,j0) a0 
  where
    ((i0,j0),(i1,j1))         = bounds uarr
    
    step idx@(i,j) a 
        | j == j1 && i == i1  = f (uarr!idx) a
        | j == j1             = step (i+1,j0) (f (uarr!idx) a) 
        | otherwise           = step (i,j+1)  (f (uarr!idx) a) 
           
    
                  
zigZagM :: (IArray arr e, Num i, Ix i, Monad m) => 
              (e -> a -> m a) -> a -> arr (i,i) e -> m a
zigZagM f a0 uarr = step (i0,j0) a0 
  where
    ((i0,j0),(i1,j1)) = bounds uarr 
    
    step idx@(i,j) a 
        | j == j1 && i == i1 = f (uarr!idx) a
        | j == j1            = f (uarr!idx) a >>= step (i+1,j0)                  
        | otherwise          = f (uarr!idx) a >>= step (i,j+1)  


-- The phi function @h@ is preformed at the end of each row, to summate
-- the row.
           
zigZagPhi :: (IArray arr e, Num i, Ix i) => 
              (e -> a -> a) -> (a -> b -> b) -> (a,b) -> arr (i,i) e -> b
zigZagPhi f h (a0,b0) uarr = step (i0,j0) (a0,b0)
  where
    ((i0,j0),(i1,j1))         = bounds uarr
    
    step idx@(i,j) (a,b) 
        | j == j1 && i == i1  = h (f (uarr!idx) a) b
        | j == j1             = step (i+1,j0) (a0, h (f (uarr!idx) a) b)                  
        | otherwise           = step (i,j+1)  (f (uarr!idx) a, b)            


zigZagPhiM :: (IArray arr e, Num i, Ix i, Monad m) => 
              (e -> a -> m a) -> (a -> b -> m b) 
                  -> (a,b) -> arr (i,i) e -> m b
zigZagPhiM f h (a0,b0) uarr = step (i0,j0) (a0,b0)
  where
    ((i0,j0),(i1,j1))         = bounds uarr
    
    step idx@(i,j) (a,b) 
        | j == j1 && i == i1  = do a' <- f (uarr!idx) a 
                                   h a' b
        | j == j1             = do a' <- f (uarr!idx) a
                                   b' <- h a' b
                                   step (i+1,j0) (a0,b')                  
        | otherwise           = do a' <- f (uarr!idx) a
                                   step (i,j+1)  (a', b)  
                                   
                                             
--------------------------------------------------------------------------------
-- traverse columns i.e. top-to-bottom along a then, then jump to the top of 
-- the column and and traverse top-to-bottom again. 

zagZig :: (IArray arr e, Num i, Ix i) => 
              (e -> a -> a) -> a -> arr (i,i) e -> a
zagZig f a0 uarr = step (i0,j0) a0 
  where
    ((i0,j0),(i1,j1))         = bounds uarr
    
    step idx@(i,j) a 
        | j == j1 && i == i1  = f (uarr!idx) a
        | i == i1             = step (i0,j+1) (f (uarr!idx) a) 
        | otherwise           = step (i+1,j)  (f (uarr!idx) a)    
    

zagZigM :: (IArray arr e, Num i, Ix i, Monad m) => 
              (e -> a -> m a) -> a -> arr (i,i) e -> m a
zagZigM f a0 uarr = step (i0,j0) a0 
  where
    ((i0,j0),(i1,j1))         = bounds uarr
  
    step idx@(i,j) a 
        | j == j1 && i == i1  = f (uarr!idx) a
        | i == i1             = f (uarr!idx) a >>= step (i0,j+1) 
        | otherwise           = f (uarr!idx) a >>= step (i+1,j)    
    
     



-- The phi function @h@ is preformed at the end of each column, to summate
-- the column.

zagZigPhi :: (IArray arr e, Num i, Ix i) => 
              (e -> a -> a) -> (a -> b -> b) -> (a,b) -> arr (i,i) e -> b
zagZigPhi f h (a0,b0) uarr = step (i0,j0) (a0,b0)
  where
    ((i0,j0),(i1,j1))         = bounds uarr
    
    step idx@(i,j) (a,b) 
        | j == j1 && i == i1  = h (f (uarr!idx) a) b
        | i == i1             = step (i0,j+1) (a0, h (f (uarr!idx) a) b)                  
        | otherwise           = step (i+1,j)  (f (uarr!idx) a, b)   
        

        


zagZigPhiM :: (IArray arr e, Num i, Ix i, Monad m) 
            => (e -> a -> m a) -> (a -> b -> m b) 
            -> (a,b) -> arr (i,i) e -> m b
zagZigPhiM f h (a0,b0) uarr = step (i0,j0) (a0,b0)
  where
    ((i0,j0),(i1,j1))         = bounds uarr
    
    step idx@(i,j) (a,b) 
        | j == j1 && i == i1  = do a' <- f (uarr!idx) a 
                                   h a' b
        | i == i1             = do a' <- f (uarr!idx) a
                                   b' <- h a' b
                                   step (i0,j+1) (a0,b')                  
        | otherwise           = do a' <- f (uarr!idx) a
                                   step (i+1,j)  (a', b)  
      