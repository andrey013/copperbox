{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.FreeType.Util.Array
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

module Graphics.Rendering.FreeType.Util.Array where

import Data.Array.Unboxed ( IArray, Ix, UArray, bounds, (!) )
import Data.Foldable ( foldl', foldlM ) 
    

-- TODO - direct recursion would probably be better as we would be building
-- the lists of indexes.
          
--------------------------------------------------------------------------------
-- row-wise traversal

rowwise :: (IArray UArray e, Enum i, Ix i) => 
              (e -> a -> a) -> a -> UArray (i,i) e -> a
rowwise f a uarr = 
    foldl' (\b x -> foldl' (\c y -> f (uarr!(x,y)) c) b [y0..y1]) a [x0..x1]
  where
    ((x0,y0),(x1,y1)) = bounds uarr


rowwiseM :: (IArray UArray e, Enum i, Ix i, Monad m) => 
              (e -> a -> m a) -> a -> UArray (i,i) e -> m a
rowwiseM f a uarr = 
    foldlM (\b x -> foldlM (\c y -> f (uarr!(x,y)) c) b [y0..y1]) a [x0..x1]
  where
    ((x0,y0),(x1,y1)) = bounds uarr
     
--------------------------------------------------------------------------------
-- column-wise traversal

colwise :: (IArray UArray e, Enum i, Ix i) => 
              (e -> a -> a) -> a -> UArray (i,i) e -> a
colwise f a uarr = 
    foldl' (\b y -> foldl' (\c x -> f (uarr!(x,y)) c) b [x0..x1]) 
           a 
           [y0..y1]
  where
    ((x0,y0),(x1,y1)) = bounds uarr 
    
                  
colwiseM :: (IArray UArray e, Enum i, Ix i, Monad m) => 
              (e -> a -> m a) -> a -> UArray (i,i) e -> m a
colwiseM f a uarr = 
    foldlM (\b y -> foldlM (\c x -> f (uarr!(x,y)) c) b [x0..x1]) 
           a 
           [y0..y1]
  where
    ((x0,y0),(x1,y1)) = bounds uarr            

--------------------------------------------------------------------------------
-- row-wise traversal with a function performed on the result of a row.
-- Similar to an apomorphism except @h@ is preformed at the end of each row
-- rather than at the end of the traversal  


rowwiseOpa :: (IArray UArray e, Enum i, Ix i) => 
              (e -> a -> a) -> (a -> b -> b) -> (a,b) -> UArray (i,i) e -> b
rowwiseOpa f h (a,b) uarr = 
    foldl' (\b' x -> h (foldl' (\a' y -> f (uarr!(x,y)) a') a [y0..y1]) b') 
           b 
           [x0..x1]
  where
    ((x0,y0),(x1,y1)) = bounds uarr          


rowwiseOpaM :: (IArray UArray e, Enum i, Ix i, Monad m) => 
              (e -> a -> m a) -> (a -> b -> m b) 
                  -> (a,b) -> UArray (i,i) e -> m b
rowwiseOpaM f h (a,b) uarr = 
    foldlM (\b' x -> do { ans <- foldlM (\a' y -> f (uarr!(x,y)) a') 
                                        a 
                                        [y0..y1] 
                        ; h ans b'} ) 
           b 
           [x0..x1]
  where
    ((x0,y0),(x1,y1)) = bounds uarr  

--------------------------------------------------------------------------------
-- column-wise traversal with \opa\ 


colwiseOpa :: (IArray UArray e, Enum i, Ix i) => 
              (e -> a -> a) -> (a -> b -> b) -> (a,b) -> UArray (i,i) e -> b
colwiseOpa f h (a,b) uarr = 
    foldl' (\b' y -> h (foldl' (\a' x -> f (uarr!(x,y)) a') a [x0..x1]) b') 
           b 
           [y0..y1]
  where
    ((x0,y0),(x1,y1)) = bounds uarr          


colwiseOpaM :: (IArray UArray e, Enum i, Ix i, Monad m) => 
              (e -> a -> m a) -> (a -> b -> m b) 
                  -> (a,b) -> UArray (i,i) e -> m b
colwiseOpaM f h (a,b) uarr = 
    foldlM (\b' y -> do { ans <- foldlM (\a' x -> f (uarr!(x,y)) a') 
                                        a 
                                        [x0..x1] 
                        ; h ans b'} ) 
           b 
           [y0..y1]
  where
    ((x0,y0),(x1,y1)) = bounds uarr



      