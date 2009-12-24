{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  GreyFold.Base.SyntheticFolds
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined
--
-- Folds that can be defined with foldl or foldr, so can be implemented
-- for all foldable types.
--
--------------------------------------------------------------------------------


module GreyFold.Base.SyntheticFolds (
  
  -- * counting folds
  countfoldl, countfoldr,
  countfoldlM, countfoldrM,

  -- * stateful folds
  statefoldl, statefoldr,
  statefoldlM, statefoldrM,
  
  -- * double accumulator folds
  dafoldl, dafoldr,
  dafoldlM, dafoldrM,
  
) where

import Data.Foldable
import Prelude hiding (foldl,foldr)

-- -----------------------------------------------------------------------------
-- counting folds

-- | @countfoldl@ - a stateful fold where the count is incremented at each 
-- step. The initial count is supplied as a parameter, while this clutters 
-- the interface it does allow counting from 0,1 or any other Int.
countfoldl :: Foldable t => (b -> a -> Int -> b) -> Int -> b -> t a -> b
countfoldl f i0 b0 ta = fst $ foldl (adapt f) (b0,i0) ta where
    adapt fn = \(b,i) a -> (fn b a i,i+1)
    

-- | @countfoldr@ - right to left version of countfoldl.
countfoldr :: (a -> b -> Int -> b) -> Int -> b -> [a] -> b
countfoldr f i0 b0 ta = fst $ foldr (adapt f) (b0,i0) ta where
    adapt fn = \a (b,i) -> (fn a b i,i+1)
        
-- helper
revPairupM :: Monad m => b -> a -> m (a,b) 
revPairupM b a = return (a,b) 

        
-- | @countfoldlM@ - monadic version of countfoldl.  
countfoldlM :: Monad m => (b -> a -> Int -> m b) -> Int -> b -> [a] -> m b
countfoldlM f i0 b0 ta = (return . fst) =<< foldlM (adapt f) (b0,i0) ta where
    adapt fn = \(b,i) a -> fn b a i >>= revPairupM (i+1) 

-- | @countfoldrM@ - monadic version of countfoldr.
countfoldrM :: Monad m => (a -> b -> Int -> m b) -> Int -> b -> [a] -> m b
countfoldrM f i0 b0 ta = (return . fst) =<< foldrM (adapt f) (b0,i0) ta where
    adapt fn = \a (b,i) -> fn a b i >>= revPairupM (i+1) 


-- -----------------------------------------------------------------------------
-- stateful folds

-- | @statefoldl@ - a generalization of countfoldl, the state is accessible 
-- to the reduction function, and the final state is returned with the 
-- summary value.  
statefoldl :: (b -> a -> st -> (b,st)) -> st -> b -> [a] -> (b,st) 
statefoldl f st0 b0 ta = foldl (adapt f) (b0,st0) ta where
    adapt fn = \(b,st) a -> fn b a st

-- | @statefoldr@ - right to left version of statefoldl.  
statefoldr :: (a -> b -> st -> (b,st)) -> st -> b -> [a] -> (b,st) 
statefoldr f st0 b0 ta = foldr (adapt f) (b0,st0) ta where
    adapt fn = \a (b,st) -> fn a b st
    
-- | @statefoldlM@ - monadic version of statefoldl. 
statefoldlM :: (Monad m, Foldable t) => 
    (b -> a -> st -> m (b,st)) -> st -> b -> t a -> m (b,st) 
statefoldlM f st0 b0 ta = foldlM (adapt f) (b0,st0) ta where
    adapt fn = \(b,st) a -> fn b a st

-- | @statefoldrM@ - monadic version of statefoldr.  
statefoldrM :: (Monad m, Foldable t) => 
    (a -> b -> st -> m (b,st)) -> st -> b -> t a -> m (b,st) 
statefoldrM f st0 b0 ta = foldrM (adapt f) (b0,st0) ta where
    adapt fn = \a (b,st) -> fn a b st
    
-- -----------------------------------------------------------------------------
-- double accumulator folds

dafoldl :: Foldable t =>
    (b -> c -> a -> (b,c)) -> (b -> c -> d) -> c -> b -> t a -> d
dafoldl f g c0 b0 ta  = uncurry g $ foldl (adapt f) (b0,c0) ta where
    adapt fn = \(b,c) a -> fn b c a

-- | @dafoldr@ - right to left version of dafoldl.
dafoldr :: Foldable t =>
    (a -> b -> c -> (b,c)) -> (b -> c -> d) -> c -> b -> t a -> d
dafoldr f g c0 b0 ta  = uncurry g $ foldr (adapt f) (b0,c0) ta where
    adapt fn = \a (b,c)-> fn a b c

-- | @dafoldlM@ - monadic version of dafoldl.
dafoldlM :: (Monad m, Foldable t) => 
    (b -> c -> a -> m (b,c)) -> (b -> c -> m d) -> c -> b -> t a -> m d
dafoldlM f g c0 b0 ta  = uncurry g =<< foldlM (adapt f) (b0,c0) ta where 
    adapt fn = \(b,c) a -> fn b c a

-- | @dafoldrM@ - monadic version of dafoldr.
dafoldrM :: (Monad m, Foldable t) => 
    (a -> b -> c -> m (b,c)) -> (b -> c -> m d) -> c -> b -> t a -> m d
dafoldrM f g c0 b0 ta  = uncurry g =<< foldrM (adapt f) (b0,c0) ta where
    adapt fn = \a (b,c)-> fn a b c
    
    
     