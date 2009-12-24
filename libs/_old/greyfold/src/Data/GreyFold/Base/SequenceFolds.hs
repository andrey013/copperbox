{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  GreyFold.Base.SequenceFolds
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to establish
--
-- Various folds on Data.Sequence
--
--------------------------------------------------------------------------------

module GreyFold.Base.SequenceFolds (
  
  -- * foldleft and foldright
  foldLeft, foldRight,
  foldLeftM, foldRightM,

  -- * double folds
  doublefoldl, doublefoldr,
  doublefoldlM, doublefoldrM,

  -- * counting folds
  countfoldl, countfoldr,
  countfoldlM, countfoldrM,  
  
  -- * stateful folds
  statefoldl, statefoldr,
  statefoldlM, statefoldrM,
  
  -- * lookahead folds
  lookaheadfoldl, lookaheadfoldr,
  lookaheadfoldlM, lookaheadfoldrM,
  
  -- * double accumulator folds
  dafoldl, dafoldr,
  dafoldlM, dafoldrM,
  groupByRight, groupRight,
  
) where

import GreyFold.Base.Utils
import Data.Sequence

  

-- | @foldLeft@ - idiomatic definition of foldl.
foldLeft :: (b -> a -> b) -> b -> Seq a -> b
foldLeft f b0 se = step b0 (viewl se) where
    step b EmptyL     = b
    step b (a :< sa)  = step (f b a) (viewl sa)

-- | @foldRight@ - idiomatic definition of foldr.
foldRight :: (a -> b -> b) -> b -> Seq a -> b
foldRight f b0 se = step b0 (viewl se) where
    step b EmptyL     = b
    step b (a :< sa)  = f a (step b (viewl sa))

-- | Monadic foldl.
foldLeftM :: Monad m => (b -> a -> m b) -> b -> Seq a -> m b
foldLeftM f b0 se = step b0 (viewl se) where
    step b EmptyL     = return b
    step b (a :< sa)  = f b a >>= (step `flip` (viewl sa))
    
-- | Monadic definition of foldr.
foldRightM :: Monad m => (a -> b -> m b) -> b -> Seq a -> m b
foldRightM f b0 se = step b0 (viewl se) where
    step b EmptyL     = return b
    step b (a :< sa)  = f a =<< step b (viewl sa)
    

-- -----------------------------------------------------------------------------
-- double folds


-- | @doublefoldl@ - fold on two sequences at once, this generalizes @zipWith@
-- as the summary value no longer needs to be a sequence. 
doublefoldl :: (c -> a -> b -> c) -> c -> Seq a -> Seq b -> c
doublefoldl f c0 se se' = step c0 (viewl se) (viewl se') where
    step c (a :< sa) (b :< sb)  = step (f c a b) (viewl sa) (viewl sb)
    step c _         _          = c

-- | @doublefoldr@ - foldr version of doublefoldl.
doublefoldr :: (a -> b -> c -> c) -> c -> Seq a -> Seq b -> c
doublefoldr f c0 se se' = step c0 (viewl se) (viewl se') where
    step c (a :< sa) (b :< sb)  = f a b (step c (viewl sa) (viewl sb))
    step c _          _         = c

-- | @doublefoldlM@ - monadic version of doublefoldl.    
doublefoldlM :: Monad m => (c -> a -> b -> m c) -> c -> Seq a -> Seq b -> m c
doublefoldlM f c0 se se' = step c0 (viewl se) (viewl se') where
    step c (a :< sa) (b :< sb)  = f c a b >>= (adapt'bca step) (viewl sa) (viewl sb)
    step c _         _          = return c

-- | @doublefoldrM@ - monadic version of doublefoldr.
doublefoldrM :: Monad m => (a -> b -> c -> m c) -> c -> Seq a -> Seq b -> m c
doublefoldrM f c0 se se' = step c0 (viewl se) (viewl se') where
    step c (a :< sa) (b :< sb)  = f a b =<< step c (viewl sa) (viewl sb)
    step c _          _         = return c
    
-- -----------------------------------------------------------------------------
-- counting folds

-- | @countfoldl@ - a stateful fold where the count is incremented at each 
-- step. The initial count is supplied as a parameter, while this clutters 
-- the interface it does allow counting from 0,1 or any other Int.
countfoldl :: (b -> a -> Int -> b) -> Int -> b -> Seq a -> b
countfoldl f i0 b0 se = step i0 b0 (viewl se) where
    step _ b EmptyL         = b
    step i b (a :< sa)      = step (i+1) (f b a i) (viewl sa) 

-- | @countfoldr@ - right to left version of countfoldl.
countfoldr :: (a -> b -> Int -> b) -> Int -> b -> Seq a -> b
countfoldr f i0 b0 se = step i0 b0 (viewl se) where
    step _ b EmptyL         = b
    step i b (a :< sa)      = f a (step (i+1) b (viewl sa)) i 
    
-- | @countfoldlM@ - monadic version of countfoldl.
countfoldlM :: Monad m => (b -> a -> Int -> m b) -> Int -> b -> Seq a -> m b
countfoldlM f i0 b0 se = step i0 b0 (viewl se) where
    step _ b EmptyL         = return b
    step i b (a :< sa)      = (adapt'acb step) (i+1) (viewl sa) =<< f b a i  

-- | @countfoldrM@ - monadic version of countfoldr.
countfoldrM :: Monad m => (a -> b -> Int -> m b) -> Int -> b -> Seq a -> m b
countfoldrM f i0 b0 se = step i0 b0 (viewl se) where
    step _ b EmptyL         = return b
    step i b (a :< sa)      = (adapt'acb f) a i =<< step (i+1) b (viewl sa) 

-- -----------------------------------------------------------------------------
-- stateful folds

-- | @statefoldl@ - a generalization of countfoldl, the state is accessible 
-- to the reduction function, and the final state is returned with the 
-- summary value.  
statefoldl :: (b -> a -> st -> (b,st)) -> st -> b -> Seq a -> (b,st) 
statefoldl f st0 b0 se = step st0 b0 (viewl se) where
    step st b EmptyL      = (b,st)
    step st b (a :< sa)   = step st' b' (viewl sa) where (b',st') = f b a st

-- | @statefoldr@ - right to left version of statefoldl.  
statefoldr :: (a -> b -> st -> (b,st)) -> st -> b -> Seq a -> (b,st) 
statefoldr f st0 b0 se = step st0 b0 (viewl se) where
    step st b EmptyL      = (b,st)
    step st b (a :< sa)   = f a b' st' where (b',st') = step st b (viewl sa)

-- | @statefoldlM@ - monadic version of statefoldl. 
statefoldlM :: Monad m => 
    (b -> a -> st -> m (b,st)) -> st -> b -> Seq a -> m (b,st) 
statefoldlM f st0 b0 se = step st0 b0 (viewl se) where
    step st b EmptyL      = return (b,st)
    step st b (a :< sa)   = (adapt'ctba step) (viewl sa) =<< f b a st

-- | @statefoldrM@ - monadic version of statefoldr.  
statefoldrM :: Monad m => 
    (a -> b -> st -> m (b,st)) -> st -> b -> Seq a -> m (b,st) 
statefoldrM f st0 b0 se = step st0 b0 (viewl se) where
    step st b EmptyL      = return (b,st)
    step st b (a :< sa)   = (adapt'atbc f) a =<< step st b (viewl sa)

-- -----------------------------------------------------------------------------
-- lookahead folds

-- | @lookaheadfoldl@ - foldl with lookahead. The remaining sequence
-- is visible and mutable to the reduction function. One outcome 
-- of this is that the reduction function can set the remaining 
-- list to empty, which short-circuits the traversal.  
lookaheadfoldl :: (b -> a -> Seq a -> (b, Seq a)) -> b -> Seq a -> b
lookaheadfoldl f b0 se = step b0 (viewl se) where
    step b EmptyL       = b
    step b (a :< sa)    = step b' (viewl sa') where (b',sa') = f b a sa 

-- | @lookaheadfoldr@ - right to left version of lookaheadfoldl.
-- Note the the definition is exactly the same as lookaheadfoldl,
-- except we can use viewr to go backwards.
lookaheadfoldr :: (a -> b -> Seq a -> (b, Seq a)) -> b -> Seq a -> b
lookaheadfoldr f b0 se = step b0 (viewr se) where
    step b EmptyR       = b
    step b (sa :> a)    = step b' (viewr sa') where (b',sa') = f a b sa  
    
    
-- | @lookaheadfoldlM@ - monadic version of lookaheadfoldlM. 
lookaheadfoldlM :: Monad m => 
    (b -> a -> Seq a -> m (b, Seq a)) -> b -> Seq a -> m b
lookaheadfoldlM f b0 se = step b0 (viewl se) where
    step b EmptyL       = return b
    step b (a :< sa)    = (\(b',sa') -> step b' (viewl sa')) =<< f b a sa 

-- | @lookaheadfoldrM@ - monadic version of lookaheadfoldrM. 
lookaheadfoldrM :: Monad m => 
    (b -> a -> Seq a -> m (b, Seq a)) -> b -> Seq a -> m b
lookaheadfoldrM f b0 se = step b0 (viewr se) where
    step b EmptyR       = return b
    step b (sa :> a)    = (\(b',sa') -> step b' (viewr sa')) =<< f b a sa
     
-- -----------------------------------------------------------------------------
-- double accumulator folds


-- | @dafoldl@ - a fold with two accumulators.
-- Double accumulator folds are quite handy when building a sequences 
-- of sequences - vis-a-vis pointed sets. 
--
-- Consider @b@ to be the point (type @Seq x@) and @c@ to be set 
-- (type @Seq (Seq x)@). The reducer can choose whether to put x in the 
-- point, put x in a new point and move the old point into the set, etc.
-- I've used a dafold in music processing to segment notes within a 
-- bar into beam groups. As sequence supports /snoc/-ing with the |> operator, 
-- doing this left-to-right much more tangible.
dafoldl :: (b -> c -> a -> (b,c)) -> (b -> c -> d) -> c -> b -> Seq a -> d
dafoldl f g c0 b0 se  = step (b0,c0) (viewl se) where
    step (b,c) EmptyL     = g b c
    step (b,c) (a :< sa)  = step (f b c a) (viewl sa)   
                   
-- | @dafoldr@ - right to left version of dafoldl.
-- This is simpler than the list version as we can simply go backwards
-- with viewr.
dafoldr :: (a -> b -> c -> (b,c)) -> (b -> c -> d) -> c -> b -> Seq a -> d
dafoldr f g c0 b0 se  = step (b0,c0) (viewr se) where
    step (b,c) EmptyR     = g b c
    step (b,c) (sa :> a)  = step (f a b c) (viewr sa) 

-- | @dafoldlM@ - monadic version of dafoldl.
dafoldlM :: Monad m =>
    (b -> c -> a -> m (b,c)) -> (b -> c -> m d) -> c -> b -> Seq a -> m d
dafoldlM f g c0 b0 se  = step (b0,c0) (viewl se) where
    step (b,c) EmptyL     = g b c
    step (b,c) (a :< sa)  = (step `flip` (viewl sa)) =<< f b c a     

-- | @dafoldrM@ - monadic version of dafoldr.
dafoldrM :: Monad m =>
    (a -> b -> c -> m (b,c)) -> (b -> c -> m d) -> c -> b -> Seq a -> m d
dafoldrM f g c0 b0 se  = step (b0,c0) (viewr se) where
    step (b,c) EmptyR     = g b c
    step (b,c) (sa :> a)  = (step `flip` (viewr sa)) =<< f a b c  
        

-- | @groupByRight@ - the list function @groupBy@ implemented with dafoldr.
groupByRight :: Eq a => (a -> a -> Bool) -> Seq a -> Seq (Seq a)
groupByRight p ls = dafoldr fn (<|) empty empty ls where
    fn a sb     ssc = step a (viewl sb) ssc
    
    step a EmptyL     ssc               = (singleton a,  ssc)
    step a (b :< sb)  ssc   | p a b     = (a <| b <| sb, ssc)
                            | otherwise = (singleton a,  (b <| sb) <| ssc)

  
-- | @groupRight@ - the list function @group@ defined with groupByRight.
groupRight :: Eq a => Seq a -> Seq (Seq a)
groupRight = groupByRight (==)

    
                 