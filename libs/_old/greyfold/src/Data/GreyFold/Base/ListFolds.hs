{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  GreyFold.Base.ListFolds
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to establish
--
-- Various folds on lists
--
--------------------------------------------------------------------------------


module GreyFold.Base.ListFolds (
  -- * Convensions
  -- $conventions 
  
  -- * foldLeft and foldRight
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
  lookaheadfoldl, lookbackfoldr,
  lookaheadfoldlM, lookbackfoldrM,
  
  -- * double accumulator folds
  dafoldl, dafoldr,
  dafoldlM, dafoldrM,
  groupByRight, groupRight,
  
  
) where

import GreyFold.Base.Utils

-- $conventions
-- The list folds are written with translation to Data.Sequence 
-- in mind. As Sequence has pattern matching on views and not the
-- Seq type itself, the list folds generally pattern match in the 
-- step function. 
--
-- To avoid GHC shadow binding warnings, the top line of the 
-- function definition follows one naming scheme and the step
-- function another. In the top line list arguments are ls, ls', etc.  
-- and initial values are are e.g. b0, c0, etc. the b or c corresponds 
-- to the variable in the type signature the zero indicates it
-- is the initial value. In the stepping function the naming is
-- more familiar b is generally the summary value and lists are 
-- generally (a:as) lists of lists are xss, css, etc.
-- Also, I generally avoid partial application in the top line definition.   
--
-- Finally I use the /Gibbons style/ for type variable letters, see 
-- Jeremy Gibbons /origami/ papers - this is the opposite of
-- the Haskell Prelude style. In the /Gibbons style/ the consumed 
-- list is @[alpha]@ (here @[a]@) and the produced summary value 
-- is @beta@ (here @b@).
--
-- @foldrRight@ and @foldLeft@ are redundantly defined to 
-- illustrate these conventions.  

-- | @foldLeft@ - idiomatic definition of foldl.
foldLeft :: (b -> a -> b) -> b -> [a] -> b
foldLeft f b0 ls = step b0 ls where
    step b []         = b
    step b (a:as)     = step (f b a) as
    
-- | @foldRight@ - idiomatic definition of foldr.
foldRight :: (a -> b -> b) -> b -> [a] -> b
foldRight f b0 ls = step b0 ls where
    step b []         = b
    step b (a:as)     = f a (step b as)


-- | @foldLeftM@ - monadic version of foldLeft. 
foldLeftM :: Monad m => (b -> a -> m b) -> b -> [a] -> m b
foldLeftM f b0 ls = step b0 ls where
    step b []         = return b
    step b (a:as)     = f b a >>= (step `flip` as)
    
-- | @foldRightM@ - monadic version of foldRight. 
foldRightM :: Monad m => (a -> b -> m b) -> b -> [a] -> m b
foldRightM f b0 ls = step b0 ls where
    step b []         = return b
    step b (a:as)     = f a =<< step b as
                           
    

-- -----------------------------------------------------------------------------
-- double folds
    
-- | @doublefoldl@ - fold on two lists at once, this generalizes @zipWith@
-- as the summary value does not have to be a list. 
doublefoldl :: (c -> a -> b -> c) -> c -> [a] -> [b] -> c
doublefoldl f c0 ls ls' = step c0 ls ls' where
    step c (a:as) (b:bs) = step (f c a b) as bs
    step c _      _      = c

-- | @doublefoldr@ - foldr version of doublefoldl.
doublefoldr :: (a -> b -> c -> c) -> c -> [a] -> [b] -> c
doublefoldr f c0 ls ls' = step c0 ls ls' where
    step c (a:as) (b:bs) = f a b (step c as bs)
    step c _      _      = c

-- | @doublefoldlM@ - monadic version of doublefoldl.
doublefoldlM :: Monad m => (c -> a -> b -> m c) -> c -> [a] -> [b] -> m c
doublefoldlM f c0 ls ls' = step c0 ls ls' where
    step c (a:as) (b:bs) = f c a b >>= (adapt'bca step) as bs
    step c _      _      = return c

-- | @doublefoldrM@ - monadic version of doublefoldr.
doublefoldrM :: Monad m => (a -> b -> c -> m c) -> c -> [a] -> [b] -> m c
doublefoldrM f c0 ls ls' = step c0 ls ls' where
    step c (a:as) (b:bs) = f a b =<< step c as bs
    step c _      _      = return c
    
        

 
    
-- -----------------------------------------------------------------------------
-- counting folds

-- | @countfoldl@ - a stateful fold where the count is incremented at each 
-- step. The initial count is supplied as a parameter, while this clutters 
-- the interface it does allow counting from 0,1 or any other Int.
countfoldl :: (b -> a -> Int -> b) -> Int -> b -> [a] -> b
countfoldl f i0 b0 ls = step i0 b0 ls where
    step _ b []     = b
    step i b (a:as) = step (i+1) (f b a i) as 

-- | @countfoldr@ - right to left version of countfoldl.
countfoldr :: (a -> b -> Int -> b) -> Int -> b -> [a] -> b
countfoldr f i0 b0 ls = step i0 b0 ls where
    step _ b []     = b
    step i b (a:as) = f a (step (i+1) b as) i 

-- | @countfoldlM@ - monadic version of countfoldl.  
countfoldlM :: Monad m => (b -> a -> Int -> m b) -> Int -> b -> [a] -> m b
countfoldlM f i0 b0 ls = step i0 b0 ls where
    step _ b []     = return b
    step i b (a:as) = (adapt'acb step) (i+1) as =<< f b a i 

-- | @countfoldrM@ - monadic version of countfoldr.
countfoldrM :: Monad m => (a -> b -> Int -> m b) -> Int -> b -> [a] -> m b
countfoldrM f i0 b0 ls = step i0 b0 ls where
    step _ b []     = return b
    step i b (a:as) = (adapt'acb f) a i =<< step (i+1) b as
    
      
-- -----------------------------------------------------------------------------
-- stateful folds

-- | @statefoldl@ - a generalization of countfoldl, the state is accessible 
-- to the reduction function, and the final state is returned with the 
-- summary value.  
statefoldl :: (b -> a -> st -> (b,st)) -> st -> b -> [a] -> (b,st) 
statefoldl f st0 b0 ls = step st0 b0 ls where
    step st b []     = (b,st)
    step st b (a:as) = step st' b' as where (b',st') = f b a st

-- | @statefoldr@ - right to left version of statefoldl.  
statefoldr :: (a -> b -> st -> (b,st)) -> st -> b -> [a] -> (b,st) 
statefoldr f st0 b0 ls = step st0 b0 ls where
    step st b []     = (b,st)
    step st b (a:as) = f a b' st' where (b',st') = step st b as

-- | @statefoldlM@ - monadic version of statefoldl. 
statefoldlM :: Monad m => (b -> a -> st -> m (b,st)) -> st -> b -> [a] -> m (b,st) 
statefoldlM f st0 b0 ls = step st0 b0 ls where
    step st b []     = return (b,st)
    step st b (a:as) = (adapt'ctba step) as =<< f b a st

-- | @statefoldrM@ - monadic version of statefoldr.  
statefoldrM :: Monad m => (a -> b -> st -> m (b,st)) -> st -> b -> [a] -> m (b,st) 
statefoldrM f st0 b0 ls = step st0 b0 ls where
    step st b []     = return (b,st)
    step st b (a:as) = (adapt'atbc f) a =<< step st b as
  
  
-- -----------------------------------------------------------------------------
-- lookahead folds

-- | @lookaheadfoldl@ - foldl with lookahead. The remaining list
-- is visible and mutable to the reduction function. One outcome 
-- of this is that the reduction function can set the remaining 
-- list to [], which short-circuits the traversal.  
lookaheadfoldl :: (b -> a -> [a] -> (b, [a])) -> b -> [a] -> b
lookaheadfoldl f b0 ls = step b0 ls where
    step b []     = b
    step b (a:as) = step b' as' where (b',as') = f b a as 


-- | @lookbackfoldr@ - this /is not/ the foldr version of lookaheadfoldl.
-- I\'m not sure how to properly define it for list, though there is a 
-- definition for @Seq@ in SequenceFolds (an /improper/ implementation
-- would reverse the list first then use lookaheadfoldl, @Seq@ has 
-- backwards travel built in, so it can do it one traversal).
-- The problem is that reducer of a lookahead fold should see the 
-- /rest-of-the-list/, but a reducer of a rightfold should see the values 
-- produced by the right-to-left /unwind/ (usually just the summary value).
-- Instead @lookbackfoldr@\'s reducer looks at the values it has already
-- produced - if it set the list to [] it would censor its own state, not
-- escape its input. 
lookbackfoldr :: (a -> b -> [a] -> (b, [a])) -> b -> [a] -> b
lookbackfoldr f b0 ls = fst $ step b0 ls where
    step b []     = (b,[])
    step b (a:as) = f a b' as' where (b',as') = step b as 



-- | @lookaheadfoldlM@ - monadic version of lookaheadfoldlM. 
lookaheadfoldlM :: Monad m => (b -> a -> [a] -> m (b, [a])) -> b -> [a] -> m b
lookaheadfoldlM f b0 ls = step b0 ls where
    step b []     = return b
    step b (a:as) = (uncurry step) =<< f b a as 


-- | @lookbackfoldr@ - monadic version of lookbackfoldr.    
lookbackfoldrM ::  Monad m => (a -> b -> [a] -> m (b, [a])) -> b -> [a] -> m b
lookbackfoldrM f b0 ls = (return . fst) =<< step b0 ls where
    step b []     = return (b,[])
    step b (a:as) = (adapt'atbc f) a =<< step b as 

   
-- -----------------------------------------------------------------------------
-- double accumulator folds


-- | @dafoldl@ - a fold with two accumulators.
-- Double accumulator folds are quite handy when building a list of lists -
-- vis-a-vis pointed sets.
-- 
-- Consider @b@ to be the point (type @[x]@) and @c@ to be set 
-- (type @[[x]]@). The reducer can choose whether to put x in the 
-- point, put x in a new point and move the old point into the set, etc.
-- I've used a dafold in music processing to segment notes within a 
-- bar into beam groups, though I prefer operating on sequences rather
-- than lists - sequences can be built with /snoc/-ing, so left to 
-- right traversal is much more tangible.
dafoldl :: (b -> c -> a -> (b,c)) -> (b -> c -> d) -> c -> b -> [a] -> d
dafoldl f g c0 b0 ls  = step (b0,c0) ls where
    step (b,c) []       = g b c
    step (b,c) (a:as)   = step (f b c a) as   
                   
-- | @dafoldr@ - right to left version of dafoldl.
dafoldr :: (a -> b -> c -> (b,c)) -> (b -> c -> d) -> c -> b -> [a] -> d
dafoldr f g c0 b0 ls  = uncurry g $ step (b0,c0) ls where
    step (b,c) []       = (b,c)
    step (b,c) (a:as)   = f a b' c' where (b',c') = step (b,c) as

-- | @dafoldlM@ - monadic version of dafoldl.
dafoldlM :: Monad m => 
    (b -> c -> a -> m (b,c)) -> (b -> c -> m d) -> c -> b -> [a] -> m d
dafoldlM f g c0 b0 ls  = step (b0,c0) ls where
    step (b,c) []       = g b c
    step (b,c) (a:as)   = (step `flip` as) =<< f b c a   
                   
-- | @dafoldrM@ - monadic version of dafoldr.
dafoldrM :: Monad m => 
    (a -> b -> c -> m (b,c)) -> (b -> c -> m d) -> c -> b -> [a] -> m d
dafoldrM f g c0 b0 ls  = (uncurry g) =<< step (b0,c0) ls where
    step (b,c) []       = return (b,c)
    step (b,c) (a:as)   = (adapt'atbc f) a =<< step (b,c) as
  
  
-- | @groupByRight@ - the list function @groupBy@ implemented with dafoldr.
groupByRight :: Eq a => (a -> a -> Bool) -> [a] -> [[a]]
groupByRight p ls = dafoldr fn (:) [] [] ls where
    fn a []     ccs             = ([a],ccs)
    fn a (b:bs) ccs    | p a b  = (a:b:bs,ccs)
    fn a bs     ccs             = ([a],bs:ccs)
  
-- | @groupRight@ - the list function @group@ defined with groupByRight.
groupRight :: Eq a => [a] -> [[a]]
groupRight = groupByRight (==)
  