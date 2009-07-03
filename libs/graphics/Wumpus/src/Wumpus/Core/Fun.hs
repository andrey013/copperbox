{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Core.Fun
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Functional things
--
--------------------------------------------------------------------------------


module Wumpus.Core.Fun 
  ( 
  -- * Pairs
    fork
  , prod
  , both
  , exch
  , outer
  , inner
  , cond

  -- * Three values  
  , max3
  , min3
  , med3

  -- * Functionals
  , unfoldrMap
  , unfoldlMap
  , intermap
  , intermap3
  , windowedMap2c
  , windowedMap3c
  , windowedFoldR2c
  , windowedFoldL2c
  , lZipWith
  
  -- * Index generation
  , steps
  , divisions

  -- * Composition with specs
  , oo
  , ooo
  , oooo

  ) where

import Data.List ( unfoldr )

-- Pairs

-- | Apply the pair of functions to the same argument returning the 
-- pair of answers.
fork :: (a -> b, a -> c) -> a -> (b,c)
fork (f,g) a = (f a, g a)

-- | Apply the first function to the first argument and the second 
-- function to the second, return the pair of answers. 
prod :: (a -> c) -> (b -> d) -> (a,b) -> (c,d)
prod f g (a,b) = (f a, g b)

-- | Apply the function to both elements of the pair.
both :: (a -> b) -> (a,a) -> (b,b)
both f (a,b) = (f a, f b)

-- | Swap the elements of the pair.
exch :: (a,b) -> (b,a)
exch (a,b) = (b,a)

-- | Return the /outer/ elements of the argument pairs.
outer :: (a,b) -> (c,d) -> (a,d)
outer (a,_) (_,d) = (a,d)

-- | Return the /inner/ elements of the argument pairs.
inner :: (a,b) -> (c,d) -> (b,c)
inner (_,b) (c,_) = (b,c)


-- | Apply the predicate to the pair, if true return the first element
-- if false retur the second.
cond :: (a -> a -> Bool) -> (a,a) -> a
cond p (a,b) | p a b     = a
             | otherwise = b

-- apply :: (a -> b -> c) -> (a,b) -> c
-- apply is uncurry


-- | max of 3
max3 :: Ord a => a -> a -> a -> a
max3 a b c = max (max a b) c

-- | min of 3
min3 :: Ord a => a -> a -> a -> a
min3 a b c = min (min a b) c


-- | median of 3
med3 :: Ord a => a -> a -> a -> a
med3 a b c = if c <= x then x else if c > y then y else c
  where 
    (x,y)                 = order a b
    order p q | p <= q    = (p,q)
              | otherwise = (q,p)



---- more functionals

-- unfoldrMap is the unfold analogue of accumMapR
-- we can signal exhaustion early by the Maybe type                
unfoldrMap  :: (a -> st -> Maybe (b,st)) -> st -> [a] -> ([b],st) 
unfoldrMap _ s0 []     = ([],s0)     
unfoldrMap f s0 (x:xs) = case (f x s0) of
    Nothing       -> ([],s0)
    Just (a,st)   -> (a:as,b) where (as,b) = unfoldrMap f st xs


-- unfoldrMap is the unfold analogue of accumMapL
-- we can signal exhaustion early by the Maybe type                
unfoldlMap  :: (a -> st -> Maybe (b,st)) -> st -> [a] -> ([b],st) 
unfoldlMap _ s0 []     = ([],s0)     
unfoldlMap f s0 (x:xs) = let (acc,st) = unfoldlMap f s0 xs in 
                         case (f x st) of 
                           Nothing -> ([],st)
                           Just (a,st_final) -> (a:acc,st_final)   


-- surely this one has been /discovered/ many times?
intermap :: (a -> a -> b) -> [a] -> [b]
intermap f (a:b:xs) = f a b : intermap f (b:xs)
intermap _ _        = []


intermap3 :: (a -> a -> a -> b) -> [a] -> [b]
intermap3 f (a:b:c:xs) = f a b c : intermap3 f (b:c:xs)
intermap3 _ _          = []


-- alternatively ...


-- | windowed map, window size is 2 and start point is cycled.
windowedMap2c :: (a -> a -> b) -> [a] -> [b]
windowedMap2c f (a:b:xs) = step (a:b:xs) where
  step [z]      = [f z a]
  step (x:y:zs) = f x y : step (y:zs)
  step []       = error $ "windowedMap2c: unreachable"
windowedMap2c _ _        = error $ "windowedMap2c: list must have at least 2 elements"


-- | windowed map, window size is 3 and 2 start points are cycled.
windowedMap3c :: (a -> a -> a -> b) -> [a] -> [b]
windowedMap3c f (a:b:xs) = step (a:b:xs) where
  step [y,z]      = [f y z a, f z a b] -- cycle 2
  step (x:y:z:zs) = f x y z : step (y:z:zs)
  step _         = error $ "windowedMap3c: unreachable"
windowedMap3c _ _        = error $ "windowedMap3c: list must have at least 2 elements"





-- | windowed fold, window size is 2 and start point is cycled.
windowedFoldR2c :: (a -> a -> b -> b) -> b -> [a] -> b
windowedFoldR2c f b0 (a:b:xs) = step b0 (a:b:xs)  where
  step ac [z]       = f z a ac
  step ac (x:y:zs)  = f x y (step ac (y:zs))
  step _  []        = error $ "windowedFoldR2c: unreachable"
windowedFoldR2c _ _  _        = error $ "windowedFoldR2c: list must have at least 2 elements"


-- | windowed fold, window size is 2 and start point is cycled.
windowedFoldL2c :: (b -> a -> a -> b) -> b -> [a] -> b
windowedFoldL2c f b0 (a:b:xs) = step b0 (a:b:xs)  where
  step ac [z]       = f ac z a
  step ac (x:y:zs)  = step (f ac x y) (y:zs)
  step _  []        = error $ "windowedFoldL2c: unreachable"
windowedFoldL2c _ _  _        = error $ "windowedFoldL2c: list must have at least 2 elements"


-- | Homogeneous long zipWith.
lZipWith :: (a -> a -> a) -> [a] -> [a] -> [a]
lZipWith _ []     qs     = qs
lZipWith _ ps     []     = ps
lZipWith f (p:ps) (q:qs) = f p q : lZipWith f ps qs 


-- | build a list of @i@ divisions of @a@.
divisions :: (RealFrac a, Ord a) => Int -> a -> [a]
divisions i a = take i $ iterate (+j) 0 where
  j = a / realToFrac i


-- | build a list of by enumerating @i@ steps upto value @a@.
steps :: (Num a, Ord a) => Int -> a -> [a]
steps i a = unfoldr phi i' where
  phi x | x < a     = Just (x,x+i')
        | otherwise = Nothing
  i' = fromIntegral i



-- 'specs'

oo :: (c -> d) -> (a -> b -> c) -> a -> b -> d
oo f g = (f .) . g

ooo :: (d -> e) -> (a -> b -> c -> d) -> a -> b -> c -> e
ooo f g = ((f .) .) . g

oooo :: (e -> f) -> (a -> b -> c -> d -> e) -> a -> b -> c -> d -> f
oooo f g = (((f .) .) .) . g   




