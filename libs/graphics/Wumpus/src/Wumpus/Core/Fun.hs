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


module Wumpus.Core.Fun where


-- Pairs

fork :: (a -> b, a -> c) -> a -> (b,c)
fork (f,g) a = (f a, g a)

prod :: (a -> c) -> (b -> d) -> (a,b) -> (c,d)
prod f g (a,b) = (f a, g b)

both :: (a -> b) -> (a,a) -> (b,b)
both f (a,b) = (f a, f b)

exch :: (a,b) -> (b,a)
exch (a,b) = (b,a)

outer :: (a,b) -> (c,d) -> (a,d)
outer (a,_) (_,d) = (a,d)

inner :: (a,b) -> (c,d) -> (b,c)
inner (_,b) (c,_) = (b,c)


ifpair :: (a -> b -> Bool) -> (a -> c) -> (b -> c) -> (a,b) -> c 
ifpair p tk ek (a,b) | p a b     = tk a
                     | otherwise = ek b

-- type restrcicted to homogenous pairs
cond :: (a -> a -> Bool) -> (a,a) -> a
cond p (a,b) | p a b     = a
             | otherwise = b

-- apply :: (a -> b -> c) -> (a,b) -> c
-- apply is uncurry


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



-- homogeneous long zipWith
lZipWith :: (a -> a -> a) -> [a] -> [a] -> [a]
lZipWith _ []     qs     = qs
lZipWith _ ps     []     = ps
lZipWith f (p:ps) (q:qs) = f p q : lZipWith f ps qs 



