{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  HNotate.Utils
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined
--
-- Common utils...
--
--------------------------------------------------------------------------------

module HNotate.Utils where

import qualified Data.Sequence as S
import Data.List ( unfoldr )

--------------------------------------------------------------------------------
-- HOF's

-- | Anamorphism - unfoldr.
ana :: (b -> Maybe (a,b)) -> b -> S.Seq a
ana f b0 = step (f b0) where
    step Nothing        = S.empty
    step (Just (a,st))  = a S.<| step (f st)
    


-- anaMap is the unfold analogue of accumMapL
-- we can signal exhaustion early by the Maybe type                
anaMap  :: (a -> st -> Maybe (b,st)) -> st -> [a] -> ([b],st) 
anaMap _ s0 []     = ([],s0)     
anaMap f s0 (x:xs) = case (f x s0) of
    Nothing       -> ([],s0)
    Just (a,st)   -> (a:as,b) where (as,b) = anaMap f st xs


unfoldr2 :: (s1 -> s2 -> Maybe (a,s1,s2)) -> s1 -> s2 -> [a]
unfoldr2 f s1 s2 = case f s1 s2 of
    Nothing     -> []
    Just (a,s1',s2') -> a : unfoldr2 f s1' s2'
    
        
-- Reverse application and composition

infixl 7 #

( # ) :: a -> (a -> b) -> b 
x # f = f x


infixl 7 #.

( #. ) :: (a -> b) -> (b -> c) -> (a -> c) 
g #. f = f . g


-- 'specs'

oo :: (c -> d) -> (a -> b -> c) -> a -> b -> d
oo f g = (f .) . g

ooo :: (d -> e) -> (a -> b -> c -> d) -> a -> b -> c -> e
ooo f g = ((f .) .) . g


--------------------------------------------------------------------------------
-- enum functions for cycles (primarily helpful for pitch letters)


enumFromCyc :: (Bounded a, Enum a, Eq a) => a -> [a]
enumFromCyc a = a : (unfoldr f $ nextOf a)
  where 
    f x | x == a    = Nothing
        | otherwise = Just (x,nextOf x)
    
enumFromToCyc :: (Bounded a, Enum a, Eq a) => a -> a -> [a]
enumFromToCyc a b | a == b    = [a]
                  | otherwise = a : (unfoldr f $ nextOf a) ++ [b]
  where 
    f x | x == a || x == b   = Nothing
        | otherwise          = Just (x,nextOf x)

nextOf :: (Bounded a, Eq a, Enum a) => a -> a  
nextOf x | x == maxBound = minBound
         | otherwise     = succ x



