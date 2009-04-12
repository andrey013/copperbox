{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Mullein.Utils
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined
--
-- Common utils...
--
--------------------------------------------------------------------------------

module Mullein.Utils where

import Data.List ( unfoldr )
import Data.Ratio

import Text.PrettyPrint.Leijen


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
    


-- 'specs'

oo :: (c -> d) -> (a -> b -> c) -> a -> b -> d
oo f g = (f .) . g

ooo :: (d -> e) -> (a -> b -> c -> d) -> a -> b -> c -> e
ooo f g = ((f .) .) . g

    
    
-- Reverse application and composition

infixl 7 #

( # ) :: a -> (a -> b) -> b 
x # f = f x


infixl 7 ##

( ## ) :: (a -> b) -> (b -> c) -> (a -> c) 
g ## f = f . g


enumFromCyc :: (Bounded a, Enum a, Eq a) => a -> [a]
enumFromCyc a = a : (unfoldr f $ nextOf a)
  where 
    f x | x == a    = Nothing
        | otherwise = Just (x,nextOf x)
        
nextOf :: (Bounded a, Eq a, Enum a) => a -> a  
nextOf x | x == maxBound = minBound
         | otherwise     = succ x
                 
rational :: Integral a => a -> a -> Rational
rational a b = fromIntegral a % fromIntegral b

---------------------------------------------------------------------------------
-- PPrint extras 

-- This function is primarily for Abc bar printing, where the number
-- of bars on a line in the input score is reflected by the number of
-- bars on a line in the output.

doclines :: [Int] -> [Doc] -> Doc
doclines = vsep `oo` step where
    step _      []  = []
    step []     ds  = [hsep ds]
    step (n:ns) ds  = hsep ls : step ns rs where (ls,rs) = splitAt n ds

dblangles :: Doc -> Doc 
dblangles = enclose (text "<<") (text ">>")
