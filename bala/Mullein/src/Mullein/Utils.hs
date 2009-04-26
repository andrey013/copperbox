{-# OPTIONS -Wall #-}
{-# OPTIONS -fno-warn-orphans #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Mullein.Utils
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined
--
-- Common utils...
--
--------------------------------------------------------------------------------

module Mullein.Utils where

import Control.Applicative ( Applicative(..) )
import Control.Monad.State
import Data.List ( unfoldr )
import Data.Ratio

import Text.PrettyPrint.Leijen

instance Applicative (State s) where
  pure  = return
  (<*>) = ap

-- special zipWith that zips all of the first list
overlayZipWith :: (a -> b -> c) -> (a -> c) -> [a] -> [b] -> [c]
overlayZipWith f g (x:xs) (y:ys) = f x y : overlayZipWith f g xs ys
overlayZipWith f g (x:xs) []     = g x   : overlayZipWith f g xs []
overlayZipWith _ _ []     _      = []


longZipWith :: (a -> b -> c) -> (a -> c) -> (b -> c) -> [a] -> [b] -> [c]
longZipWith f g h as bs = step as bs where
    step (x:xs) (y:ys) = f x y : step xs ys
    step (x:xs) []     = g x : step xs []
    step []     (y:ys) = h y : step [] ys
    step []     []     = []
 

-- anaMap is the unfold analogue of accumMapL
-- we can signal exhaustion early by the Maybe type                
anaMap  :: (a -> st -> Maybe (b,st)) -> st -> [a] -> ([b],st) 
anaMap _ s0 []     = ([],s0)     
anaMap f s0 (x:xs) = case (f x s0) of
    Nothing       -> ([],s0)
    Just (a,st)   -> (a:as,b) where (as,b) = anaMap f st xs


-- variant of /apomorphism/, but we return the final state 
-- rather than running a flush function on it
anaSt :: (st -> Maybe (a,st)) -> st -> ([a],st)
anaSt f s0 = case (f s0) of
    Nothing     -> ([],s0)
    Just (a,st) -> (a:as,b) where (as,b) = anaSt f st 


unfoldr2 :: (s1 -> s2 -> Maybe (a,s1,s2)) -> s1 -> s2 -> [a]
unfoldr2 f s1 s2 = case f s1 s2 of
    Nothing     -> []
    Just (a,s1',s2') -> a : unfoldr2 f s1' s2'
    


-- 'specs'

oo :: (c -> d) -> (a -> b -> c) -> a -> b -> d
oo f g = (f .) . g

ooo :: (d -> e) -> (a -> b -> c -> d) -> a -> b -> c -> e
ooo f g = ((f .) .) . g

oooo :: (e -> f) -> (a -> b -> c -> d -> e) -> a -> b -> c -> d -> f
oooo f g = (((f .) .) .) . g    
    
-- Reverse application and composition

infixl 7 #

( # ) :: a -> (a -> b) -> b 
x # f = f x


infixl 7 ##

( ## ) :: (a -> b) -> (b -> c) -> (a -> c) 
g ## f = f . g

prod :: (a -> c) -> (b -> d) -> (a,b) -> (c,d) 
prod f g (a,b) = (f a,g b)

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


-- an alternative to (<$>) when Control.Applicative is alos imported
infixr 5 `nextLine`
nextLine :: Doc -> Doc -> Doc 
nextLine = (<$>)
