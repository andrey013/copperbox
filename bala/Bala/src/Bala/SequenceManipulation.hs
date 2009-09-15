{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.SequenceManipulation
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Sequence manipulation
--
--------------------------------------------------------------------------------

module Bala.SequenceManipulation
  ( 
  -- * Rotate
    ( <<& )
  , ( &>> )

  -- * Duplicate elements
  , ntimes

  -- * Delete elements
  , nthdelete


  -- * Transform part of a sequence
  , after
  , upto

  , interleave

  ) where


import Prelude hiding ( head, tail, zipWith )


-- import Data.Stream ( Stream, head, tail, zipWith )
-- import qualified Data.Stream as S

-- import Data.Stream.Hinze.Stream ( (<:) )
-- import qualified Data.Stream.Hinze.Stream as HS

infixl 4 <<&, &>>

-- | Rotate n times to the left.
(<<&) :: [a] -> Int -> [a]
(<<&) ss n = ys++xs where (xs,ys) = splitAt (n `mod` (length ss)) ss



-- | Rotate n times to the right.
(&>>) :: [a] -> Int -> [a]
(&>>) ss n = ys++xs where
   len     = length ss
   (xs,ys) = splitAt (len - (n `mod` len)) ss  


-- | Duplicate every element n times.
ntimes :: Int -> [a] -> [a]
ntimes n (x:xs) = replicate n x ++ ntimes n xs
ntimes _ []     = []

-- | Delete every nth element.
-- n==1 deletes everything
-- n==2 [1..10] = [1,3,4,5,7,9]
nthdelete :: Int -> [a] -> [a]
nthdelete n ss = step (n-1) ss where
  step i (x:xs) | i <= 0    = step (n-1) xs
                | otherwise = x : step (i-1) xs
  step _ []                 = []


-- | Take an initial prologue of n elements then run the 
-- transformation on the remaining sequence.
after :: Int -> ([a] -> [a]) -> [a] -> [a]
after n f ss = xs ++ f ys where (xs,ys) = splitAt n ss

upto :: Int -> ([a] -> [a]) -> [a] -> [a]
upto n f ss = f xs ++ ys where (xs,ys) = splitAt n ss


-- interleave

interleave :: Int -> [a] -> Int -> [a] -> [a]
interleave _ [] _ ys = ys
interleave _ xs _ [] = xs  
interleave i xs j ys = h ++ interleave j ys i t where (h,t) = splitAt i xs
