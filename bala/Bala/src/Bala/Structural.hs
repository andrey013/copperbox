{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Structural
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Build scores...
--
--------------------------------------------------------------------------------

module Bala.Structural
  ( 
  -- * fret diagrams
    FretDiagramPattern
  , fretpic


  -- *** ...
  , string1
  , string2
  , strings

  , rootFifth
  , rootSeventh
  , bass2
  , high3
  
  -- * Join lists
  , alternate
  , twice

  , aaba
  , abba
  , cycdist
  
  
  , (~*)

  -- * List transformations
  , ( <<& )
  , ( &>> )
  , ntimes
  , nthdelete
  , after
  , upto
  , interleave

  ) where

import Bala.BeatPattern
import Bala.Chord
import Bala.Pitch
import Bala.Mullein
import Bala.Mullein.LilyPond
import Bala.Utils


import Mullein.Core
import Mullein.Extended

import qualified Mullein.Duration as M

import Data.JoinList ( JoinList, wrap, join, fromList )
import qualified Data.JoinList as JL

--------------------------------------------------------------------------------

type FretDiagramPattern = MetricalPattern FretDiagramAlias

fretpic :: FretDiagramAlias -> Multiplier -> FretDiagramPattern
fretpic name i = MetricalPattern i (wrap $ Nb i name)


--------------------------------------------------------------------------------


type GlyphF anno = M.Duration -> StdGlyph anno


unpair :: (a,a) -> JoinList a
unpair = (uncurry join) . dist wrap

anno1 :: a -> Pitch -> GlyphF a
anno1 = flip $ makeNote . toPitch

anno2 :: (a,a) -> (Pitch,Pitch) -> (GlyphF a, GlyphF a)
anno2 = (uncurry prod) . dist anno1

annos :: [a] -> [Pitch] -> [GlyphF a]
annos = matchZipWith anno1


string1 :: StringNumber -> Pitch -> JoinList (GlyphF StringNumber)
string1 = wrap `oo` anno1

string2 :: (StringNumber,StringNumber) 
        -> (Pitch,Pitch) 
        -> JoinList (GlyphF StringNumber)
string2 = unpair `oo` anno2


strings :: [StringNumber] -> [Pitch] -> JoinList (GlyphF StringNumber)
strings = fromList `oo` annos


rootFifth :: Chord -> (Pitch,Pitch)
rootFifth ch = (chordRoot ch, maybe err id $ chordFifth ch)
  where
    err = error $ "Chord " ++ show ch ++ " has no fifth."


rootSeventh :: Chord -> (Pitch,Pitch)
rootSeventh ch = (chordRoot ch, maybe err id $ nthTone 8 ch)
  where
    err = error $ "Chord " ++ show ch ++ " has no eighth (octave)."



bass2 :: Chord -> (Pitch,Pitch)
bass2 ch = (chordRoot ch, fn ch) where
  fn = maybe (error $ "bass2 - chord " ++ show ch ++ " has no third") id . chordThird
  
high3 :: Chord -> (Pitch,Pitch,Pitch) 
high3 = step . pitchContent where
  step [s,t,u] = (s,t,u)
  step (_:xs)  = step xs
  step _       = error $ "high3 - too few tones in chord."
  



--------------------------------------------------------------------------------

alternate :: a -> a -> JoinList a
alternate a b =  (wrap a) `join` (wrap b)

twice :: JoinList a -> JoinList a
twice = JL.repeated 2

aaba :: JoinList a -> JoinList a -> JoinList a
aaba a b = (JL.repeated 2 a) `join` b `join` a

abba :: JoinList a -> JoinList a -> JoinList a
abba a b = a `join` (JL.repeated 2 b) `join` a



cycdist :: (a -> b -> c) -> [a] -> JoinList b -> JoinList c
cycdist f xs ks = JL.xzipWith (flip f) ks (cycle xs)


infixr 6 ~*

-- | Repetition - performs some /optimization/.
(~*) :: JoinList a -> Int -> JoinList a
(~*) = flip JL.repeated




--------------------------------------------------------------------------------
-- List transformations

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
