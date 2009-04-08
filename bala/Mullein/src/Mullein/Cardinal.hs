{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Mullein.Cardinal
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined
--
-- A datatype to represent cardinality.
-- 
-- A major complication in representing musical structure stems from
-- music appearing mostly linear, but needing a escape to /parallelism/.
--
-- For instance a staff is linearly divided into bars, which are divided 
-- into notes (and rests). Chords are only a slight problem - we can expand 
-- the element type held by the collection /bar/ to include chords.
-- Held bass notes are more of a problem (here we mean bass notes with same 
-- staff as the melody, not a seperate bass clef), essentially they are 
-- parallel stream of notes. Abc choose to represent held notes as bar 
-- length /overlays/. (Hand-written) LilyPond allows overlays at note, 
-- bar or melody level but for simplicity HNotate follows Abc and generates 
-- overlays divided at the bar level.
-- 
-- It is helpful, conceptually, to think of the regular single case 
-- (e.g. a melody without bass accompaniment) without the complication
-- of the multiple case. Hence the Cardinal datatype which is a analogue
-- of the Maybe type. Often the computation over Multi case can simply be
-- a map of the Single case. 
--    
-- Note Cardinal has no representation for null or Nothing, although
-- a Mutli case could (wrongly) be constructed with an empty list.
-- Where Maybe represents 0 or 1, Cardinal represents 1 or (>1), 
-- and List represents 0 or (>0).
--  
--------------------------------------------------------------------------------

module Mullein.Cardinal where


data Cardinal a = Single a 
                | Multi [a]
  deriving (Eq,Show)

toList :: Cardinal a -> [a]
toList (Single a) = [a]
toList (Multi xs) = xs

fromList :: [a] -> Cardinal a
fromList [x]    = Single x
fromList (x:xs) = Multi (x:xs)
fromList []     = error $ "Cardinal - fromList on empty list"

  
instance Functor Cardinal where
  fmap f (Single a) = Single (f a)
  fmap f (Multi xs) = Multi (fmap f xs)
  
  
   