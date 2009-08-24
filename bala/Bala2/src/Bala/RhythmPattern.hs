{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.RhythmPattern
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Rhythm represention
--
--------------------------------------------------------------------------------

module Bala.RhythmPattern  where

import Data.Stream.Hinze.Stream 

-- import Data.Set ( Set )
-- import qualified Data.Set as Set

import Prelude hiding (head, tail)
import qualified Prelude as Pre

-- Rhythmic Pulse
data Pulse = N Int | R Int
  deriving (Eq,Show)

-- Represent rhythm patterns as a list of pulses. This is more 
-- useful than say subset patterns or box notation, because it
-- contains more information (length of the pulses - rather than
-- just their onset).
newtype RhythmPattern = RhythmPattern { getPulseList :: [Pulse] }

makeRhythmPattern :: [Pulse] -> Int -> RhythmPattern
makeRhythmPattern xs 1 = RhythmPattern xs
makeRhythmPattern xs n 
    | n > 1            = RhythmPattern $ concat (replicate n xs)
    | otherwise        = error $ 
          "makeRhythmPattern - cannot make RhythmPattern with " 
          ++ show n ++ " repetitions."
                         
                          


timespan :: RhythmPattern -> Int
timespan = foldr fn 0 . getPulseList where
  fn (N i) n = n+i
  fn (R i) n = n+i



-- | Does the rhythm pattern start on an on-beat?
onBeat :: RhythmPattern -> Bool
onBeat (RhythmPattern ((N _):_)) = True
onBeat _                         = False



-- | Print a SubsetPattern in /box notation/.
showBox :: RhythmPattern -> String
showBox (RhythmPattern xs) = foldr fn "" xs
  where
    fn (N i) acc = 'X' : replicate (i-1) '.' ++ acc
    fn (R i) acc = replicate i '.' ++ acc


toStream :: RhythmPattern -> Stream Pulse
toStream (RhythmPattern xs) = st where st = xs << st


takeNBars :: Int -> Int -> Stream Pulse -> [Pulse]
takeNBars n bar_len = step (n*bar_len) where
  step a s = let sh = head s
                 sz = case sh of (N i) -> i; (R i) -> i
             in if (sz>a) then []
                          else sh : step (a-sz) (tail s)

