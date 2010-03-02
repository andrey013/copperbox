{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}


--------------------------------------------------------------------------------
-- |
-- Module      :  Neume.Bracket
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Bar splitting and beam grouping.
--
--------------------------------------------------------------------------------

module Neume.Bracket where

import Neume.Datatypes
import Neume.Duration
import Neume.Utils

import Text.PrettyPrint.Leijen                  -- package: wl-pprint

import Data.List ( foldl' )
import Data.Ratio



-- | A Metric unit represents a division of a NoteList according
-- to the sizes of a 'MeterPattern'.
--
-- Where there is not an exact fit, the splitting algorithm will
-- borrow from the input list an extra note. This potentially 
-- means the next metrical unit might have been 'all borrowed', 
-- example a whole note in 4/4 time with meter pattern [2,2] 
-- gives:
-- 
-- > [ MUnit "wholenote", BZero ]
--
--
data MetricUnit a = MUnit (NoteList a)
                  | BZero                  -- Borrowed zero
 deriving (Eq,Show)


--------------------------------------------------------------------------------
-- Cycle the MetricalSpec / MeterPattern

-- | 'cyclePattern' : anacrusis * meter_pattern -> meter_pattern
--
-- Cycle the MeterPattern producing an infinite list.
-- 
-- The anacrusis is the length of the prefix - negative values 
-- are ignored.
--
cyclePattern :: Rational -> MeterPattern -> MeterPattern
cyclePattern ana mp | ana <= 0  = cycle mp
                    | otherwise = let len = sum mp
                                      a2  = len - (ana `modR` len)
                                  in sminus a2 (cycle mp)








--------------------------------------------------------------------------------
-- Stack (meter pattern) addition and subtraction

-- | sminus subtracts from the top of stack.
--
-- If the number to be subtracted is greater than the top of the
-- stack the top of the stack is popped and the remainder is 
-- subtracted from the new stack top (the stack will never 
-- contain negative numbers).
-- 
--

sminus :: (Num a, Ord a) => a -> [a] -> [a]
sminus a       xs     | a < 0   = splus (abs a) xs
sminus _       []               = []
sminus a       (x:xs)           = step (x-a) xs where
  step r ys      | r > 0     = r:ys
  step r (y:ys)  | r < 0     = step (y - abs r) ys
  step _ ys                  = ys                   -- empty stack or r==0
  

-- | splus always conses the scalar (unless it is negative, which 
-- is treated as stack-minus).
--    
splus :: (Num a, Ord a) => a -> [a] -> [a]
splus a xs | a > 0     = a:xs
           | a < 0     = sminus (abs a) xs
           | otherwise = xs

--------------------------------------------------------------------------------
-- Measuring the plet-tree, and tuplet stack


type TStack = [Rational]

-- scale factor
sf :: TStack -> Rational
sf []     = (1%1)
sf (x:xs) = x * sf xs

tempty :: TStack
tempty = []

pushT :: Int -> Int -> TStack -> TStack
pushT p q stk = s : stk where
  s = (fromIntegral q) % (fromIntegral p)   -- note - q%p 

-- | The measure of a \single\ or a \plet tree\ - plet trees are
-- considered indivisable so it is not a problem to sum them.
--
umeasure :: (Measurement a ~ DurationMeasure, NumMeasured a) 
             => PletTree a -> DurationMeasure
umeasure = snd . pletFold  phi chi (tempty,0) where
  phi a      (stk,acc) = (stk, acc + sf stk * nmeasure a)
  chi p q    (stk,acc) = (pushT p q stk,acc) 

-- | The number of items in a PletTree 
--
-- NOTE - need the pred to test e.g. grace notes, which 
-- shouldn\'t be counted.
--
-- Is this a suitable case for another Type Class?
--
ucount :: (a -> Bool) -> PletTree a -> Int
ucount test = pletFold phi chi 0 where
  phi a   n  | test a    = n+1
             | otherwise = n
  chi _ _ n              = n

pletFold :: (a -> b -> b) -> (Int -> Int -> b -> b) -> b -> PletTree a -> b
pletFold f _ b (S a)         = f a b
pletFold f g b (Plet p q xs) = foldl' (pletFold f g) (g p q b) $ getNoteList xs

--------------------------------------------------------------------------------
-- Divide the NoteList

divisions :: (Measurement a ~ DurationMeasure, NumMeasured a) 
          => Rational -> MeterPattern -> NoteList a -> [MetricUnit a]
divisions ana mp notes = step 0 (cyclePattern ana mp) (getNoteList notes) where
  
  -- borrow greater than next mertical unit => produce a BZero
  step borrow (d:ds) xs   | borrow >= d = BZero : step (borrow-d) ds xs
 
   -- input exhausted => produce empty list
  step _      _      []   = []
 
  -- 'normal' operation => use division1 to produce a MUnit
  step borrow (d:ds) xs   = let (one,borrow',rest) = division1 (d-borrow) xs
                            in one : step borrow' ds rest

  -- unreachable as meter patterns have been cycled
  step _       []     _    = error "Bracket.divisions - unreachable"

                            
-- | Extract one MeticUnit from the input list, return the unit, 
-- the carry, and the rest-of-input.
--
division1 :: (Measurement a ~ DurationMeasure, NumMeasured a)  
          => DurationMeasure
          -> [PletTree a] 
          -> (MetricUnit a, DurationMeasure, [PletTree a])
division1 runit = post . unwind phi runit where
  phi r a | r > 0       = AYield a (r - umeasure a)
          | otherwise   = ADone    

  post (xs,borrow,rest) = (MUnit $ NoteList xs, abs borrow, rest)


-- | unfold against a list, presenting the rest-of-list and the 
-- state at the end

data AStep a st = AYield a  !st
                | ADone     
  deriving (Eq,Show)

unwind :: (st -> a -> AStep b st) -> st -> [a] -> ([b],st,[a])
unwind phi s0 = step id s0 where
  step k st (x:xs) = case phi st x of
                       AYield a st' -> step (k . (a:)) st' xs
                       ADone        -> (k [],st,x:xs)
  step k st []     = (k [],st,[])  


--------------------------------------------------------------------------------
-- Pretty instances

instance Pretty a => Pretty (MetricUnit a) where
  pretty (MUnit notes) = angles $ pretty notes
  pretty BZero         = text "___" 


