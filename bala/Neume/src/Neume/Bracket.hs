{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleContexts           #-}
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
import Neume.OneList
-- import Neume.OneTwo
import Neume.SyntaxStaff
import Neume.Utils

import qualified Data.Foldable          as F
import Data.List ( unfoldr )
import Data.Sequence ( Seq )
import qualified Data.Sequence          as S
import Data.Ratio


-- This is the Measured class from the FingerTree paper and 
-- library but with Num for the superclass rather than Monoid

class Num (Measurement a) => NumMeasured a where
  type Measurement a
  nmeasure :: a -> Measurement a

class BeamExtremity a where 
   outerElement :: a -> Bool
   innerOnly    :: a -> Bool
   
   innerOnly    = not . outerElement



--------------------------------------------------------------------------------
-- Cycle the MetricalSpec / MeterPattern

-- | 'cyclePattern' : anacrusis * meter_pattern -> meter_pattern
--
-- Cycle the MeterPattern producing an infinite list.

cyclePattern :: Rational -> MeterPattern -> MeterPattern
cyclePattern ana mp = ana `sminus` cycle mp




type SegmentedList a = [Segment a]

data Segment a = One a
               | Many (Seq a)






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
sminus a       []               = []
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
-- tuplet stack

type TStack = [Ratio Int]

scaleFactor :: TStack -> Ratio Int
scaleFactor []     = (1%1)
scaleFactor (x:xs) = x * scaleFactor xs


--------------------------------------------------------------------------------
-- Extracting extremities...

{-

-- | leftExtremity peels at most 1 left element from the list, if
-- it is an \'inner only\' regardless of whether the next element 
-- is \'inner only\' as well.
--
leftExtremity :: BeamExtremity a => OneMany a -> OneTwo (OneMany a) a
leftExtremity os = step $ viewl os where
  step (x :<< xs)  | innerOnly x = Two xs x     -- atleast 1 left extremity
  step _                         = One os       -- no left extremity

-- | 'rightExtremity' is unfortunately more complicated than 
-- 'leftExtremity'.
-- 
-- It produces a beam group, if there is one, and a possibly 
-- empty list of elements from the right of the list that 
-- cannot be the extremity of a beam group (rests, spaces...)
--
rightExtremity :: BeamExtremity a => OneMany a -> (Maybe (OneMany a), [a])
rightExtremity = post . F.foldr unwind ([],[]) where
  unwind a ([], ys) | innerOnly a = ([],a:ys)
  unwind a (xs, ys)               = (a:xs,ys)
  post ([],ys)  = (Nothing, ys)             -- all extremities
  post ([a],ys) = (Nothing, a:ys)           -- don't yield singleton beamgroup
  post (xs,ys)  = (Just $ fromList xs, ys)  -- beamgroup, zero-or-more exts

-}
--------------------------------------------------------------------------------

phrase :: MetricalSpec -> [gly] -> StaffPhrase gly
phrase (MetricalSpec time_sig meter_patts) notes = undefined

{-
-- No! - remove extremities in one step...
-- (Putting into CExprs should be done when grouping for bars)
cexpressions :: BeamExtremity a => [OneMany a] -> [CExpr a]
cexpressions []     = []
cexpressions (x:xs) | isOne x = (Atoms x) : cexpressions xs
-}



{-
segmentAll :: (BeamExtremity a, Measurement a ~ DurationMeasure, NumMeasured a)
           => [DurationMeasure] -> [a] -> [OneMany a]
segmentAll meter_patts notes = moveExtremities xs ++ map one ys
  where
    (xs,ys) = beamSegment meter_patts notes
              


-- | Beam groups cannot start or end with rests or spacers...
--
moveExtremities :: forall a. BeamExtremity a => [OneMany a] -> [OneMany a]
moveExtremities = unfoldr phi where
  phi []      = Nothing
  phi (x:xs)  = case leftExtremity x of
      Two os a  -> Just (one a, os:xs) 
      One os    -> case rightExtremity os of
          (Nothing, y:ys) -> Just (one y, prepend ys xs) 
          (Nothing, _)    -> error "moveExtremities unreachable"
          (Just bg, ys)   -> Just (bg, prepend ys xs)


  prepend :: [a] -> [OneMany a] -> [OneMany a]
  prepend []     xs = xs
  prepend (a:as) xs = one a : prepend as xs
-}

beamSegment :: (Measurement a ~ DurationMeasure, NumMeasured a) 
            => [DurationMeasure] -> [a] -> ([OneMany a],[a])
beamSegment meter_pats xs = 
    beamingAUnfold bufferReduce carryBorrow beamSegStep1 meter_pats 0 xs


bufferReduce :: [a] -> Maybe (OneMany a)
bufferReduce [] = Nothing
bufferReduce xs = Just $ fromList xs


carryBorrow :: MeterPattern
            -> DurationMeasure 
            -> Maybe (MeterPattern,DurationMeasure)
carryBorrow mp dx | dx == 0   = post mp
                  | carry dx  = post $ dx:mp
                  | otherwise = post $ step (abs dx) mp 
   where
     post []                     = Nothing 
     post (x:xs)                 = Just (xs,x)
     
     step _   []                 = []
     step bor (x:xs) | x >  bor  = (x-bor):xs
                     | x == bor  = xs
                     | otherwise = step (bor-x) xs
 

carry :: DurationMeasure -> Bool
carry = (>=0)

-- borrow :: DurationMeasure -> Bool
-- borrow = (<0)


beamSegStep1 :: (Measurement a ~ DurationMeasure, NumMeasured a) 
             => a -> DurationMeasure -> BIStep a (OneMany a) DurationMeasure
beamSegStep1 _ v | v <= 0 = BIDone v
beamSegStep1 x v          = step $ nmeasure x where
  step dx | dx >= 1%4 = BIYield (one x) (v-dx)
          | otherwise = BIPush  x       (v-dx)




--------------------------------------------------------------------------------
-- Bar unfold - fairly complex...

-- Buffered
data BStep a st = BYield   !st
                | BPush  a !st
                | BDone
  deriving (Eq,Show)

bufferedUnfold :: forall a interim ans st. 
                  (interim -> ans) 
               -> (interim -> Bool)  
               -> interim
               -> (interim -> a -> interim) 
               -> (st -> BStep a st) 
               -> st 
               -> [ans]
bufferedUnfold out is_zero zero snoc phi st0 = step zero (phi st0) where
  step :: interim -> BStep a st -> [ans]
  step buf (BYield   st') = out buf : step zero (phi st')
  step buf (BPush  a st') = step (snoc buf a) (phi st')
  step buf BDone          | is_zero buf = []
                          | otherwise   = [out buf]



--------------------------------------------------------------------------------
-- Beaming unfold - highly complex!

data BIStep interim ans st = BIYield ans      !st
                           | BIPush  interim  !st 
                           | BIDone           !st
  deriving (Eq,Show)


-- FIFO - Hughes representation of stacks/lists for efficient 
-- snoc-ing

type FIFO a = [a] -> [a]       

-- The \'unfold\' for beaming is not exactly \"general\"...
-- And at some point it would be nice to see, if it can be 
-- \"composed from smaller parts\".
beamingAUnfold :: forall outer_state inner_state a interim ans.
                  ([interim] -> Maybe ans) 
               -> (outer_state -> inner_state -> Maybe (outer_state,inner_state))
               -> (a -> inner_state -> BIStep interim ans inner_state) 
               -> outer_state -> inner_state -> [a] -> ([ans],[a])
beamingAUnfold buf_reduce next_state phi outer_st inner_st xs0 = 
    outer outer_st inner_st xs0
  where
    outer :: outer_state -> inner_state -> [a] -> ([ans],[a])
    outer _   _   []  = ([],[])
    outer out inn xs  = case next_state out inn of
        Nothing          -> ([],xs)
        Just (out',inn') -> inner out' inn' fifoEmpty xs

    inner :: outer_state -> inner_state -> FIFO interim -> [a] -> ([ans],[a])
    inner _   _   stk []     = (terminate stk,[])
    inner out inn stk (x:xs) = case phi x inn of
        BIYield a st' -> (mbCons pref (a:as'), xs') 
                         where pref      = buf_reduce $ unFIFO stk
                               (as',xs') = inner out st' fifoEmpty xs
        BIPush  b st' -> inner out st' (fifoSnoc stk b) xs
        BIDone    st' -> (mbCons pref as', xs') 
                         where pref      = buf_reduce $ unFIFO stk
                               (as',xs') = outer out st' (x:xs)

    terminate :: FIFO interim -> [ans]
    terminate stk       = maybe [] return $ buf_reduce (unFIFO stk)


fifoEmpty           :: FIFO a
fifoEmpty           = id 

fifoSnoc            :: FIFO a -> a -> FIFO a
fifoSnoc f a        = f . (a:)           


unFIFO              :: FIFO a -> [a]
unFIFO              = ($ [])

mbCons :: Maybe a -> [a] -> [a]
mbCons Nothing  = id
mbCons (Just a) = (a:)


