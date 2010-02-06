{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# OPTIONS -Wall #-}


--------------------------------------------------------------------------------
-- |
-- Module      :  M2.Segment
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Segmenting recursion
--
--------------------------------------------------------------------------------

module M2.Segment where

import M2.Datatypes
import M2.Duration
import M2.OneList

import Data.Ratio

-- This is the Measured class from the FingerTree paper and 
-- library but with Num for the superclass rather than Monoid

class Num (Measurement a) => NumMeasured a where
  type Measurement a
  nmeasure :: a -> Measurement a

instance NumMeasured Char where
  type Measurement Char = Int
  nmeasure 'a' = 0
  nmeasure _   = 1


class Extremity a where 
   outerElement :: a -> Bool
   innerOnly    :: a -> Bool
   
   innerOnly    = not . outerElement



data CB a = Carry a | Borrow a
  deriving (Eq,Show)



segment :: (Measurement a ~ v, NumMeasured a, Ord v, Num v) 
        => [a] -> [v] -> [[a]]
segment [] _      = []
segment xs []     = [xs]
segment xs (y:ys) = (bs:bss) where
                    (bs, (diff,xs')) = segStep xs y
                    bss              = segment xs' (cb diff ys)


-- carry-borrow interpretation is crucial...

cb :: (Num a, Ord a) => CB a -> [a] -> [a] 
cb (Carry a)  xs      | a >  0    = a:xs
                      | a == 0    = xs
cb (Borrow a) (x:xs)  | a >  x    = cb (Borrow $ a-x) xs
                      | a == x    = xs
                      | otherwise = (x-a):xs 
cb _          _                   = []
                         



segStep :: (Measurement a ~ v, NumMeasured a, Ord v, Num v) 
        => [a] -> v -> ([a], (CB v, [a]))
segStep []     _  = ([], (Carry 0, []))
segStep (x:xs) v  = step $ nmeasure x where
  step xd | xd >  v   = ([x], (Borrow $ negate $ v-xd, xs))
          | xd == v   = ([x], (Carry 0, xs))
          | otherwise = (x:xs', ans) where (xs',ans) = segStep xs (v-xd)



carryBorrow1 :: DurationMeasure -> MeterPattern 
            -> Either MeterPattern DurationMeasure
carryBorrow1 dx mp | dx == 0   = Left $ mp
                   | carry dx  = Left $ dx:mp
                   | otherwise = step (abs dx) mp 
   where
     post []                     = Right 0
     post xs                     = Left xs
     
     step bor []                 = Right bor
     step bor (x:xs) | x >  bor  = Left $ (x-bor):xs
                     | x == bor  = post xs
                     | otherwise = step (bor-x) []
 

carry :: DurationMeasure -> Bool
carry = (>=0)

borrow :: DurationMeasure -> Bool
borrow = (<0)


beam1 :: (Measurement a ~ DurationMeasure, NumMeasured a) 
      => DurationMeasure -> [a] -> ([OneMany a],[a],DurationMeasure) 
beam1 dur = bufferUnfold fromBuffer beamSegStep1 dur where
  fromBuffer [] = Nothing
  fromBuffer xs = Just $ fromList xs



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
 


beamSegStep1 :: (Measurement a ~ DurationMeasure, NumMeasured a) 
             => a -> DurationMeasure -> BStep a (OneMany a) DurationMeasure
beamSegStep1 _ v | v <= 0 = BDone v
beamSegStep1 x v          = step $ nmeasure x where
  step dx | dx >= 1%4  = BYield (one x) (v-dx)
          | otherwise  = BPush  x       (v-dx)




--------------------------------------------------------------------------------


data BStep interim ans st = BYield ans      !st
                          | BPush  interim  !st 
                          | BDone           !st
  deriving (Eq,Show)

-- outer is more a state than a list as borrow_carry doesn't
-- necessary consume it linearly.

beamingAUnfold :: forall outer_state inner_state a interim ans.
                  ([interim] -> Maybe ans) 
               -> (outer_state -> inner_state -> Maybe (outer_state,inner_state))
               -> (a -> inner_state -> BStep interim ans inner_state) 
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
        BYield a st' -> (mbCons pref (a:as'), xs') 
                        where pref      = buf_reduce $ unFIFO stk
                              (as',xs') = inner out st' fifoEmpty xs
        BPush  b st' -> inner out st' (fifoSnoc stk b) xs
        BDone    st' -> (mbCons pref as', xs') 
                        where pref      = buf_reduce $ unFIFO stk
                              (as',xs') = outer out st' (x:xs)

    terminate :: FIFO interim -> [ans]
    terminate stk       = maybe [] return $ buf_reduce (unFIFO stk)



bufferUnfold :: ([interim] -> Maybe ans) 
             -> (a -> st -> BStep interim ans st) 
             -> st -> [a] -> ([ans],[a],st)
bufferUnfold buf_reduce phi = step fifoEmpty where 
  step stk st  []     = (terminate stk,[],st)
  step stk st (x:xs)  = case phi x st of
      BYield a st' -> (mbCons pref (a:xs'), ys, st'') where 
                          pref          = buf_reduce $ unFIFO stk
                          (xs',ys,st'') = step fifoEmpty st' xs
      BPush  b st' -> step (fifoSnoc stk b) st' xs
      BDone    st' -> (terminate stk,x:xs,st')

  terminate stk       = maybe [] return $ buf_reduce (unFIFO stk)


-- fifo - Hughes resentation for efficient snoc-ing

type FIFO a = [a] -> [a]       

fifoEmpty           :: FIFO a
fifoEmpty           = id 

fifoSnoc            :: FIFO a -> a -> FIFO a
fifoSnoc f a        = f . (a:)           


unFIFO              :: FIFO a -> [a]
unFIFO              = ($ [])

mbCons :: Maybe a -> [a] -> [a]
mbCons Nothing  = id
mbCons (Just a) = (a:)





