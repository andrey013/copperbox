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
import M2.Unfold

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

-- 
{-
newSegment :: (Measurement a ~ DurationMeasure, NumMeasured a) 
           => MeterPattern -> [a] -> ([[OneMany a]],[a],DurationMeasure)
newSegment ms input = post $ aUnfoldMap phi (0,input) ms where
  post (ans,_,(cb,rest))    = (ans,rest,cb)
  phi mp (carry_borrow,inp) = undefined 
-}


carryBorrow :: DurationMeasure -> MeterPattern 
            -> Either MeterPattern DurationMeasure
carryBorrow dx mp | dx == 0   = Left $ mp
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


segmentBeam :: [DurationMeasure] -> [a] -> ([[OneMany a]],[a])
segmentBeam meter_pats xs = interlockAUnfold phi state meter_pats xs 
  where
    phi dur x _ = undefined
    state     = 0

beam1 :: (Measurement a ~ DurationMeasure, NumMeasured a) 
      => DurationMeasure -> [a] -> ([OneMany a],[a],DurationMeasure) 
beam1 dur = bufferUnfold fromBuffer beamSegStep1 dur where
  fromBuffer [] = Nothing
  fromBuffer xs = Just $ fromList xs


-- produces notes or beam groups
-- ... the beam groups may not be properly formed as there isn't 
-- an obvious way to handle extremities in this function
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



beamingAUnfold :: forall outer inner interim st ans. Num st =>
                  ([interim] -> Maybe ans) 
               -> (st -> [outer] -> [outer])
               -> (outer -> inner -> st -> BStep interim ans st) 
               -> st -> [outer] -> [inner] -> ([ans],[inner])
beamingAUnfold buf_reduce borrow_carry phi st0 outs inns = outer outs inns st0 
  where
    outer :: [outer] -> [inner] -> st -> ([ans],[inner])
    outer []  ys  _    = ([],ys)
    outer _   []  _    = ([],[])
    outer os  ys  st   = case borrow_carry st os of
                           []     -> ([],ys)
                           (x:xs) -> inner xs (phi x) fifoEmpty ys 0

    inner :: [outer] -> (inner -> st -> BStep interim ans st) 
                     -> FIFO interim -> [inner] -> st -> ([ans],[inner])
    inner _    _  stk []     _   = (terminate stk,[])
    inner next fn stk (y:ys) st  = case fn y st of
        BYield a st' -> (mbCons pref (a:as'), ys') 
                        where 
                          pref      = buf_reduce $ unFIFO stk
                          (as',ys') = inner next fn fifoEmpty ys st'
        BPush  b st' -> inner next fn (fifoSnoc stk b) ys st'
        BDone    st' -> (mbCons pref as', ys') 
                        where 
                          pref      = buf_reduce $ unFIFO stk
                          (as',ys') = inner next fn fifoEmpty ys st'
        
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





