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

module Neume.Bracket 
  ( 
    NumMeasured(..)
  , BeamExtremity(..)
  , beamSegment

  ) where

import Neume.Datatypes
import Neume.Duration
import Neume.OneList

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



-- cexpression :: BeamExtremity a => [OneMany a] -> CExpr a

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
             => a -> DurationMeasure -> BStep a (OneMany a) DurationMeasure
beamSegStep1 _ v | v <= 0 = BDone v
beamSegStep1 x v          = step $ nmeasure x where
  step dx | dx >= 1%4 = BYield (one x) (v-dx)
          | otherwise = BPush  x       (v-dx)




--------------------------------------------------------------------------------


data BStep interim ans st = BYield ans      !st
                          | BPush  interim  !st 
                          | BDone           !st
  deriving (Eq,Show)



-- FIFO - Hughes representation of stacks/lists for efficient 
-- snoc-ing

type FIFO a = [a] -> [a]       

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


fifoEmpty           :: FIFO a
fifoEmpty           = id 

fifoSnoc            :: FIFO a -> a -> FIFO a
fifoSnoc f a        = f . (a:)           


unFIFO              :: FIFO a -> [a]
unFIFO              = ($ [])

mbCons :: Maybe a -> [a] -> [a]
mbCons Nothing  = id
mbCons (Just a) = (a:)


