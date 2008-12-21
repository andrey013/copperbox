{-# LANGUAGE FlexibleContexts           #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  HNotate.BeamTrafo
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  mptc, fundeps
--
-- Transformation for beam grouping.
--
--------------------------------------------------------------------------------


module HNotate.BeamTrafo where


import HNotate.Duration 
import HNotate.Fits
import HNotate.NoteListDatatypes

import Data.Sequence


beam :: [Duration] -> Seq Element -> Seq MetricalWord
beam ds se = beamfoldl reducer flush empty empty se ds

reducer :: 
    Seq MetricalWord -> Seq Element -> Element -> [Duration] 
                     -> (Seq MetricalWord, Seq Element, [Duration])
reducer smw se e []       = (flushAddSingle smw se e, empty, [])
reducer smw se e (d:ds)   = step (d - measure e) (subtracts (measure e) (d:ds))
  where
   -- An eighth or less and fits in the current beam group
   step d' ds' 
       | d' >  0 && lteEighth e = (smw, se |> e, ds')

   -- An eighth or less and /seals/ the current beam group               
       | d' == 0 && lteEighth e = (flush smw (se |> e), empty, ds')
               
   -- Otherwise the element isn't part of a beam group            
       | otherwise	            = (flushAddSingle smw se e, empty, ds')

lteEighth :: Fits a Duration => a -> Bool
lteEighth a = measure a <= eighth  

subtracts :: Duration -> [Duration] -> [Duration]
subtracts _ []     = []
subtracts a (d:ds) | a == d    = ds
                   | a <  d    = (d-a):ds
                   | otherwise = subtracts (a-d) ds 


flushAddSingle :: Seq MetricalWord -> Seq Element -> Element -> Seq MetricalWord 
flushAddSingle smw se e =  (flush smw se) |> Singleton e

flush :: Seq MetricalWord -> Seq Element -> Seq MetricalWord
flush smw se = step (viewl se) where
    step EmptyL  = smw
    step _ 	     = smw |> (BeamGroup se)



-- | @beamfoldl@ - takes an unbeamed sequence of @Element@ and a list
-- of durations derived from the meter pattern. The fold can advance through
-- the list of meter patterns at will, hence the reducer has access to the 
-- list both as consumer and producer.   
-- The fold is also a double accumulator - @c@ holds the /bar/ produced so
-- far, @d@ holds the elements (if there are any) that will form the current 
-- beam group.
beamfoldl :: (c -> d -> a -> [b] -> (c,d,[b])) -> 
                (c -> d -> c) -> c -> d -> Seq a -> [b] -> c
beamfoldl f g c0 d0 se ls = step c0 d0 (viewl se) ls where
    step c d (a :< sa) bs  = step c' d' (viewl sa) bs' where (c',d',bs') = f c d a bs
    step c d EmptyL    _   = g c d


   