{-# LANGUAGE ScopedTypeVariables        #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Neume.Core.Utils.SpecialTraversals
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Non-standard traversals...
--
--------------------------------------------------------------------------------


module Neume.Core.Utils.SpecialTraversals
  ( 
    
  -- * Segmenting
    interior

  , foldl_st'

  ) where 

import Neume.Core.Utils.HList

--------------------------------------------------------------------------------

-- Note to self - when using ScopedTypeVariables you have to 
-- add forall for the appropriate variables as well.
--

--------------------------------------------------------------------------------

-- | @interior@ models grouping notes within a metrical unit 
-- into a beam group.
--
-- First we buffer all notes longer than (1%8) etc that are on
-- the left. 
-- 
-- Once we meet an eighth note we move inside with two buffers,
-- the second buffer can be speculatively filled with rests/ spacers
-- that cannot end a beam group but can be within it. We continue
-- until we reach the end of the bar of a note longer than an
-- eighth.
--
interior :: forall a b. 
  (a -> Bool) -> (a -> Bool, a -> Bool) -> ([b] -> b) -> (a -> b) -> [a] -> [b]
interior outLeft (insideAlways, insideSpeculative) incrush fn lzt = 
    leftside id lzt
  where
    leftside :: H b -> [a] -> [b]
    leftside accf []                 = accf []  -- all left, no interior
    leftside accf (x:xs) | outLeft x = leftside (accf `snoc` fn x) xs
                         | otherwise = accf $ inside (fn x:) id xs

    -- specf is a /speculative/ buffer...
    inside :: H b -> H b -> [a] -> [b]
    inside alwaysf specf []     = (incrush $ alwaysf []) : specf []
    inside alwaysf specf (x:xs) 
      | insideSpeculative x     = inside alwaysf (specf `snoc` fn x) xs
      | insideAlways x          = inside (alwaysf . specf `snoc` fn x) id xs
      | otherwise               = (incrush $ alwaysf []) : specf (map fn (x:xs))


--------------------------------------------------------------------------------
-- foldl_st

-- this could be done as a foldl with a bit of pairing of course...

-- data SP a b = SP !a !b

foldl_st' :: (st -> b -> a -> (b,st)) -> st -> b -> [a] -> (b,st) 
foldl_st' f st0 b0 lzt = step st0 b0 lzt where
    step st b []     = (b,st)
    step st b (x:xs) = let (b',st') = f st b x in b' `seq` st' `seq` step st' b' xs  
