{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Neume.Core.Utils.Basis
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Common functions
--
--------------------------------------------------------------------------------


module Neume.Core.Utils.Basis
  ( 
    stmap

  -- * Left view as per Data.Sequence  
  , mapFst
  , mapSnd
  
  ) where



stmap :: (a -> st -> (b,st)) -> st -> [a] -> ([b],st)
stmap fn s0 = step s0
  where
    step s []     = ([],s)
    step s (x:xs) = let (a,s1)  = fn x s 
                        (as,s2) = step s1 xs
                    in (a:as,s2)


mapFst :: (a -> c) -> (a,b) -> (c,b)
mapFst f (a,b) = (f a, b)

mapSnd :: (b -> c) -> (a,b) -> (a,c)
mapSnd g (a,b) = (a, g b)
