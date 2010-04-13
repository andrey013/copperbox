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

  -- * Pinpoint schemes
  , PinpointScheme(..)
  , PinpointSchemeSt(..)

  , foldl_st'

  ) where 

import Neume.Core.Utils.HList
import qualified Neume.Core.Utils.OneList as O
import Neume.Core.Utils.StateMap

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

-- | pinpoint 
-- Variations on map where the first or last element is special
--
class PinpointScheme t where
  firstSpecial :: (a -> b) -> (a -> b) -> t a -> t b
  lastSpecial  :: (a -> b) -> (a -> b) -> t a -> t b


-- instances

instance PinpointScheme [] where
  firstSpecial _ _ []     = []
  firstSpecial f g (a:as) = f a : map g as

  lastSpecial _ _ []     = []   
  lastSpecial f g (a:as) = step a as where
    step x []           = [g x]
    step x (y:ys)       = f x : step y ys
   
    
instance PinpointScheme O.OneList where
  firstSpecial f g s = case O.viewl s of
    O.OneL x    -> O.one (f x) 
    x O.:<< xs  -> f x `O.cons` fmap g xs

  lastSpecial f g s  = step (O.viewl s) where
    step (O.OneL x)   = O.one (g x)
    step (e O.:<< xs) = f e `O.cons` step (O.viewl xs)

--------------------------------------------------------------------------------


class PinpointSchemeSt t where
  firstSpecial_st :: 
      (st -> a -> (b,st)) -> (st -> a -> (b,st)) -> st -> t a -> (t b,st)
  
  lastSpecial_st  ::
      (st -> a -> (b,st)) -> (st -> a -> (b,st)) -> st -> t a -> (t b,st)



instance PinpointSchemeSt [] where
  firstSpecial_st _ _ st []     = ([],st)
  firstSpecial_st f g st (a:as) = (a':as',st'') 
    where (a',st')   = f st a 
          (as',st'') = stmap g st' as

  lastSpecial_st _ _ st []     = ([],st)
  lastSpecial_st f g st (a:as) = step st a as where
    step s x []           = let (z,st') = g s x in ([z],st')
    step s x (y:ys)       = let (z,st') = f s x 
                                (zs,st'') = step st' y ys
                            in (z:zs,st'')

--------------------------------------------------------------------------------
-- foldl_st

-- this could be done as a foldl with a bit of pairing of course...

-- data SP a b = SP !a !b

foldl_st' :: (st -> b -> a -> (b,st)) -> st -> b -> [a] -> (b,st) 
foldl_st' f st0 b0 lzt = step st0 b0 lzt where
    step st b []     = (b,st)
    step st b (x:xs) = let (b',st') = f st b x in b' `seq` st' `seq` step st' b' xs  
