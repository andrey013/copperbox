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
    segmentToSize
  , interior

  -- * Pinpoint schemes
  , PinpointScheme(..)
  , PinpointSchemeSt(..)
  ) where 

import qualified Neume.Core.Utils.OneList as O

import qualified Data.Sequence as S

--------------------------------------------------------------------------------

type H a = [a] -> [a]

infixr 2 `snoc`
snoc :: H b -> b -> H b
snoc accf a = accf . (a:)

-- Note to self - when using ScopedTypeVariables you have to 
-- add forall for the appropriate variables as well.
--


-- | @segmentToSize@ splits a list into segments of a particular 
-- length.
-- 
-- For Neume's purpose we allow an anacrusis of a different 
-- length and include a type changing function to be mapped 
-- along the list at the same time
--
segmentToSize :: forall a b . Int -> Int -> (a -> b) -> [a] -> [[b]]
segmentToSize ana_len seg_len fn lzt = anastep lzt
  where
    anastep :: [a] -> [[b]]
    anastep xs = a:as 
      where (a,rest) = step1 id ana_len xs 
            as       = segstep rest 
            
    segstep :: [a] -> [[b]]
    segstep [] = []
    segstep xs = a : segstep rest 
      where (a,rest) = step1 id seg_len xs
    
    step1 :: H b -> Int -> [a] -> ([b],[a])
    step1 accf n xs     | n <= 0 = (accf [], xs)  
    step1 accf _ []              = (accf [], [])
    step1 accf n (x:xs)          = step1 (accf `snoc` fn x) (n-1) xs


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
interior :: forall a. 
            (a -> Bool) -> (a -> Bool, a -> Bool) -> ([a] -> a) -> [a] -> [a]
interior outLeft (insideAlways, insideSpeculative) incrush lzt = leftside id lzt
  where
    leftside :: H a -> [a] -> [a]
    leftside accf []                 = accf []  -- all left, no interior
    leftside accf (x:xs) | outLeft x = leftside (accf `snoc` x) xs
                         | otherwise = accf $ inside (x:) id xs

    -- specf is a /speculative/ buffer...
    inside :: H a -> H a -> [a] -> [a]
    inside alwaysf specf []          = (incrush $ alwaysf []) : specf []
    inside alwaysf specf (x:xs) 
      | insideSpeculative x          = inside alwaysf (specf `snoc` x) xs
      | insideAlways x               = inside (alwaysf . specf `snoc` x) id xs
      | otherwise                    = (incrush $ alwaysf []) : specf (x:xs)


--------------------------------------------------------------------------------

-- | pinpoint 
-- Variations on map where the first or last element is special
--
class PinpointScheme t where
  firstSpecial :: (a -> b) -> (a -> b) -> t a -> t b
  lastSpecial  :: (a -> b) -> (a -> b) -> t a -> t b


instance PinpointScheme [] where
  firstSpecial _ _ []     = []
  firstSpecial f g (a:as) = f a : map g as

  lastSpecial _ _ []     = []   
  lastSpecial f g (a:as) = step a as where
    step x []           = [g x]
    step x (y:ys)       = f x : step y ys

instance PinpointScheme S.Seq where
  firstSpecial f g s = case S.viewl s of
    S.EmptyL   -> S.empty
    e S.:< se  -> f e S.<| fmap g se

  lastSpecial f g s  = case S.viewr s of
    S.EmptyR   -> S.empty
    se S.:> e  -> fmap f se S.|> g e
   
    
instance PinpointScheme O.OneList where
  firstSpecial f g s = case O.viewl s of
    O.OneL x    -> O.one (f x) 
    x O.:<< xs  -> f x `O.cons` fmap g xs

  lastSpecial f g s  = step (O.viewl s) where
    step (O.OneL x)   = O.one (g x)
    step (e O.:<< xs) = f e `O.cons` step (O.viewl xs)




class PinpointSchemeSt t where
  firstSpecial_st :: 
      (st -> a -> (b,st)) -> (st -> a -> (b,st)) -> st -> t a -> (t b,st)
  
  lastSpecial_st  ::
      (st -> a -> (b,st)) -> (st -> a -> (b,st)) -> st -> t a -> (t b,st)
