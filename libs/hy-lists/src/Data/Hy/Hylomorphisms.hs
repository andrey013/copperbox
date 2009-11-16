{-# OPTIONS -Wall #-}


--------------------------------------------------------------------------------
-- |
-- Module      :  Data.Hy.Hylomorphisms
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
-- 
-- Hylomorphisms...
--
--------------------------------------------------------------------------------

module Data.Hy.Hylomorphisms
  ( 
    hylor
  , hylol

  ) where


hylor :: (st -> Maybe (a,st)) -> (a -> b -> b) -> b -> st -> b
hylor g f e a = case g a of
                  Nothing     -> e
                  Just (b,st) -> f b (hylor g f e st)



hylol :: (st -> Maybe (a,st)) -> (b -> a -> b) -> b -> st -> b
hylol g f e a = case g a of
                  Nothing     -> e
                  Just (b,st) -> hylol g f (f e b) st

-- hyfilter :: (a -> Bool) 
-- 