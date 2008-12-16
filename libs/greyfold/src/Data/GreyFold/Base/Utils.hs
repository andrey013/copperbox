{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  GreyFold.Base.Utils
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined
--
-- Common utility functions.
--
--------------------------------------------------------------------------------


module GreyFold.Base.Utils where

-- | @adapt'bca@ - adapt a ternary function that expects args @a b c@ 
-- to receive args @b c a@ 
adapt'bca :: (a -> b -> c -> d) -> b -> c -> a -> d
adapt'bca f = \b c a -> f a b c

adapt'acb :: (a -> b -> c -> d) -> a -> c -> b -> d
adapt'acb f = \a c b -> f a b c

-- | @adapt'tabc@ - adapt a ternary function that expects args @a b c@ 
-- to receive args @(a,b) c@ 
adapt'tabc :: (a -> b -> c -> d) -> (a,b) -> c -> d
adapt'tabc f = \(a,b) c -> f a b c

adapt'atbc :: (a -> b -> c -> d) -> a -> (b,c) -> d
adapt'atbc f = \a (b,c) -> f a b c

adapt'ctab :: (a -> b -> c -> d) -> c -> (a,b) -> d
adapt'ctab f = \c (a,b) -> f a b c

adapt'ctba :: (a -> b -> c -> d) -> c -> (b,a) -> d
adapt'ctba f = \c (b,a) -> f a b c

  