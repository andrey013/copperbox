{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Utils
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Utility functions
--
--------------------------------------------------------------------------------

module Bala.Utils
  ( 
  -- * Various functions

  -- ** reverse application
    ( # )
    
  , mapAfter
  , ntimes
  , nrotate

  ) where

import Data.List ( splitAt )

-- Reverse application

infixl 7 #

( # ) :: a -> (a -> b) -> b 
x # f = f x


mapAfter :: Int -> (a -> a) -> [a] -> [a]
mapAfter i f xs = ys ++ map f zs where
  (ys,zs) = splitAt i xs


ntimes :: Int -> [a] -> [a]
ntimes i = concat . map (replicate i)

nrotate :: Int -> [a] -> [a]
nrotate i xs = let (h,t) = splitAt i xs in t++ h
