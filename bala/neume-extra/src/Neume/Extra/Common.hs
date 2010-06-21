{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Neume.Extra.Common
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Common utils...
--
--------------------------------------------------------------------------------

module Neume.Extra.Common 
  ( 
    BarNum

  , strip
  , longZipWith

  ) where



type BarNum = Int



-- | strip - dual of const
--
strip :: a -> b -> b
strip _ b = b

longZipWith :: (a -> a -> a) -> [a] -> [a] -> [a]
longZipWith _ xs     []     = xs
longZipWith _ []     ys     = ys
longZipWith f (x:xs) (y:ys) = f x y : longZipWith f xs ys