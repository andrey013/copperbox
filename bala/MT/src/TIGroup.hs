{-# LANGUAGE FlexibleInstances          #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  TIGroup
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Transposition / Inversion group
--
--------------------------------------------------------------------------------

module TIGroup 
  ( 
  -- * Represent as a type class
    TIGroup(..)

  ) where


import Data.Sequence

class TIGroup a where 
  transpose :: Int -> a -> a 
  invert    :: a -> a
  invertn   :: Int -> a -> a 

instance TIGroup a => TIGroup [a] where
  transpose n = map (transpose n)
  invert = map invert 
  invertn n = map (invertn n)


instance TIGroup a => TIGroup (Seq a) where
  transpose n = fmap (transpose n)
  invert = fmap invert 
  invertn n = fmap (invertn n)


instance TIGroup a => TIGroup (a,a,a) where
  transpose n (a,b,c) = (transpose n a, transpose n b, transpose n c)
  invert (a,b,c) = (invert a, invert b, invert c)
  invertn n (a,b,c) = (invertn n a, invertn n b, invertn n c)