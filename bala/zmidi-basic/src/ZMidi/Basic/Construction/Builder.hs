{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZMidi.Basic.Construction.Builder
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Building /high level/ MIDI.
--
--------------------------------------------------------------------------------

module ZMidi.Basic.Construction.Builder
  (
  -- * Note constructors 
    c_nat  
  , c_sharp
  , d_flat
  , d_nat
  , d_sharp
  , e_flat
  , e_nat
  , f_nat
  , f_sharp  
  , g_flat
  , g_nat
  , g_sharp
  , a_flat
  , a_nat
  , b_flat
  , b_nat

  , Build
  , BuildT

  ) where


import ZMidi.Basic.Construction.Datatypes

import Control.Applicative
import Data.Word


clampPch :: Int -> Word8
clampPch i | i < 0     = 0
           | i > 127   = 127
           | otherwise = fromIntegral i


c_nat       :: Int -> Word8
c_nat o     = clampPch $     12 * (o + 1)

c_sharp     :: Int -> Word8
c_sharp o   = clampPch $ 1 + 12 * (o + 1)

d_flat      :: Int -> Word8
d_flat      = c_sharp

d_nat       :: Int -> Word8
d_nat o     = clampPch $ 2 + 12 * (o + 1)

d_sharp     :: Int -> Word8
d_sharp o   = clampPch $ 3 + 12 * (o + 1)

e_flat      :: Int -> Word8
e_flat      = d_sharp

e_nat       :: Int -> Word8
e_nat o     = clampPch $ 4 + 12 * (o + 1)

f_nat       :: Int -> Word8
f_nat o     = clampPch $ 5 + 12 * (o + 1)

f_sharp     :: Int -> Word8
f_sharp o   = clampPch $ 6 + 12 * (o + 1)

g_flat      :: Int -> Word8
g_flat      = f_sharp

g_nat       :: Int -> Word8
g_nat o     = clampPch $ 7 + 12 * (o + 1)

g_sharp     :: Int -> Word8
g_sharp o   = clampPch $ 8 + 12 * (o + 1)

a_flat      :: Int -> Word8
a_flat     = a_sharp

a_nat       :: Int -> Word8
a_nat o     = clampPch $ 9 + 12 * (o + 1)

a_sharp     :: Int -> Word8
a_sharp o   = clampPch $ 10 + 12 * (o + 1)

b_flat      :: Int -> Word8
b_flat      = a_sharp

b_nat       :: Int -> Word8
b_nat o     = clampPch $ 11 + 12 * (o + 1)



--------------------------------------------------------------------------------




data BuildEnv = BuildEnv
      { note_on_velocity        :: Word8
      , note_off_velocity       :: Word8
      , note_volume_level       :: Word8
      }
  deriving (Eq,Ord,Show)


newtype Build a = Build { getBuild :: BuildEnv -> a }

instance Functor Build where
  fmap f ma = Build $ \r -> f $ getBuild ma r


instance Applicative Build where
  pure a    = Build $ \_ -> a
  mf <*> ma = Build $ \r -> let f = getBuild mf r
                                a = getBuild ma r
                            in f a


instance Monad Build where
  return a  = Build $ \_ -> a
  m >>= k   = Build $ \r -> let a = getBuild m r in getBuild (k a) r
                          

newtype BuildT m a = BuildT { getBuildT :: BuildEnv -> m a }


instance Monad m => Functor (BuildT m) where
  fmap f ma = BuildT $ \r -> getBuildT ma r >>= \a -> return (f a)


instance Monad m => Applicative (BuildT m) where
  pure a    = BuildT $ \_ -> return a
  mf <*> ma = BuildT $ \r -> getBuildT mf r >>= \f -> 
                             getBuildT ma r >>= \a -> return (f a)
                       


instance Monad m => Monad (BuildT m) where
  return a  = BuildT $ \_ -> return a
  m >>= k   = BuildT $ \r -> getBuildT m r >>= \a -> getBuildT (k a) r
