{-# LANGUAGE ForeignFunctionInterface   #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.FreeType.FixedPrecision
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Handle fixed numbers - 2dot14 and 26dot6.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.FreeType.FixedPrecision where

import Data.Data
import Data.Int
import Data.Ratio
import Text.Read

newtype F2d14 = F2d14 { getF2d14 :: Int16 }
  deriving ( Enum, Eq, Data, Num, Ord, Typeable )

newtype F26d6 = F26d6 { getF26d6 :: Int32 }
  deriving ( Enum, Eq, Data, Num, Ord,Typeable )

f2d14 :: Integral a => a -> a -> F2d14
f2d14 a b = F2d14 $ fromIntegral $ a * b

f26d6 :: Integral a => a -> a -> F26d6
f26d6 a b = F26d6 $ fromIntegral $ a * b
  


--------------------------------------------------------------------------------
-- Read and Show
  
instance Show F2d14 where
  show (F2d14 r) = show $ (toRational r) 

instance Show F26d6 where
  show (F26d6 r) = show $ (toRational r) 

instance Read F2d14 where 
  readPrec = do 
      (d::Double) <- readPrec
      return (F2d14 $ round $ 64.0 * approxRational d ((1e-24)/2))


instance Read F26d6 where 
  readPrec = do 
      (d::Double) <- readPrec
      return (F26d6 $ round $ 64.0 * approxRational d ((1e-12)/2))
