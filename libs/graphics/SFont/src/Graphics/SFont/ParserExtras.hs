{-# LANGUAGE FlexibleContexts           #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.SFont.ParserExtras
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Font specific parsers
--
--------------------------------------------------------------------------------

module Graphics.SFont.ParserExtras where

import Graphics.SFont.PrimitiveDatatypes
-- import Graphics.SFont.ParseMonad
-- import Graphics.SFont.ParserCombinators 
import Graphics.SFont.Utils

import Control.Applicative
import Data.Array.Unboxed
import Data.Bits
import Data.Int
import Data.Word




ushort :: Monad m => ParserT r m Word16
ushort = word16be

ulong :: Monad m => ParserT r m Word32
ulong = word32be

byte :: Monad m => ParserT r m Word8
byte = word8 


short :: Monad m => ParserT r m Int16
short = int16be 

fixed :: Monad m => ParserT r m Fixed 
fixed = mk <$> word16be <*> word16be where
    mk a b = Fixed $ (fromIntegral a) + ((fromIntegral b) / 10000)

-- TODO
fword :: Monad m => ParserT r m FWord 
fword = FWord <$> int16be 

ufword :: Monad m => ParserT r m UFWord 
ufword = UFWord <$> word16be 


f2dot14 :: Monad m => ParserT r m F2Dot14
f2dot14 = F2Dot14 . fromIntegral <$> int16be


bitfield :: (Bits a, Ord a, Enum b, Monad m) => 
            ParserT r m a -> ParserT r m [b]
bitfield p = unbits <$> p 

longDateTime :: Monad m => ParserT r m DateTime
longDateTime = (\w -> DateTime w undefined) <$> word64be

usequence :: (IArray UArray a, Monad m) => 
             Int -> ParserT r m a -> ParserT r m (USequence a)
usequence i p = mkArr <$> count (fromIntegral i) p where
    mkArr xs = listArray (0,i-1) xs
    
bxsequence :: (IArray Array a, Monad m) => 
              Int -> ParserT r m a -> ParserT r m (BxSequence a)
bxsequence i p = mkArr <$> count i p where
    mkArr xs = listArray (0,i-1) xs


countPrefixedList :: (Integral i, Monad m) => 
    ParserT r m i -> ParserT r m a -> ParserT r m [a] 
countPrefixedList lenParser parser = do 
    len   <- lenParser
    count (fromIntegral len) parser