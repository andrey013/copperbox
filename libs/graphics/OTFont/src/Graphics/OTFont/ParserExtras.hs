{-# LANGUAGE FlexibleContexts           #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.OTFont.ParserExtras
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

module Graphics.OTFont.ParserExtras where

import Graphics.OTFont.Datatypes
import Graphics.OTFont.ParserCombinators
import Graphics.OTFont.Utils

import Control.Applicative
import Data.Array.Unboxed
import Data.Bits
import Data.Int
import Data.Word

satisfies :: Monad m => ParserT m r a -> (a -> Bool) -> ParserT m r  a
satisfies p f = p >>= (\x -> if f x then return x else fail "satisfies")


chars :: Monad m => String -> ParserT m r String
chars s = mapM matchChar s 

matchChar :: Monad m => Char -> ParserT m r Char 
matchChar c = satisfies char (==c)


ushort :: Monad m => ParserT m r Word16
ushort = word16be

ulong :: Monad m => ParserT m r Word32
ulong = word32be

byte :: Monad m => ParserT m r Word8
byte = word8 


short :: Monad m => ParserT m r Int16
short = int16be 

fixed :: Monad m => ParserT m r Fixed 
fixed = mk <$> word16be <*> word16be where
    mk a b = Fixed $ (fromIntegral a) + ((fromIntegral b) / 10000)

-- TODO
fword :: Monad m => ParserT m r FWord 
fword = FWord <$> int16be 

ufword :: Monad m => ParserT m r UFWord 
ufword = UFWord <$> word16be 

bitfield :: (Bits a, Ord a, Unmarshal b, Monad m) => 
            ParserT m r a -> ParserT m r [b]
bitfield p = unbits <$> p 

longDateTime :: Monad m => ParserT m r DateTime
longDateTime = (\w -> DateTime w undefined) <$> word64be

usequence :: (IArray UArray a, Monad m) => 
             Int -> ParserT m r a -> ParserT m r (USequence a)
usequence i p = mkArr <$> count (fromIntegral i) p where
    mkArr xs = listArray (0,i-1) xs
    
bxsequence :: (IArray Array a, Monad m) => 
              Int -> ParserT m r a -> ParserT m r (BxSequence a)
bxsequence i p = mkArr <$> count i p where
    mkArr xs = listArray (0,i-1) xs


    