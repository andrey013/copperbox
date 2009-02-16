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

satisfies :: ParserM r a -> (a -> Bool) -> ParserM r  a
satisfies p f = p >>= (\x -> if f x then return x else fail "satisfies")


chars :: String -> ParserM r String
chars s = mapM matchChar s 

matchChar :: Char -> ParserM r Char 
matchChar c = satisfies char (==c)


ushort :: ParserM r Word16
ushort = word16be

ulong :: ParserM r Word32
ulong = word32be

byte :: ParserM r Word8
byte = word8 


short :: ParserM r Int16
short = int16be 

fixed :: ParserM r Fixed 
fixed = mk <$> word16be <*> word16be where
    mk a b = Fixed $ (fromIntegral a) + ((fromIntegral b) / 10000)

-- TODO
fword :: ParserM r FWord 
fword = FWord <$> int16be 

ufword :: ParserM r UFWord 
ufword = UFWord <$> word16be 

bitfield :: (Bits a, Ord a, Unmarshal b) => ParserM r a -> ParserM r [b]
bitfield p = unbits <$> p 

longDateTime :: ParserM r DateTime
longDateTime = (\w -> DateTime w undefined) <$> word64be

usequence :: IArray UArray a => Int -> ParserM r a -> ParserM r (USequence a)
usequence i p = mkArr <$> count (fromIntegral i) p where
    mkArr xs = listArray (0,i-1) xs
    
bxsequence :: IArray Array a => Int -> ParserM r a -> ParserM r (BxSequence a)
bxsequence i p = mkArr <$> count i p where
    mkArr xs = listArray (0,i-1) xs


    