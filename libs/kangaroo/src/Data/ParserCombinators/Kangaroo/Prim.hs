{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Data.ParserCombinators.Kangaroo.Prim
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Primitives
--
--------------------------------------------------------------------------------

module Data.ParserCombinators.Kangaroo.Prim where

import Data.ParserCombinators.Kangaroo.ParseMonad
import Data.ParserCombinators.Kangaroo.Utils

import Control.Applicative
import Data.Char
import Data.Int
import Data.Word

--------------------------------------------------------------------------------
-- helpers



satisfy :: (Word8 -> Bool) -> GenKangaroo ust Word8
satisfy p = word8 >>= 
    (\x -> if p x then return x else reportError $ "satisfy...")



-- definable without peeking into the Kanagroo newtype?


manyTill :: GenKangaroo ust a -> GenKangaroo ust b -> GenKangaroo ust [a]
manyTill p end = do 
   ans <- opt end
   case ans of
     Just _ -> return []
     Nothing -> p <:> manyTill p end 


runOn :: GenKangaroo ust a -> GenKangaroo ust [a]
runOn p = do at_end <- atEnd
             if at_end then return [] else  p <:> runOn p


-- | Read a null-terminated string
cstring :: GenKangaroo ust String
cstring = manyTill char w8Zero


w8Zero :: GenKangaroo ust Word8
w8Zero = satisfy (==0)

getBytes :: Integral a => a -> GenKangaroo ust [Word8]
getBytes i = count (fromIntegral i) word8

char :: GenKangaroo ust Char
char = (chr . fromIntegral) <$> word8 

text :: Int -> GenKangaroo ust String
text i = count i char

getChar8bit :: GenKangaroo ust Char
getChar8bit = (chr . fromIntegral) <$> word8 



count :: Int -> GenKangaroo ust a -> GenKangaroo ust [a]
count i p = step i [] where
  step n xs  | n <= 0     = return (reverse xs)
             | otherwise  = p >>= \a -> step (n-1) (a:xs)
             


countPrefixed :: Integral i 
              => GenKangaroo ust i -> GenKangaroo ust a -> GenKangaroo ust [a] 
countPrefixed plen p = plen >>= \i -> count (fromIntegral i) p


int8 :: GenKangaroo ust Int8
int8 = (fromIntegral . unwrap) <$> word8
  where
    unwrap :: Word8 -> Int
    unwrap i | i > 128   = (fromIntegral i) - 256
             | otherwise = fromIntegral i

word16be   :: GenKangaroo ust Word16
word16be   = w16be     <$> word8 <*> word8  

word32be   :: GenKangaroo ust Word32
word32be   = w32be     <$> word8 <*> word8 <*> word8 <*> word8

word64be   :: GenKangaroo ust Word64
word64be   = w64be <$> word8 <*> word8 <*> word8 <*> word8
                   <*> word8 <*> word8 <*> word8 <*> word8

word16le   :: GenKangaroo ust Word16
word16le   = w16le     <$> word8 <*> word8  

word32le   :: GenKangaroo ust Word32
word32le   = w32le     <$> word8 <*> word8 <*> word8 <*> word8

  
int32be   :: GenKangaroo ust Int32
int32be   = i32be <$> word8 <*> word8 <*> word8 <*> word8
                         
int32le   :: GenKangaroo ust Int32
int32le   = i32le <$> word8 <*> word8 <*> word8 <*> word8

int16be   :: GenKangaroo ust Int16
int16be   = i16be <$> word8 <*> word8
                         
int16le   :: GenKangaroo ust Int16
int16le   = i16le <$> word8 <*> word8

--------------------------------------------------------------------------------
-- helpers



