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


import Control.Applicative
import Control.Monad
import Data.Bits
import Data.Char
import Data.Int
import Data.Word

--------------------------------------------------------------------------------
-- helpers

-- | applicative cons
(<:>) :: Applicative f => f a -> f [a] -> f [a]
(<:>) p1 p2 = (:) <$> p1 <*> p2

-- Wrong type should return a number...
inputRemaining :: GenKangaroo ust Int
inputRemaining = getSt >>= \(ArrIx ix end) -> 
   let rest = end - ix in if rest < 0 then return 0 else return rest

{-
jumpto :: Int -> GenKangaroo ust ()
jumpto n = getSt >>= \(ArrIx ix end) -> 
  let ix' 
-}

satisfy :: (Word8 -> Bool) -> GenKangaroo ust Word8
satisfy p = word8 >>= 
    (\x -> if p x then return x else reportError $ "satisfy...")



-- definable without peeking into the Kanagroo newtype?

opt :: GenKangaroo ust a -> GenKangaroo ust (Maybe a)
opt p = GenKangaroo $ \env st ust -> (getGenKangaroo p) env st ust >>= \ ans -> 
    case ans of
      (Left _, st', ust')  -> return (Right Nothing, st', ust')
      (Right a, st', ust') -> return (Right $ Just a, st', ust')

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

filePosition :: GenKangaroo ust Int
filePosition = liftM arr_ix_ptr  getSt


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



w16be :: Word8 -> Word8 -> Word16
w16be a b = (shiftL8 a) + fromIntegral b
     
            
w32be :: Word8 -> Word8 -> Word8 -> Word8 -> Word32
w32be a b c d = (shiftL24 a) + (shiftL16 b) + (shiftL8 c) + fromIntegral d

-- To do...
w64be :: Word8 -> Word8 -> Word8 -> Word8 -> 
           Word8 -> Word8 -> Word8 -> Word8 -> Word64
w64be a b c d e f g h = a' + b' + c' + d' + e' + f' + g' + h' where
    a' = (fromIntegral a) `shiftL` 54
    b' = (fromIntegral b) `shiftL` 48
    c' = (fromIntegral c) `shiftL` 40
    d' = (fromIntegral d) `shiftL` 32
    e' = (fromIntegral e) `shiftL` 24
    f' = (fromIntegral f) `shiftL` 16
    g' = (fromIntegral g) `shiftL` 8
    h' = (fromIntegral h) 


w16le :: Word8 -> Word8 -> Word16
w16le a b = fromIntegral a + (shiftL8 b)

w32le :: Word8 -> Word8 -> Word8 -> Word8 -> Word32
w32le a b c d = fromIntegral a + (shiftL8 b) + (shiftL16 c) + (shiftL24 d)      


-- Woah! These don't look right - what about the sign?

i16le :: Word8 -> Word8 -> Int16
i16le a b = fromIntegral $ w16le a b
              
i32le :: Word8 -> Word8 -> Word8 -> Word8 -> Int32
i32le a b c d  = fromIntegral $ w32le a b c d                


i16be :: Word8 -> Word8 -> Int16
i16be a b = fromIntegral $ w16be a b
                            
i32be :: Word8 -> Word8 -> Word8 -> Word8 -> Int32
i32be a b c d = fromIntegral $ w32be a b c d


shiftL8 :: (Bits b, Integral b) => Word8 -> b
shiftL8 = (`shiftL` 8) . fromIntegral


shiftL16 :: (Bits b, Integral b) => Word8 -> b
shiftL16 = (`shiftL` 16) . fromIntegral


shiftL24 :: (Bits b, Integral b) => Word8 -> b
shiftL24 = (`shiftL` 24) . fromIntegral

