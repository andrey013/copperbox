{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Text.ZParse.Binary
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Combinators - ...
--
--------------------------------------------------------------------------------


module Text.ZParse.Binary where

import Text.ZParse.Combinators
import Text.ZParse.ParseMonad
import Text.ZParse.SrcPos


import Control.Applicative
import Control.Monad.State
import Data.Bits
import Data.Char
import Data.Int
import Data.Word

import qualified Data.ByteString as BS

instance ParseState (StreamPosn,BS.ByteString) StreamPosn BS.ByteString where
    pos                   = fst
    setPos p (_,r)        = (p,r)
    remaining             = snd 
    setRemaining r (p,_)  = (p,r) 

type BinaryParserT m a = ParserT (StreamPosn,BS.ByteString) m a

--------------------------------------------------------------------------------
--


chars :: Monad m => String -> BinaryParserT m String 
chars s = mapM char s 


char :: Monad m => Char -> BinaryParserT m Char 
char c = satisfies anychar (==c)


text :: Monad m => Int -> BinaryParserT m String
text i = count i anychar

bytestring :: Int -> BinaryParserT m BS.ByteString
bytestring i = do 
    inp            <- input
    StreamPosn p   <- position
    let (a,b) = BS.splitAt i inp
    updateState (StreamPosn $ p+i) b
    return a
  
flush :: BinaryParserT m BS.ByteString
flush = do 
    inp           <- input
    StreamPosn p  <- position
    updateState (StreamPosn $ p + BS.length inp) BS.empty
    return inp
    
    
  

--------------------------------------------------------------------------------
--
word8 :: Monad m => BinaryParserT m Word8
word8 = do
    bs <- input
    case BS.uncons bs of
        Nothing    -> fail "Unexpected eof" 
        Just (a,b) -> do { p <- position 
                         ; updateState (nextPos undefined p) b
                         ; return a }

anychar :: Monad m => BinaryParserT m Char
anychar = chr . fromIntegral <$> word8  

word64be   :: Monad m => BinaryParserT m Word64
word64be   = mkW64be <$> word8 <*> word8 <*> word8 <*> word8
                     <*> word8 <*> word8 <*> word8 <*> word8

word32be   :: Monad m => BinaryParserT m Word32
word32be   = mkW32be <$> word8 <*> word8 <*> word8 <*> word8
                         
word32le   :: Monad m => BinaryParserT m Word32
word32le   = mkW32le <$> word8 <*> word8 <*> word8 <*> word8

word16be   :: Monad m => BinaryParserT m Word16
word16be   = mkW16be <$> word8 <*> word8
                         
word16le   :: Monad m => BinaryParserT m Word16
word16le   = mkW16le <$> word8 <*> word8

int32be   :: Monad m => BinaryParserT m Int32
int32be   = mkI32be <$> word8 <*> word8 <*> word8 <*> word8
                         
int32le   :: Monad m => BinaryParserT m Int32
int32le   = mkI32le <$> word8 <*> word8 <*> word8 <*> word8

int16be   :: Monad m => BinaryParserT m Int16
int16be   = mkI16be <$> word8 <*> word8
                         
int16le   :: Monad m => BinaryParserT m Int16
int16le   = mkI16le <$> word8 <*> word8



mkW16le :: Word8 -> Word8 -> Word16
mkW16le a b = a' + b' where
    a' = (fromIntegral a)
    b' = (fromIntegral b) `shiftL` 8
             
mkW32le :: Word8 -> Word8 -> Word8 -> Word8 -> Word32
mkW32le a b c d = a' + b' + c' + d' where
    a' = (fromIntegral a) 
    b' = (fromIntegral b) `shiftL` 8
    c' = (fromIntegral c) `shiftL` 16
    d' = (fromIntegral d) `shiftL` 24


-- For Data.Bits - 0 is lsb, 7 is msb

-- 7 or 0 ?
mkI8le :: Word8 -> Int16
mkI8le a = if a `testBit` 7 then fromIntegral a 
                            else 1 - fromIntegral (0xFF .&. a)
                      
-- 15 or 0 ?
mkI16le :: Word8 -> Word8 -> Int16
mkI16le a b = let v = mkW16le a b in
    if v `testBit` 15 then fromIntegral v 
                      else 1 - fromIntegral (0xFFFF .&. v)

-- 31 or 0 ?                
mkI32le :: Word8 -> Word8 -> Word8 -> Word8 -> Int32
mkI32le a b c d  = let v = mkW32le a b c d in
    if v `testBit`31 then fromIntegral v 
                     else 1 - fromIntegral (0xFFFFFFFF .&. v)                  

               
mkW16be :: Word8 -> Word8 -> Word16
mkW16be a b = a' + b' where
    a' = (fromIntegral a) `shiftL` 8
    b' = (fromIntegral b) 
             
mkW32be :: Word8 -> Word8 -> Word8 -> Word8 -> Word32
mkW32be a b c d = a' + b' + c' + d' where
    a' = (fromIntegral a) `shiftL` 24
    b' = (fromIntegral b) `shiftL` 16
    c' = (fromIntegral c) `shiftL` 8
    d' = (fromIntegral d) 

mkW64be :: Word8 -> Word8 -> Word8 -> Word8 -> 
           Word8 -> Word8 -> Word8 -> Word8 -> Word64
mkW64be a b c d e f g h = a' + b' + c' + d' + e' + f' + g' + h' where
    a' = (fromIntegral a) `shiftL` 54
    b' = (fromIntegral b) `shiftL` 48
    c' = (fromIntegral c) `shiftL` 40
    d' = (fromIntegral d) `shiftL` 32
    e' = (fromIntegral e) `shiftL` 24
    f' = (fromIntegral f) `shiftL` 16
    g' = (fromIntegral g) `shiftL` 8
    h' = (fromIntegral h) 
    
    
    
mkI8be :: Word8 -> Int16
mkI8be a = if a `testBit` 7 then fromIntegral a 
                            else 1 - fromIntegral (0xFF .&. a)
                      

mkI16be :: Word8 -> Word8 -> Int16
mkI16be a b = let v = mkW16be a b in
    if v `testBit` 15 then fromIntegral v 
                      else 1 - fromIntegral (0xFFFF .&. v)
                            
mkI32be :: Word8 -> Word8 -> Word8 -> Word8 -> Int32
mkI32be a b c d  = let v = mkW32be a b c d in
    if v `testBit` 31 then fromIntegral v 
                      else 1 - fromIntegral (0xFFFFFFFF .&. v) 
                       