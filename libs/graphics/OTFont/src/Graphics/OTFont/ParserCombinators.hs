{-# LANGUAGE FlexibleContexts           #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.OTFont.ParserCombinators
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Read an otf file 
--
--------------------------------------------------------------------------------

module Graphics.OTFont.ParserCombinators where

import Graphics.OTFont.ParseMonad

import Control.Applicative
import Data.Array.IO
import Data.Array.Unboxed ( (!), bounds )
import Data.Bits
import Data.Char ( chr )
import Data.Int
import Data.Word
import System.IO 

type ParserM r a = ContState RAstate RAenv r a
 

    
runParser :: ParserM r r -> ByteSequence -> r
runParser m arr = runCS m (\c _st _env -> c) st arr where
  st = let (i,j) = bounds arr in position i j

runParserFile :: FilePath -> ParserM r r -> IO r 
runParserFile path p = withBinaryFile path ReadMode $ \h -> do
    sz_i  <- hFileSize h
    let abound = mkBounds sz_i
    marr   <- newArray_ abound 
    sz_r  <- hGetArray h marr (fromIntegral sz_i)
    arr   <- freezeByteSequence marr
    if sz_r == (fromIntegral sz_i) 
        then return $ runParser p arr
        else error $ "Problem with hGetArray" 

  where
    mkBounds :: Integral a => a -> (Int,Int)
    mkBounds i = (0, fromIntegral $ i - 1)
    
    freezeByteSequence :: IOUArray Int Word8 -> IO ByteSequence
    freezeByteSequence = freeze

--------------------------------------------------------------------------------
-- Primitive parser - word8 

word8 :: ParserM r Word8
word8 = do
    a <- input
    i <- absPosition
    let e = a!i
    movePos1
    return e
    
(<:>) :: Applicative f => f a -> f [a] -> f [a]
(<:>) p1 p2 = (:) <$> p1 <*> p2

count :: Applicative f => Int -> f a -> f [a]
count i p | i <= 0    = pure []
          | otherwise = p <:> count (i-1) p 
          
          
--------------------------------------------------------------------------------
-- Text parsers

char :: ParserM r Char
char = chr . fromIntegral <$> word8  

text :: Int -> ParserM r String
text i = count i char

    
--------------------------------------------------------------------------------
--  Number parsers

 

word64be   :: ParserM r Word64
word64be   = mkW64be <$> word8 <*> word8 <*> word8 <*> word8
                     <*> word8 <*> word8 <*> word8 <*> word8

word32be   :: ParserM r Word32
word32be   = mkW32be <$> word8 <*> word8 <*> word8 <*> word8
                         
word32le   :: ParserM r Word32
word32le   = mkW32le <$> word8 <*> word8 <*> word8 <*> word8

word16be   :: ParserM r Word16
word16be   = mkW16be <$> word8 <*> word8
                         
word16le   :: ParserM r Word16
word16le   = mkW16le <$> word8 <*> word8

int32be   :: ParserM r Int32
int32be   = mkI32be <$> word8 <*> word8 <*> word8 <*> word8
                         
int32le   :: ParserM r Int32
int32le   = mkI32le <$> word8 <*> word8 <*> word8 <*> word8

int16be   :: ParserM r Int16
int16be   = mkI16be <$> word8 <*> word8
                         
int16le   :: ParserM r Int16
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


mkI16le :: Word8 -> Word8 -> Int16
mkI16le a b = fromIntegral $ mkW16le a b
              
mkI32le :: Word8 -> Word8 -> Word8 -> Word8 -> Int32
mkI32le a b c d  = fromIntegral $ mkW32le a b c d                

               
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
    
    
    


mkI16be :: Word8 -> Word8 -> Int16
mkI16be a b = fromIntegral $ mkW16be a b
                            
mkI32be :: Word8 -> Word8 -> Word8 -> Word8 -> Int32
mkI32be a b c d = fromIntegral $ mkW32be a b c d
    