{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.SFont.ParserCombinators
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

module Graphics.SFont.ParserCombinators where

import Graphics.SFont.ParseMonad

import Control.Applicative
-- import Control.Monad.Error


import Data.Bits
import Data.Char ( chr )
import Data.Int
import qualified Data.Sequence as S
import Data.Word




(<:>) :: Applicative f => f a -> f [a] -> f [a]
(<:>) p1 p2 = (:) <$> p1 <*> p2

count :: Applicative f => Int -> f a -> f [a]
count i p | i <= 0    = pure []
          | otherwise = p <:> count (i-1) p 
          

satisfies :: Monad m => ParserT r m a -> (a -> Bool) -> ParserT r m  a
satisfies p f = p >>= 
    (\x -> if f x then return x else throwError $ strMsg "satisfies")


chars :: Monad m => String -> ParserT r m String
chars s = mapM matchChar s 

matchChar :: Monad m => Char -> ParserT r m Char 
matchChar c = satisfies char (==c)


runOnL :: Monad m => ParserT r m a -> ParserT r m [a]
runOnL p = do i <- inputRemaining
              if i == 0 then return []
                        else p <:> runOnL p
             -- an error will have been thrown if (i<0)
 
(<<|>) :: Applicative f => f a -> f (S.Seq a) -> f (S.Seq a)
(<<|>) p1 p2 = (S.<|) <$> p1 <*> p2


runOnS :: Monad m => ParserT r m a -> ParserT r m (S.Seq a)
runOnS p = do i <- inputRemaining
              if i == 0 then return S.empty
                        else p <<|> runOnS p
             -- an error will have been thrown if (i<0)
          
--------------------------------------------------------------------------------
-- Text parsers

char :: Monad m => ParserT r m Char
char = chr . fromIntegral <$> word8  

text :: Monad m => Int -> ParserT r m String
text i = count i char

pascalString :: Monad m => ParserT r m String
pascalString = do 
    i <- word8 
    count (fromIntegral i) char

     
--------------------------------------------------------------------------------
--  Number parsers

word8 :: Monad m => ParserT r m Word8
word8 = getWord8
 

word64be   :: Monad m => ParserT r m Word64
word64be   = mkW64be <$> word8 <*> word8 <*> word8 <*> word8
                     <*> word8 <*> word8 <*> word8 <*> word8

word32be   :: Monad m => ParserT r m Word32
word32be   = mkW32be <$> word8 <*> word8 <*> word8 <*> word8
                         
word32le   :: Monad m => ParserT r m Word32
word32le   = mkW32le <$> word8 <*> word8 <*> word8 <*> word8

word16be   :: Monad m => ParserT r m Word16
word16be   = mkW16be <$> word8 <*> word8
                         
word16le   :: Monad m => ParserT r m Word16
word16le   = mkW16le <$> word8 <*> word8

int32be   :: Monad m => ParserT r m Int32
int32be   = mkI32be <$> word8 <*> word8 <*> word8 <*> word8
                         
int32le   :: Monad m => ParserT r m Int32
int32le   = mkI32le <$> word8 <*> word8 <*> word8 <*> word8

int16be   :: Monad m => ParserT r m Int16
int16be   = mkI16be <$> word8 <*> word8
                         
int16le   :: Monad m => ParserT r m Int16
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
    