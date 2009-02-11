{-# LANGUAGE FlexibleContexts           #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.OTFont.Parse
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

module Graphics.OTFont.Parse where

import Graphics.OTFont.Datatypes
import Graphics.OTFont.Utils

import Text.ZParse

import Control.Applicative
import Data.Array.Unboxed
import Data.Bits
import qualified Data.ByteString as BS
import Data.Int
import qualified Data.Map as Map
import Data.Word

protoFace :: Monad m => BinaryParserT m ProtoFace
protoFace = do 
    inp                         <- input      -- cache the original bytestring
    ot@(OffsetTable _ i _ _ _)  <- offsetTable
    dirs                        <- count (fromIntegral i) tableDirectory
    let m = extractTableStreams dirs inp
    return (ProtoFace ot dirs m) 
    
     
offsetTable :: Monad m => BinaryParserT m OffsetTable 
offsetTable = OffsetTable 
    <$> (o1oo <|> otto) <*> word16be <*> word16be <*> word16be <*> word16be
  where
    otto :: Monad m => BinaryParserT m String 
    otto = chars "OTTO"
    
    o1oo :: Monad m => BinaryParserT m String 
    o1oo = chars ['\0','\1','\0','\0']
    
    
tableDirectory :: Monad m => BinaryParserT m TableDirectory 
tableDirectory = TableDirectory 
    <$> text 4 <*> word32be <*> word32be <*> word32be

extractTableStreams :: [TableDirectory] -> BS.ByteString -> TableStreams
extractTableStreams ts inp = foldr fn Map.empty ts
  where
    fn (TableDirectory name _ o l) fm = 
        Map.insert name (section (fromIntegral o) (fromIntegral l) inp) fm
   
    
 
        

--------------------------------------------------------------------------------
-- NameTable





--------------------------------------------------------------------------------
-- 

ushort :: Monad m => BinaryParserT m Word16
ushort = word16be

ulong :: Monad m => BinaryParserT m Word32
ulong = word32be

byte :: Monad m => BinaryParserT m Word8
byte = word8 

char :: Monad m => BinaryParserT m Char 
char = anychar  

short :: Monad m => BinaryParserT m Int16
short = int16be 

fixed :: Monad m => BinaryParserT m Fixed 
fixed = mk <$> word16be <*> word16be where
    mk a b = Fixed $ (fromIntegral a) + ((fromIntegral b) / 10000)

-- TODO
fword :: Monad m => BinaryParserT m FWord 
fword = FWord <$> int16be 

ufword :: Monad m => BinaryParserT m UFWord 
ufword = UFWord <$> word16be 



bitfield :: (Monad m, Bits a, Ord a, Unmarshal b) 
         => BinaryParserT m a -> BinaryParserT m [b]
bitfield p = unbits <$> p 

longDateTime :: Monad m => BinaryParserT m DateTime
longDateTime = (\w -> DateTime w undefined) <$> word64be 

array :: (Monad m, Integral idx, Ix idx, IArray Array a) =>
        idx -> BinaryParserT m a -> BinaryParserT m (Array idx a)
array i p = mkArr <$> count (fromIntegral i) p where
    mkArr xs = listArray (0,i-1) xs

uarray :: (Monad m, Integral idx, Ix idx, IArray UArray a) =>
        idx -> BinaryParserT m a -> BinaryParserT m (UArray idx a)
uarray i p = mkArr <$> count (fromIntegral i) p where
    mkArr xs = listArray (0,i-1) xs
  
type ReadTable m a = BinaryParserT m a
type ReadData m a = BinaryParserT m a
