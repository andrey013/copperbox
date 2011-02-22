{-# LANGUAGE ViewPatterns               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Syn.OutputIOArray
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Output to 16 bit audio
--
--------------------------------------------------------------------------------


module Syn.OutputIOArray
 

  where 

import Syn.Stream

import Control.Applicative
import Data.Array.IO  hiding ( hPutArray )
import Data.Array.Unboxed
import Data.Bits
import Data.Char
import Data.Int
import Data.Word
import System.IO


hPutArray :: Handle -> IOUArray Int Word8 -> Int -> IO ()
hPutArray handle arr count = do
	bds <- getBounds arr
	if count < 0 || count > rangeSize bds
	   then illegalBufferSize handle "hPutArray" count
	   else put 0
 where
  put i | i == count = return ()
	| otherwise = do
		w <- readArray arr i
		hPutChar handle (chr (fromIntegral w))
		put (i+1)

illegalBufferSize :: Handle -> String -> Int -> IO a
illegalBufferSize _ fn sz = ioError $
	userError (fn ++ ": illegal buffer size " ++ showsPrec 9 (sz::Int) [])



temp_runBuilder :: Int -> Builder a -> IO (UArray Int Word8)
temp_runBuilder sz act = do 
    arr <- newArray (0,sz) 0
    _ <- getBuilder act arr 0
    freeze arr

outputWAV :: Int -> FilePath -> Stream Double -> IO ()
outputWAV len path strm = withBinaryFile path WriteMode body
  where
    payload_sz = subChunk2Size (fromIntegral len) 1
    total_sz   = 36 + payload_sz
    header     = riffHeader total_sz >> wavFormat 1 >> dataHeader payload_sz

    body h = do 
        arr <- newArray (0,arr_SIZE) 0
        _   <- getBuilder header arr 0
        hPutArray h arr 44
        owb1_16 h arr strm len
    
-- outputs 1 channel at 16 bit ...
--
owb1_16 :: Handle -> IOUWavArr -> Stream Double -> Int -> IO ()
owb1_16 h arr = go
  where
    half_size         = arr_SIZE `div` 2      
    go ss i
      | i <= 0        = return ()
      | i < half_size = getBuilder (buildChunk ss) arr 0 >> hPutArray h arr (i*2)
      | otherwise     = getBuilder (buildChunk ss) arr 0 >>= \(ss1,_) -> 
                            hPutArray h arr 1024 >> go ss1 (i - half_size)

-- All we have is hPutArray

type IOUWavArr = IOUArray Int Word8



arr_SIZE :: Int
arr_SIZE = 1024



-- Builder is just a state monad.
--
newtype Builder a = Builder { 
          getBuilder :: IOUWavArr -> Int -> IO (a, Int) }


instance Functor Builder where
  fmap f mf = Builder $ \ar ix -> 
                getBuilder mf ar ix >>= \(a,ix1) -> return (f a, ix1)

instance Applicative Builder where
  pure a    = Builder $ \_  ix -> return (a,ix)
  mf <*> ma = Builder $ \ar ix -> getBuilder mf ar ix >>= \(f,ix1) ->
                                  getBuilder ma ar ix1 >>= \(a,ix2) -> 
                                  return (f a, ix2)  



instance Monad Builder where
  return a  = Builder $ \_  ix -> return (a,ix)
  m >>= k   = Builder $ \ar ix -> getBuilder m ar ix >>= \(a,ix1) ->
                                  getBuilder (k a) ar ix1

{-# INLINE putWord8 #-}
putWord8 :: Word8 -> Builder ()
putWord8 w = Builder $ \ar ix -> writeArray ar ix w >> return ((), ix+1)

{-# INLINE putWord16le #-}
putWord16le :: Word16 -> Builder ()
putWord16le w = Builder $ \ar ix -> do 
    writeArray ar ix     (fromIntegral w) 
    writeArray ar (ix+1) (fromIntegral $ shiftR w 8) 
    return ((), ix+2)

{-# INLINE putWord32le #-}
putWord32le :: Word32 -> Builder ()
putWord32le w = Builder $ \ar ix -> do 
    writeArray ar ix     (fromIntegral w) 
    writeArray ar (ix+1) (fromIntegral $ shiftR w 8) 
    writeArray ar (ix+2) (fromIntegral $ shiftR w 16) 
    writeArray ar (ix+3) (fromIntegral $ shiftR w 24) 
    return ((), ix+4)


{-# INLINE putWord16be #-}
putWord16be :: Word16 -> Builder ()
putWord16be w = Builder $ \ar ix -> do 
    writeArray ar ix     (fromIntegral $ shiftR w 8) 
    writeArray ar (ix+1) (fromIntegral w) 
    return ((), ix+2)


{-# INLINE putWord32be #-}
putWord32be :: Word32 -> Builder ()
putWord32be w = Builder $ \ar ix -> do 
    writeArray ar ix     (fromIntegral $ shiftR w 24) 
    writeArray ar (ix+1) (fromIntegral $ shiftR w 16) 
    writeArray ar (ix+2) (fromIntegral $ shiftR w 8) 
    writeArray ar (ix+3) (fromIntegral w) 
    return ((), ix+4)





be_RIFF   :: Word32
be_RIFF   = 0x52494646

be_WAVE   :: Word32
be_WAVE   = 0x57415645

be_FMT    :: Word32
be_FMT    = 0x666d7420

be_DATA   :: Word32
be_DATA   = 0x64617461


riffHeader :: Word32 -> Builder ()
riffHeader sz = do
    putWord32be be_RIFF
    putWord32le sz
    putWord32be be_WAVE

-- | nc = num channels
--
wavFormat :: Int -> Builder ()
wavFormat nc = do
    putWord32be         be_FMT
    putWord32le         16
    putWord16le         1                       -- 1 == PCM
    putWord16le         (fromIntegral nc)       -- 1 mono, 2 stereo
    putWord32le         44100
    putWord32le         byte_rate
    putWord16le         block_align
    putWord16le         16
  where
    ncd         = fromIntegral nc
    byte_rate   = floor $ (44100 * ncd * 16) / (8::Double)
    block_align = floor $ ncd * 16 / (8::Double)



dataHeader :: Word32 -> Builder ()
dataHeader sz = do 
    putWord32be         be_DATA
    putWord32le         sz
 


bits_PER_SAMPLE :: Word32
bits_PER_SAMPLE = 16

subChunk2Size :: Word32 -> Int -> Word32
subChunk2Size len nc = (len * fromIntegral nc * bits_PER_SAMPLE) `div` 8

     

imax :: Double
imax = fromIntegral (maxBound :: Int16)

imin :: Double 
imin = fromIntegral (minBound :: Int16)



-- Can only put Data.Word and not Data.Int, need to convert 
--
{-# INLINE toInt #-}
toInt :: Double -> Word16
toInt = go . clamp (-1) 1 
  where
    go :: Double -> Word16
    go d | d >= 0    = floor   (d*imax)
         | otherwise = negate $ ceiling (d*imin)

    clamp :: Double -> Double -> Double -> Double
    clamp mi ma a = min ma (max mi a)


buildChunk :: Stream Double -> Builder (Stream Double)
buildChunk = go 0 
  where
    go i s         | i == arr_SIZE = return s
    go i (a :< sa) = let n = toInt a in putWord16le n >> go (i+2) sa

