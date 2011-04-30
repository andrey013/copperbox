{-# LANGUAGE BangPatterns               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZSyn.WavOutput
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


module ZSyn.WavOutput
 

  where 

import ZSyn.Base
import ZSyn.HSStream
import ZSyn.Seconds
import ZSyn.WavHeader


import Control.Applicative
import Data.Array.IO  hiding ( hPutArray )
import Data.Bits
import Data.Char
import Data.Int
import Data.Word
import System.IO


type IOUWavArr = IOUArray Int Word8

-- | Constant
-- 
-- > be_RIFF = "RIFF"
--
be_RIFF   :: Word32
be_RIFF   = 0x52494646

-- | Constant
-- 
-- > be_WAVE = "WAVE"
--
be_WAVE   :: Word32
be_WAVE   = 0x57415645

-- | Constant
-- 
-- > be_FMT = "FMT "
--
be_FMT    :: Word32
be_FMT    = 0x666d7420

-- | Constant
-- 
-- > be_DATA = "DATA"
--
be_DATA   :: Word32
be_DATA   = 0x64617461


output_BUFFER_SIZE :: Int
output_BUFFER_SIZE = 1024



outputWav_1Chan_16Bit :: FilePath -> AudioStream -> Word32 -> Seconds -> IO ()
outputWav_1Chan_16Bit path strm sr len = withBinaryFile path WriteMode body
  where
    sz     = secondsToSamples (fromIntegral sr) len
    body h = do 
        hPutHeader h (makeWavHeader sz 1 sr 16)
        hPut1Ch_16Bit h strm sz

--------------------------------------------------------------------------------

-- | array-0.3.0.2 is buggy.
--
hPutArray :: Handle -> IOUArray Int Word8 -> Int -> IO ()
hPutArray handle arr count = do
    hds <- getBounds arr
    if count < 0 || count > rangeSize hds
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


--------------------------------------------------------------------------------

hPutHeader :: Handle -> WavHeader -> IO ()
hPutHeader h wavh = runBuilderH h 44 (headerBuild wavh)

headerBuild :: WavHeader -> Builder ()
headerBuild wavh = do
    putWord32be     be_RIFF
    putWord32le     (wav_size             wavh)
    putWord32be     be_WAVE
    putWord32be     be_FMT
    putWord32le     0x10
    putWord16le     (wav_audio_format     wavh)   
    putWord16le     (wav_num_channels     wavh)
    putWord32le     (wav_sample_rate      wavh)
    putWord32le     byte_rate
    putWord16le     block_align
    putWord16le     (wav_bits_per_sample  wavh)
    putWord32be     be_DATA
    putWord32le     (subtract 36 $ wav_size wavh)
  where
    num_chans   = fromIntegral $ wav_num_channels wavh
    sampr       = fromIntegral $ wav_sample_rate  wavh
    bits_per_s  = fromIntegral $ wav_bits_per_sample  wavh
    byte_rate   = floor $ (sampr * num_chans * bits_per_s) / (8::Double)
    block_align = floor $ (num_chans * bits_per_s) / (8::Double)




-- Builder is a reader-state monad in IO.
--
newtype Builder a = Builder { 
          getBuilder :: IOUWavArr -> Int -> IO (a, Int) }

runBuilderH :: Handle -> Int -> Builder a -> IO a
runBuilderH h arr_sz mf = do 
    arr    <- newArray (0, arr_sz) 0
    (a,_)  <- getBuilder mf arr 0
    hPutArray h arr arr_sz
    return a


runBuilderA :: Handle -> IOUWavArr -> Int -> Builder a -> IO a
runBuilderA h arr sz mf = do 
    (a,_)  <- getBuilder mf arr 0
    hPutArray h arr sz
    return a


instance Functor Builder where
  fmap f mf = Builder $ \ar !ix -> 
                getBuilder mf ar ix >>= \(a,ix1) -> return (f a, ix1)

instance Applicative Builder where
  pure a    = Builder $ \_  !ix -> return (a,ix)
  mf <*> ma = Builder $ \ar !ix -> getBuilder mf ar ix >>= \(f,ix1) ->
                                   getBuilder ma ar ix1 >>= \(a,ix2) -> 
                                   return (f a, ix2)  



instance Monad Builder where
  return a  = Builder $ \_  !ix -> return (a,ix)
  m >>= k   = Builder $ \ar !ix -> getBuilder m ar ix >>= \(a,ix1) ->
                                   getBuilder (k a) ar ix1



{-# INLINE putWord8 #-}
putWord8 :: Word8 -> Builder ()
putWord8 w = Builder $ \ar !ix -> writeArray ar ix w >> return ((), ix+1)

{-# INLINE putWord16le #-}
putWord16le :: Word16 -> Builder ()
putWord16le w = Builder $ \ar !ix -> do 
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
putWord16be w = Builder $ \ar !ix -> do 
    writeArray ar ix     (fromIntegral $ shiftR w 8) 
    writeArray ar (ix+1) (fromIntegral w) 
    return ((), ix+2)


{-# INLINE putWord32be #-}
putWord32be :: Word32 -> Builder ()
putWord32be w = Builder $ \ar !ix -> do 
    writeArray ar ix     (fromIntegral $ shiftR w 24) 
    writeArray ar (ix+1) (fromIntegral $ shiftR w 16) 
    writeArray ar (ix+2) (fromIntegral $ shiftR w 8) 
    writeArray ar (ix+3) (fromIntegral w) 
    return ((), ix+4)





imax :: Double
imax = fromIntegral (maxBound :: Int16)

imin :: Double 
imin = fromIntegral (minBound :: Int16)



-- Can only put Data.Word and not Data.Int, need to convert 
--
{-# INLINE toInt #-}
toInt :: Double -> Word16
toInt = conv . clamp (-1) 1 
  where
    conv :: Double -> Word16
    conv d | d >= 0    = floor   (d*imax)
           | otherwise = negate $ ceiling (d*imin)

    clamp :: Double -> Double -> Double -> Double
    clamp mi ma a = min ma (max mi a)



-- outputs 1 channel at 16 bit ...
--
hPut1Ch_16Bit :: Handle -> AudioStream -> Int -> IO ()
hPut1Ch_16Bit h ss len = do
    arr    <- newArray (0, output_BUFFER_SIZE) 0
    go arr len ss
  where
    half_SIZE                             = output_BUFFER_SIZE `div` 2

    go _   n _  | n <= 0                  = return ()

    go arr n s0 | n <= half_SIZE =
        runBuilderA h arr n (step 0 s0) >> return ()
      where
        step i _         | i >= n = return ()
        step i (a :< sa) = let x = toInt a in putWord16le x >> step (i+2) sa

    go arr n s0                           = do
        s1 <- runBuilderA h arr output_BUFFER_SIZE (step 0 s0)
        go arr (n - half_SIZE) s1
      where
        step i s         | i >= output_BUFFER_SIZE = return s
        step i (a :< sa) = let x = toInt a in putWord16le x >> step (i+2) sa
   
