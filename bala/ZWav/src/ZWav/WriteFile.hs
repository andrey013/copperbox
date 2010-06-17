{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZWav.WriteFile
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Write WAV file.
--
--------------------------------------------------------------------------------

module ZWav.WriteFile 
  (
    -- * Write WAV files
  
    writeWav
  ) where

import ZWav.Datatypes

import Data.Binary.Put                  -- package: binary

import qualified Data.ByteString.Lazy   as L
import Data.Char
import Data.Word
import System.IO

writeWav :: FilePath -> WavFile -> IO ()
writeWav filename wav = 
    openBinaryFile filename WriteMode        >>= \hdl -> 
    L.hPut hdl (runPut $ wavFile wav)        >>
    hClose hdl


wavFile :: WavFile -> PutM ()
wavFile wav = 
       riffHeader   (chunk_size     wav) 
    >> wavFormat    (fmt_subchunk   wav)
    >> dataSubchunk (data_subchunk  wav)


riffHeader :: Word32 -> PutM ()
riffHeader sz = 
    put8BitString "RIFF" >> putWord32le sz >> put8BitString "WAVE"

wavFormat :: WavFormat -> PutM ()
wavFormat fmt = 
       put8BitString "fmt "
    >> putWord32le    (subchunk_size   fmt)  
    >> putWord16le    (audio_format    fmt)
    >> putWord16le    (num_channels    fmt)
    >> putWord32le    (sample_rate     fmt)
    >> putWord32le    (byte_rate       fmt)
    >> putWord16le    (block_align     fmt)
    >> putWord16le    (bits_per_sample fmt)
     
dataSubchunk :: DataSubchunk -> PutM ()
dataSubchunk dat = 
       put8BitString "data"
    >> putWord32le        (sub2_size       dat)
    >> putLazyByteString  (payload_data    dat) 


put8BitString :: String -> PutM ()
put8BitString = step where
    step []     = return ()
    step (s:ss) = putWord8 (fromIntegral $ ord s) >> step ss
  