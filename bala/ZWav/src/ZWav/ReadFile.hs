{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZWav.ReadFile
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Read WAV file.
--
--------------------------------------------------------------------------------

module ZWav.ReadFile 
  (
    -- * Read WAV files
    readWav
  ) where

import ZWav.Datatypes

import Data.Binary.Get                  -- package: binary

import Control.Applicative
import qualified Data.ByteString        as S
import qualified Data.ByteString.Lazy   as L
import Data.Char
import Data.Word


readWav :: FilePath -> IO WavFile  
readWav filename = L.readFile filename >>= return . runGet wavFile


wavFile :: Get WavFile
wavFile = WavFile <$> riffHeader <*> wavFormat <*> dataSubchunk

riffHeader :: Get Word32
riffHeader = string "RIFF" *> getWord32le <* string "WAVE"


wavFormat :: Get WavFormat
wavFormat = do
   _            <- string "fmt "
   subc_sz      <- getWord32le             -- expect 16 for PCM
   afmt         <- getWord16le             -- 1 for PCM others for compressed
   chans        <- getWord16le
   sr           <- getWord32le
   b_rate       <- getWord32le
   b_align      <- getWord16le
   bits_ps      <- getWord16le
   _            <- dropBytes (subc_sz - 16)
   return $ WavFormat subc_sz afmt chans sr b_rate b_align bits_ps

dataSubchunk :: Get DataSubchunk
dataSubchunk = do
   _            <- string "data"
   sz           <- getWord32le
   payload      <- getLazyByteString (fromIntegral sz)
   return $ DataSubchunk sz payload


dropBytes :: Integral a => a -> Get ()
dropBytes n = getBytes (fromIntegral n) >> return ()


string :: String -> Get String
string s = getBytes (length s) >>= \bs ->
   if matchString s bs then return s else error $ "parseFailure " ++ s

matchString :: String -> S.ByteString -> Bool
matchString str w8s = step str (S.uncons w8s)
  where
    step (s:ss) (Just (w,bs))   | match1 s w = step ss (S.uncons bs)
    step []     Nothing                      = True
    step _      _                            = False
                          
    match1 :: Char -> Word8 -> Bool
    match1 c w = ord c == fromIntegral w 