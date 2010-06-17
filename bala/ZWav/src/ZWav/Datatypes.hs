{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZWav.Datatypes
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Datatypes.
--
--------------------------------------------------------------------------------


module ZWav.Datatypes 
  (
    -- * Wav file representation
    WavFile(..)
  , WavFormat(..)
  , DataSubchunk(..)
  ) where

import Data.ByteString.Lazy ( ByteString )
import Data.Word

data WavFile = WavFile 
      { chunk_size              :: Word32 
      , fmt_subchunk            :: WavFormat
      , data_subchunk           :: DataSubchunk
      }
  deriving (Eq,Show)


data WavFormat = WavFormat 
      { subchunk_size           :: Word32
      , audio_format            :: Word16
      , num_channels            :: Word16
      , sample_rate             :: Word32
      , byte_rate               :: Word32
      , block_align             :: Word16
      , bits_per_sample         :: Word16
      }
  deriving (Eq,Show) 

data DataSubchunk = DataSubchunk
      { sub2_size               :: Word32
      , payload_data            :: ByteString
      }
  deriving (Eq)

instance Show DataSubchunk where
  show (DataSubchunk sz _) = "(Datasubchunk " ++ show sz ++ " ...)"