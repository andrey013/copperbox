{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Sound.Jerry.OSC.Datatypes
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  unstable
-- Portability :  GHC (at least generalized newtype deriving)
--
-- /Printer/.
--
-- OSC is big-endian.
--
--------------------------------------------------------------------------------


module Sound.Jerry.OSC.WriteOSC
  (
     
    serializeOSC

  ) where


import Sound.Jerry.OSC.Datatypes

import Data.Binary.IEEE754                      -- package: data-binary-ieee754
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString      as S


import Data.Binary.Put
import Data.Char

serializeOSC :: Packet -> S.ByteString
serializeOSC = S.concat . L.toChunks . runPut . packet


packet :: Packet -> Put
packet (Message addr args) =
    alignedString addr >> arglist args >> mapM_ atom args
    
packet (Bundle tt es) = 
    alignedString "#bundle" >> timeTag tt >> mapM_ element es

arglist :: [Atom] -> Put
arglist = alignedString . (',':) . map typeTag

element :: Packet -> Put
element a = alignedByteString $ runPut (packet a)


atom :: Atom -> Put
atom (Int32 i)       = putInt32 i
atom (AtomTime t)    = timeTag t
atom (Float32 d)     = putFloat32 d
atom (String s)      = alignedString s
atom (Blob b)        = 
    putInt32 (fromIntegral $ L.length b) >> alignedByteString b
atom (Double64 d)    = putFloat64 d


-- | To do
timeTag :: TimeTag -> Put
timeTag (TimeTag a b) = putInt32 (fromIntegral a) >> putInt32 (fromIntegral b)


putChar8 :: Char -> Put
putChar8 = putWord8 . fromIntegral . ord


putInt32 :: Int -> Put
putInt32 = putWord32be . fromIntegral

putFloat32 :: Float -> Put
putFloat32 = putFloat32be

putFloat64 :: Double -> Put
putFloat64 = putFloat64be

-- | Strings are terminated with the null char. They may be padded
-- with extra null chars to align at 4 bytes.
--
alignedString :: String -> Put
alignedString ss = primString ss >> putCharZero >> addAlign (1 + length ss)


-- | ByteStrings are They may be padded with extra null chars to 
-- align at 4 bytes.
--
alignedByteString :: L.ByteString -> Put
alignedByteString ss = 
    putLazyByteString ss >> addAlign (fromIntegral $ L.length ss)


addAlign :: Int -> Put
addAlign n = go (n `mod` 4)
  where
    go 3 = putCharZero 
    go 2 = putCharZero >> putCharZero
    go 1 = putCharZero >> putCharZero >> putCharZero
    go _ = return ()




putCharZero :: Put
putCharZero = putChar8 '\0'

-- | No packing.
--
primString :: String -> Put
primString = mapM_ putChar8
