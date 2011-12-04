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
    

    writePacket
  , serializePacket 
  , putAtom
    
  , putInt32
  , putOscString

  ) where


import Sound.Jerry.OSC.Datatypes

import Data.Binary.IEEE754                      -- package: data-binary-ieee754
import qualified Data.ByteString.Lazy as L


import Data.Binary.Put
import Data.Char

writePacket :: Packet -> L.ByteString
writePacket = runPut . putPacket

serializePacket :: Packet -> String
serializePacket = map (chr . fromIntegral) . L.unpack . writePacket


putPacket :: Packet -> Put
putPacket (Message addr args) = do
    putOscString addr
    putOscString (',':tags)
    mapM_ putAtom args
  where
    tags = map typeTag args
    
putPacket (Bundle) = error "putPacket"



putAtom :: Atom -> Put
putAtom (Int32 i)   = putInt32 i
putAtom (TimeTag )  = undefined
putAtom (Float32 d) = putFloat32 d
putAtom (String ss) = undefined



putChar8 :: Char -> Put
putChar8 = putWord8 . fromIntegral . ord


putInt32 :: Int -> Put
putInt32 = putWord32be . fromIntegral

putFloat32 :: Float -> Put
putFloat32 = putFloat32be


-- | The String must be padded to a multiple of 4 bytes.
--
putOscString :: String -> Put
putOscString = step (0::Int)
  where
    step n (s:ss) = putChar8 s >> step (n+1) ss
    step n []     = putChar8 '\0' >> finalize ((n+1) `mod` 4)
  
    finalize 1 = putChar8 '\0' >> putChar8 '\0' >> putChar8 '\0'
    finalize 2 = putChar8 '\0' >> putChar8 '\0'
    finalize 3 = putChar8 '\0'
    finalize _ = return ()
