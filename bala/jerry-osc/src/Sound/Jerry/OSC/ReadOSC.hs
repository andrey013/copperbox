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
-- /Parser/.
--
-- OSC is big-endian.
--
--------------------------------------------------------------------------------


module Sound.Jerry.OSC.ReadOSC
  (
    
    getPacket

  ) where


import Sound.Jerry.OSC.Datatypes

import Data.Binary.IEEE754                      -- package: data-binary-ieee754

import Control.Applicative
import Data.Binary.Get
import qualified Data.ByteString.Lazy as L
import Data.Char

-- Bundles require testing for empty.

getPacket :: Get Packet
getPacket = getChar8 >>= go
  where
    go '#' = Bundle  <$> (stringLiteral "bundle" >> timeTag) <*> bundleElements
    go '/' = Message <$> addressRest <*> arglist
    go ch  = fail $ "getPacket - unrecognized initial char " ++ [ch]


-- | '/' already consumed.
-- 
addressRest :: Get String
addressRest = primString >>= go
  where
    go ss = dropAlign (1 + length ss) >> return ('/':ss)


                    
-- | @#bundle@ and TimeTag already consumed.
--
bundleElements :: Get [Packet]
bundleElements = remaining >>= go
  where
    go n | n > 0  = (:) <$> sizedGet n getPacket <*> bundleElements
    go _          = return []

-- | /Fork/ a parser, running it on fixed sized input.
--
sizedGet :: Integral n => n -> Get a -> Get a
sizedGet n pf = getLazyByteString (fromIntegral n) >>= go
  where
    go ss = return $ runGet pf ss

-- | To do - TimeTag currently not interpreted.
--
timeTag :: Get TimeTag
timeTag = TimeTag <$> getWord32be <*> getWord32be

arglist :: Get [Atom]
arglist = typeTags >>= mapM atom


typeTags :: Get [Char]
typeTags = charLiteral ',' >> oscString

-- | Parse an atom according to type tag.
--
atom :: Char -> Get Atom
atom 'i' = Int32 <$> getInt32
atom 'f' = Float32 <$> getFloat32
atom 's' = String <$> oscString
atom 'b' = Blob <$> oscBlob
atom _   = fail "getAtom failed"

getChar8 :: Get Char 
getChar8 = (chr . fromIntegral) <$> getWord8


getInt32 :: Get Int
getInt32 = fromIntegral <$> getWord32be

getFloat32 :: Get Float
getFloat32 = getFloat32be


-- | Strings must be a multiple of 4 bytes.
--
oscString :: Get String
oscString = primString >>= \ss -> dropAlign (length ss) >> return ss

oscBlob :: Get L.ByteString
oscBlob = 
    getInt32                            >>= \i  -> 
    getLazyByteString (fromIntegral i)  >>= \ss -> 
    dropAlign i                         >> 
    return ss

dropAlign :: Int -> Get ()
dropAlign i = go (i `mod` 4)
  where
    go 3 = skip 1
    go 2 = skip 2
    go 1 = skip 3
    go _ = return ()


-- | Just get a string upto the null terminator. Don\'t worry 
-- about packing to 4 byte boundary.
--
primString :: Get String
primString = getChar8 >>= go
  where
    go '\0' = return []
    go ch   = (ch:) <$> (getChar8 >>= go)

charLiteral :: Char -> Get Char
charLiteral ch = fn <$> getChar8
  where
    fn a = if a == ch then a else error "getChar"

stringLiteral :: String -> Get String
stringLiteral = mapM charLiteral
    