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
  , getAtom
    
  , getInt32
  , getFloat32

  ) where


import Sound.Jerry.OSC.Datatypes

import Data.Binary.IEEE754                      -- package: data-binary-ieee754

import Control.Applicative
import Data.Binary.Get
import Data.Char

getPacket :: Get Packet
getPacket = getChar8 >>= step 
  where
    step '#' = stringLiteral "bundle" >> undefined
    step '/' = (\addr args -> Message ('/':addr) args) 
                 <$>  getOscString 1 <*> getArgs
    step _   = error "getPacket"
                    

getArgs :: Get [Atom]
getArgs = getTypeTags >>= mapM getAtom


getTypeTags :: Get [Char]
getTypeTags = charLiteral ',' >> getOscString 1


getAtom :: Char -> Get Atom
getAtom 'i' = Int32 <$> getInt32
getAtom 'f' = Float32 <$> getFloat32
getAtom _   = error "getAtom failed"

getChar8 :: Get Char 
getChar8 = (chr . fromIntegral) <$> getWord8


getInt32 :: Get Int
getInt32 = fromIntegral <$> getWord32be

getFloat32 :: Get Float
getFloat32 = getFloat32be


-- | Strings must be a multiple of 4 bytes - we may already have 
-- consumed the first byte.
--
getOscString :: Int -> Get String
getOscString i = getChar8 >>= step (i+1)
  where
    step n '\0' | n `mod` 4 == 0 = return []
    step n '\0'                  = charLiteral '\0' >>= step (n+1)
    step n ch   = (ch:) <$> (getChar8 >>= step (n+1))


charLiteral :: Char -> Get Char
charLiteral ch = fn <$> getChar8
  where
    fn a = if a == ch then a else error "getChar"

stringLiteral :: String -> Get String
stringLiteral = mapM charLiteral
    