{-# LANGUAGE FlexibleContexts           #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.SFont.KangarooAliases
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Map to the terminolgy of the Font documentation rather than 
-- Kangaroo.
--
--------------------------------------------------------------------------------

module Graphics.SFont.KangarooAliases where


import Graphics.SFont.Syntax

import Data.ParserCombinators.KangarooWriter

import Control.Applicative
import Data.Int
import Data.Word

type Log = String

type Parser a = Kangaroo Log a

logPos :: String -> Parser a -> Parser a
logPos msg p = do 
  p1 <- position 
  a  <- p 
  p2 <- position
  tell $ unwords ["\n", show p1, "--", msg, "--", show p2]
  return a


logline :: String -> Parser ()
logline = tell . ("\n" ++)


tellPos :: String -> Parser ()
tellPos str = region >>= \(_,p,e) ->
  tell $ "\n" ++ str ++ ", position " ++ show p ++ ", region end " ++ show e

-- give 'position' a visually distinct alias
currentParsePosition :: Parser Int
currentParsePosition = position


--------------------------------------------------------------------------------


uint8 :: Parser Word8
uint8 = word8

uint16 :: Parser Word16
uint16 = word16be

uint32 :: Parser Word32
uint32 = word32be

int16 :: Parser Int16
int16 = int16be

int32 :: Parser Int32
int32 = int32be

-- 16 bit signed integer
fword :: Parser FWord 
fword = FWord <$> int16be 


fixed :: Parser Fixed 
fixed = mk <$> word16be <*> word16be where
    mk a b = Fixed $ (fromIntegral a) + ((fromIntegral b) / 10000)




-- TODO
ufword :: Parser UFWord 
ufword = UFWord <$> word16be 

-- TODO 
f2dot14 :: Parser F2Dot14
f2dot14 = F2Dot14 . fromIntegral <$> int16be

-- TODO
longDateTime :: Parser DateTime
longDateTime = DateTime <$> word64be

