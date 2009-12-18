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
import Graphics.SFont.Utils

import Data.ParserCombinators.KangarooWriter

import Control.Applicative
import Data.Bits
import Data.Int
import Data.Word

type Log = String

infixr 5 <:>

-- | applicative cons
(<:>) :: Applicative f => f a -> f [a] -> f [a]
(<:>) p1 p2 = (:) <$> p1 <*> p2


type Parser a = Kangaroo Log a


-- To parse a ttf or otf file we need to see a lot of intermediate information
-- which isn't reflected in the final /parse tree/. We build this information
-- as we parse with the state monad. It doesn't seem very elegant to have 
-- all of the initial state fields undefined, but...   
data TtffParseState = TtffParseState 
        { table_count     :: Int
        , table_locs      :: TableLocs
        , glyph_locs      :: [Region]
        }
  deriving (Show)



ushort :: Parser Word16
ushort = word16be

ulong :: Parser Word32
ulong = word32be

byte :: Parser Word8
byte = word8 


short :: Parser Int16
short = int16be 

fixed :: Parser Fixed 
fixed = mk <$> word16be <*> word16be where
    mk a b = Fixed $ (fromIntegral a) + ((fromIntegral b) / 10000)

-- TODO
fword :: Parser FWord 
fword = FWord <$> int16be 

ufword :: Parser UFWord 
ufword = UFWord <$> word16be 


f2dot14 :: Parser F2Dot14
f2dot14 = F2Dot14 . fromIntegral <$> int16be


bitfield :: (Bits a, Ord a, Enum b) => Parser a -> Parser [b]
bitfield p = unbits <$> p 

longDateTime :: Parser DateTime
longDateTime = (\w -> DateTime w undefined) <$> word64be


{-
usequence :: (IArray UArray a) => Int -> Parser a -> Parser (USequence a)
usequence i p = mkArr <$> count (fromIntegral i) p where
    mkArr xs = listArray (0,i-1) xs
    
bxsequence :: (IArray Array a, Monad m) => 
              Int -> ParserT r m a -> ParserT r m (BxSequence a)
bxsequence i p = mkArr <$> count i p where
    mkArr xs = listArray (0,i-1) xs
-}

