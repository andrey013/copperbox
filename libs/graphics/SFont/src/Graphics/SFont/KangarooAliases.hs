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


import Graphics.SFont.ExtraSyntax
import Graphics.SFont.PrimitiveDatatypes
import Graphics.SFont.Utils

import Data.ParserCombinators.KangarooState

import Control.Applicative
import Data.Bits
import Data.Int
import Data.Word



infixr 5 <:>

-- | applicative cons
(<:>) :: Applicative f => f a -> f [a] -> f [a]
(<:>) p1 p2 = (:) <$> p1 <*> p2


type FontParser a = Kangaroo TtffParseState a


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



ushort :: Kangaroo ust Word16
ushort = word16be

ulong :: Kangaroo ust Word32
ulong = word32be

byte :: Kangaroo ust Word8
byte = word8 


short :: Kangaroo ust Int16
short = int16be 

fixed :: Kangaroo ust Fixed 
fixed = mk <$> word16be <*> word16be where
    mk a b = Fixed $ (fromIntegral a) + ((fromIntegral b) / 10000)

-- TODO
fword :: Kangaroo ust FWord 
fword = FWord <$> int16be 

ufword :: Kangaroo ust UFWord 
ufword = UFWord <$> word16be 


f2dot14 :: Kangaroo ust F2Dot14
f2dot14 = F2Dot14 . fromIntegral <$> int16be


bitfield :: (Bits a, Ord a, Enum b) => Kangaroo ust a -> Kangaroo ust [b]
bitfield p = unbits <$> p 

longDateTime :: Kangaroo ust DateTime
longDateTime = (\w -> DateTime w undefined) <$> word64be


{-
usequence :: (IArray UArray a) => 
             Int -> Kangaroo ust a -> Kangaroo ust (USequence a)
usequence i p = mkArr <$> count (fromIntegral i) p where
    mkArr xs = listArray (0,i-1) xs
    
bxsequence :: (IArray Array a, Monad m) => 
              Int -> ParserT r m a -> ParserT r m (BxSequence a)
bxsequence i p = mkArr <$> count i p where
    mkArr xs = listArray (0,i-1) xs
-}

{-
-- TODO - these are the 'primal' operations that Kangaroo should 
-- be doing

-- absolute - absolute is more common (at least for fonts), hence it gets 
-- the shorthand alias
withinRange :: Int -> Int -> Kangaroo ust a -> Kangaroo ust a 
withinRange = withinRangeAbs


withinRangeAbs :: Int -> Int -> Kangaroo ust a -> Kangaroo ust a  
withinRangeAbs absval len p = do 
    error "withinRangeAbs"
    
    
-- relative
withinRangeRel :: Int -> Int -> Kangaroo ust a -> Kangaroo ust a  
withinRangeRel offset len p = do 
    error "withinRangeRel"
-}