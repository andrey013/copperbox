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

import Data.ParserCombinators.KangarooRWS

import Data.DList ( DList, append )
import Control.Applicative
import Data.Bits
import Data.Int
import Data.Word

type Log = DList String

infixr 5 <:>

-- | applicative cons
(<:>) :: Applicative f => f a -> f [a] -> f [a]
(<:>) p1 p2 = (:) <$> p1 <*> p2


type FontParser a = Kangaroo () Log TtffParseState a


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



ushort :: FontParser Word16
ushort = word16be

ulong :: FontParser Word32
ulong = word32be

byte :: FontParser Word8
byte = word8 


short :: FontParser Int16
short = int16be 

fixed :: FontParser Fixed 
fixed = mk <$> word16be <*> word16be where
    mk a b = Fixed $ (fromIntegral a) + ((fromIntegral b) / 10000)

-- TODO
fword :: FontParser FWord 
fword = FWord <$> int16be 

ufword :: FontParser UFWord 
ufword = UFWord <$> word16be 


f2dot14 :: FontParser F2Dot14
f2dot14 = F2Dot14 . fromIntegral <$> int16be


bitfield :: (Bits a, Ord a, Enum b) => FontParser a -> FontParser [b]
bitfield p = unbits <$> p 

longDateTime :: FontParser DateTime
longDateTime = (\w -> DateTime w undefined) <$> word64be


{-
usequence :: (IArray UArray a) => 
             Int -> FontParser a -> FontParser (USequence a)
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
withinRange :: Int -> Int -> FontParser a -> FontParser a 
withinRange = withinRangeAbs


withinRangeAbs :: Int -> Int -> FontParser a -> FontParser a  
withinRangeAbs absval len p = do 
    error "withinRangeAbs"
    
    
-- relative
withinRangeRel :: Int -> Int -> FontParser a -> FontParser a  
withinRangeRel offset len p = do 
    error "withinRangeRel"
-}