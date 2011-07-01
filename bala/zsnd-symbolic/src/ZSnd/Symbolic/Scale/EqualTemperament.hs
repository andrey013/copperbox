{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZSnd.Symbolic.Scale.EqualTemperament
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- 
--
--------------------------------------------------------------------------------

module ZSnd.Symbolic.Scale.EqualTemperament
  ( 

    ET12
  , pitchC

  , hertz
  , fromFrac

  , octaveToHertz
  , et12ToOctave

  , Iv

  ) where

-- Note - this representation avoids pitch (and interval) naming
-- as we are only generating numerical values...


import ZSnd.Symbolic.Basis.Z12
import ZSnd.Symbolic.Scale.Base

import Data.AffineSpace                         -- package: vector-space


-- | Pitch-class representation - octave and pitch-class
-- 
data ET12 = ET12 Int Z12
  deriving (Eq,Ord)

instance Show ET12 where
  showsPrec _ (ET12 o i) = shows o . showChar '.' . fn i
    where
      fn n | n < 10    = showChar '0' . shows n
           | otherwise = shows n 
 

instance HertzValue ET12 where
  hertz = octaveToHertz . et12ToOctave


pitchC :: Int -> Z12 -> ET12
pitchC o i = ET12 o i


{-

et12Hz :: ET12 -> Double
et12Hz (ET12 o i) = 
    (2.0 ** (od + (0.08333333 * (fromIntegral i)))) * 1.021975
  where
    od = fromIntegral o
-}

et12ToOctave :: ET12 -> Double
et12ToOctave (ET12 o i) = y + (z / 12)
  where
    y = fromIntegral o  
    z = fromIntegral i


instance FromFrac ET12 where
  fromFrac d = ET12 (o + a) (fromIntegral i)
    where
      (o,f) = properFraction d
      i0    = round $ f * 100
      (a,i) = i0 `divMod` 12
    




semitoneDiff :: ET12 -> ET12 -> Iv
semitoneDiff (ET12 o0 i0) (ET12 o1 i1) = idif + odif
  where
    odif = fromIntegral $ o1 - o0
    idif = if i1 > i0 then fromIntegral (i1 - i0) 
                      else negate $ fromIntegral (i0 - i1)

semitoneAdd :: ET12 -> Iv -> ET12
semitoneAdd (ET12 o i) iv = ET12 (o + o') (i + fromIntegral i')
  where
    a        = fromIntegral iv
    (o', i') = a `divMod` 12

instance AffineSpace ET12 where
  type Diff ET12 = Iv
  (.-.) = semitoneDiff
  (.+^) = semitoneAdd