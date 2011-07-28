{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Majalan.Symbolic.Scale.Centaur
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Kraig Grady\'s 12-tone Centaur scale.
--
--------------------------------------------------------------------------------

module Majalan.Symbolic.Scale.Centaur
  ( 

    Centaur

  , fromFrac
  , hertz

  , centaurToOctave

  ) where

-- Note - this representation avoids pitch (and interval) naming
-- as we are only generating numerical values...

import Majalan.Symbolic.Basis.Z12
import Majalan.Symbolic.Scale.Base



-- | Pitch-class representation - octave and pitch-class
-- 
data Centaur = Centaur Int Z12
  deriving (Eq,Ord)

instance Show Centaur where
  showsPrec _ (Centaur o i) = shows o . showChar '.' . fn i
    where
      fn n | n < 10    = showChar '0' . shows n
           | otherwise = shows n 
 

instance HertzValue Centaur where
  hertz = octaveToHertz . centaurToOctave


instance FromFrac Centaur where
  fromFrac d = Centaur (o + a) (fromIntegral i)
    where
      (o,f) = properFraction d
      i0    = round $ f * 100
      (a,i) = i0 `divMod` 12

centaurToOctave :: Centaur -> Double
centaurToOctave (Centaur o i) = y + (fn i - 1.0)
  where
    y = fromIntegral o  
    fn  0 = 1/1
    fn  1 = 21/20
    fn  2 = 9/8
    fn  3 = 7/6
    fn  4 = 5/4
    fn  5 = 4/3
    fn  6 = 7/5 
    fn  7 = 3/2
    fn  8 = 14/9
    fn  9 = 5/3
    fn 10 = 7/4
    fn 11 = 15/8
    fn  _ = error "Centaur - unreachable (modolu 12)"