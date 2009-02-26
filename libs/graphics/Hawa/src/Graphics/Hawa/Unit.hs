{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Hawa.Unit
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- a unit 1/72 of an inch 
--
--------------------------------------------------------------------------------

module Graphics.Hawa.Unit where


newtype Unit = Unit { getUnit :: Double }
  deriving (Eq,Fractional,Num,Ord,Read,Real,RealFrac,Show)


whole :: Unit -> Bool
whole = (==0) . snd . propFrac . getUnit


propFrac :: RealFrac a => a -> (Int, a)
propFrac = properFraction
