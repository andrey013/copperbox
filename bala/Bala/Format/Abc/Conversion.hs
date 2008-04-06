
--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Format.Abc.Conversion
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Conversion to and from ABC format
--
--------------------------------------------------------------------------------

module Bala.Format.Abc.Conversion  where

import Bala.Format.Abc.Datatypes hiding (Accidental(..) )
import qualified Bala.Format.Abc.Datatypes as Abc
import Bala.Base.Base

import Data.Char

instance EncodePitch PitchSpec where
  fromPitch (Pitch (PitchLabel l a) o s _) = 
    let alt = Nothing -- case valueAlt s of Abc.Natural -> Nothing ; a' -> Just a'
        (ch,ov) = valuePch l o
    in PitchSpec ch alt ov

  toPitch (PitchSpec ch oa oo) =
    let (pl,ove) = pchValue ch oo
        alt      = maybe Nat altValue oa 
    in Pitch (PitchLabel pl alt) ove 0  0

pchValue 'A' ove = (A, oveValue ove)
pchValue 'B' ove = (B, oveValue ove)
pchValue 'C' ove = (C, oveValue ove)
pchValue 'D' ove = (D, oveValue ove)
pchValue 'E' ove = (E, oveValue ove)
pchValue 'F' ove = (F, oveValue ove)
pchValue 'G' ove = (G, oveValue ove)
pchValue 'a' ove = (A, 1 + oveValue ove)
pchValue 'b' ove = (B, 1 + oveValue ove)
pchValue 'c' ove = (C, 1 + oveValue ove)
pchValue 'd' ove = (D, 1 + oveValue ove)
pchValue 'e' ove = (E, 1 + oveValue ove)
pchValue 'f' ove = (F, 1 + oveValue ove)
pchValue 'g' ove = (G, 1 + oveValue ove)
pchValue ch  ove = error $ "unrecognized pitch " ++ show ch

valuePch A ove = (chdisp 'A' ove, ovdisp ove)
valuePch B ove = (chdisp 'B' ove, ovdisp ove)
valuePch C ove = (chdisp 'C' ove, ovdisp ove)
valuePch D ove = (chdisp 'D' ove, ovdisp ove)
valuePch E ove = (chdisp 'E' ove, ovdisp ove)
valuePch F ove = (chdisp 'F' ove, ovdisp ove)
valuePch G ove = (chdisp 'G' ove, ovdisp ove)

chdisp ch i | i > 4     = toLower ch
            | otherwise = toUpper ch

ovdisp i | i < 4            = Just (OctaveLow  $ 4 - i)
         | i > 5            = Just (OctaveHigh $ i - 5)
         | otherwise        = Nothing

oveValue = maybe 4 f
  where f (OctaveLow i)  = 4 - i
        f (OctaveHigh i) = 4 + i

altValue :: Abc.Accidental -> Accidental
altValue (Abc.Natural)      = Nat
altValue (Abc.Sharp)        = Sharpi 1
altValue (Abc.DoubleSharp)  = Sharpi 2       
altValue (Abc.Flat)         = Flati 1 
altValue (Abc.DoubleFlat)   = Flati 2


valueAlt :: Accidental -> Abc.Accidental
valueAlt Nat          = Abc.Natural
valueAlt (Sharpi 1)   = Abc.Sharp 
valueAlt (Sharpi 2)   = Abc.DoubleSharp 
valueAlt (Flati 1)    = Abc.Flat 
valueAlt (Flati 2)    = Abc.DoubleFlat
valueAlt _            = undefined

     