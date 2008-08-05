
--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Base.HNotateInstances
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Pitch represention
--
--------------------------------------------------------------------------------

module Bala.Base.HNotateInstances (
  P.PitchRepr, 
  P.DurationRepr
  ) where
  
import Bala.Base.Pitch
import Bala.Base.Duration

import qualified HNotate.Base.Class as P
import qualified HNotate.Base.Datatypes as P

import Data.Ratio

instance P.PitchRepr Pitch where 
  renderPitch p = P.Pitch (transPitchLetter p) (transAccidental p) (transOctave p)
  
instance P.DurationRepr Duration where 
  renderDuration dur = let (d,dots) = unDuration dur
                           (num,den) = nr d
                       in P.Duration (num,den)

transPitchLetter :: Pitch -> P.PitchLetter  
transPitchLetter = fn . pitchLetter 
  where 
    fn C = P.C
    fn D = P.D 
    fn E = P.E 
    fn F = P.F 
    fn G = P.G 
    fn A = P.A 
    fn B = P.B
    
    

  
transAccidental :: Pitch -> P.Accidental
transAccidental = fn . pitchAccidental
  where
    fn DoubleFlat   = P.DoubleFlat
    fn Flat         = P.Flat  
    fn Nat          = P.Nat
    fn Sharp        = P.Sharp
    fn DoubleSharp  = P.DoubleSharp
  


transOctave :: Pitch -> Int
transOctave = octaveMeasure

nr :: (Integral a, Integral b) => Ratio a -> (b,b)
nr a = (fromIntegral $ numerator a, fromIntegral $ denominator a)