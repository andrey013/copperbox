


--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Base.Perform
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Interval representation
-- |
--------------------------------------------------------------------------------



module Bala.Base.Interval where

import Bala.Base.PitchRep

data NamedInterval = Unison | Second | Third | Fourth | Fifth | Sixth
                   | Seventh | Octave
  deriving (Eq,Enum,Ord,Show)

data IntervalQuality = Perfect | Major | Minor | Augmented | Diminished
  deriving (Eq,Enum,Ord,Show)

data IntervalSize = Simple | Compound
  deriving (Eq,Enum,Ord,Show)


-- mspan 
mspan :: Pitch -> Pitch -> Int
mspan pch1 pch2 = 1 + mod7 (7 + p2 - p1) + (7 * od )
  where fn = fromEnum . pitch
        p1 = fn pch1
        p2 = fn pch2
        od = octave pch2 - octave pch1
         
