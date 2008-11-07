
--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Base.Unfiled
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Experiments that have become orphaned (they no longer belong to their 
-- initial module)
--
--------------------------------------------------------------------------------

module Bala.Base.Unfiled where

import Bala.Base.BaseExtra
import Bala.Base.Pitch
import Bala.Base.Interval
import Bala.Base.NamedElems

data ParsonsCode = PaR | PaU | PaD    
  deriving (Eq,Ord,Show)
  
contour :: [Pitch] -> [ParsonsCode]  
contour = zam diff
  where diff a b = case a `compare` b of
                    EQ -> PaR
                    LT -> PaU
                    GT -> PaD
  
data RefinedContour = ReR | ReUS | ReUL | ReDS | ReDL
  deriving (Eq,Ord,Show)


-- | Pitch spelling not not same as Duckworth.  
circle_of_fifths :: [PitchLabel]
circle_of_fifths = map pitchName $ take 12 $ iterate (`extUp` perfect_fifth) c4


  
  
 