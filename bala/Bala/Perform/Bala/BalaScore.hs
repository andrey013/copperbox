--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Perform.Bala.BaseScore
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Use datatypes from Bala.Base libraries to implement the Score interfaces.
--
--------------------------------------------------------------------------------


module Bala.Perform.Bala.BalaScore where

import Bala.Base
import Bala.Format.Score.Class
import Bala.Perform.Base.Class

instance ScoreDuration Duration where
  toDouble      = durationSize 
  fromDouble    = rationalDuration . toRational 
  
instance Printable Pitch where
  stringrep pch = afficher pch
  
instance Printable Duration where
  stringrep dur = afficher dur
  
   
  


