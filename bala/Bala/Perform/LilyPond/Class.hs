
--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Perform.LilyPond.LyInterface
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Typeclasses that must be implemented to emit LilyPond from Score.
--
--------------------------------------------------------------------------------

module Bala.Perform.LilyPond.Class where

import Bala.Format.Output.OutputLilyPond


class (Eq pch) => LilyPondPitch pch where
  middleC         :: pch
  
  octaveDist      :: pch -> pch -> Int
 
  mkPitchName     :: pch -> LyPitchName
  
  -- | No direct equivalent of 'natural' in LilyPond, hence the Maybe type
  mkAccidental    :: pch -> Maybe LyAccidental
  
  
  
class (Eq dur) => LilyPondDuration dur where
  quaternoteDuration  :: dur
  mkDuration          :: dur -> Maybe LyDuration
  
  