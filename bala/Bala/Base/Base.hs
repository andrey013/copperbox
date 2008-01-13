
--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Base.Base
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Import module for the base libs
-- |
--------------------------------------------------------------------------------


module Bala.Base.Base 
  ( module Bala.Base.PitchRep
  , module Bala.Base.PitchClass
  , module Bala.Base.PitchConversion
  , module Bala.Base.Interval
  , module Bala.Base.Triad
  -- do we want none 'music' libraries in Base?
  , module Bala.Base.BaseExtra
  )
  where
  
import Bala.Base.PitchRep
import Bala.Base.PitchClass
import Bala.Base.PitchConversion
import Bala.Base.Interval
import Bala.Base.Triad

import Bala.Base.BaseExtra

