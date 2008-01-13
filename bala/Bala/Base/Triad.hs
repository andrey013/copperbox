

--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Base.Triad
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Triad representation
-- |
--------------------------------------------------------------------------------


module Bala.Base.Triad where

import Bala.Base.PitchRep
import Bala.Base.BaseExtra

import Control.Applicative hiding (many, optional)
import Control.Monad (ap)
import Text.ParserCombinators.ReadP


newtype Triad = Triad {unTriad :: (Int,Int,Int)}

instance Read Triad where 
  readsPrec i s = readP_to_S readTriad s

readTriad :: ReadP Triad  
readTriad = undefined



