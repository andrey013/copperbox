
--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Base.Scale
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- A datatypes for representing pitch
-- |
--------------------------------------------------------------------------------

module Bala.Base.Scale where

data ScaleDegrees = Tonic | SuperTonic | Mediant | Subdominant | Dominant
                  | Submediant | LeadingTone 
  deriving (Eq)
  
                    