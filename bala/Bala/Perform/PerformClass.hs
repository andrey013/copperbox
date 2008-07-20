{-# OPTIONS_GHC -XMultiParamTypeClasses #-}
{-# OPTIONS_GHC -XFlexibleInstances #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Perform.Perform
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Type class for `performing` a music representation
--
--------------------------------------------------------------------------------


module Bala.Perform.PerformClass where

import Bala.Base

class (Show evt) => Perform evt where
  opitch     :: evt -> Maybe Pitch
  oduration  :: evt -> Maybe Duration
  
   


