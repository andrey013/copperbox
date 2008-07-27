{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Perform.Base.Class
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Type classes for `performing` a music representation
-- The Score renderer and backends are parametrized on pitch and duration
-- So the can output for other music representations not just Bala.
--
--------------------------------------------------------------------------------

module Bala.Perform.Base.Class where


class (Show evt) => Perform evt pch dur | evt -> pch, evt -> dur where
  eventvalues     :: evt -> (Maybe pch, Maybe dur)


-- Score representation 

class ScoreDuration a where
  toDouble      :: a -> Double
  fromDouble    :: Double -> a
  
  