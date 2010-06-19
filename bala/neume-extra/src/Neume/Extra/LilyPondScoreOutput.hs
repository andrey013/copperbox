{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Neume.Extra.LilyPondScoreOutput
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Output the Score representation.
--
--------------------------------------------------------------------------------

module Neume.Extra.LilyPondScoreOutput
  (

    barNumber

  ) where

import Neume.Core.Utils.Pretty

import Text.PrettyPrint.Leijen          -- package: wl-pprint

-- TEMP - add to neueme-core
type BarNum = Int


-- | Default bar numbering function.
--
barNumber :: BarNum -> DocS
barNumber i = ((text $ "%% Bar " ++ show i) <$>)
