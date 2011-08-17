{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Drawing.Extras.Clip
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Clipping paths.
--
-- Note - at the moment there is nothing much to this module.
-- Ideally, clipping would be defined in Wumpus-Basic, but clipping
-- needs a higher level path object than Wumpus-Basic provides.
-- 
--------------------------------------------------------------------------------

module Wumpus.Drawing.Extras.Clip
  ( 
   
    locClip

  ) where


import Wumpus.Drawing.Paths

import Wumpus.Basic.Kernel                      -- package: wumpus-basic




-- | Clip a LocGraphic.
--
locClip :: InterpretUnit u => AbsPath u -> LocGraphic u -> LocGraphic u
locClip absp gf = promoteLoc $ \pt -> 
    liftQuery (toPrimPath absp) >>= \pp -> clipImage pp (gf `at` pt)

