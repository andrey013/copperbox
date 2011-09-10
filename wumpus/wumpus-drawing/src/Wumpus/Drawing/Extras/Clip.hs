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
-- TODO - names need improving.
--
--------------------------------------------------------------------------------

module Wumpus.Drawing.Extras.Clip
  ( 
   
    locClip

  ) where


import Wumpus.Drawing.Paths

import Wumpus.Basic.Kernel                      -- package: wumpus-basic

import Wumpus.Core                              -- package: wumpus-core



-- | Clip a LocGraphic.
--
locClip :: InterpretUnit u => AbsPath u -> LocGraphic u -> LocGraphic u
locClip absp gf = promoteLoc $ \pt -> 
    liftQuery (toPrimPath absp) >>= \pp -> clipImage pp (gf `at` pt)



{-

-- | Clip a LocGraphic.
--
locClipH :: (Floating u, Ord u, InterpretUnit u, Tolerance u) 
         => HPath u -> LocGraphic u -> LocGraphic u
locClipH hp gf = promoteLoc $ \pt -> 
    liftQuery (toPrimPath $ runHPath hp pt) >>= \pp -> clipImage pp (gf `at` pt)

-}