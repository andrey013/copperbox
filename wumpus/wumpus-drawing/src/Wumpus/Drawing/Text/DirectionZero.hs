{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Drawing.DirectionZero
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Common import module for the Writing Direction 0 modules
-- 
--------------------------------------------------------------------------------

module Wumpus.Drawing.Text.DirectionZero
  ( 
    module Wumpus.Drawing.Text.Base.DocTextZero

  , textline
  , rtextline
  , multilineText

  ) where

import Wumpus.Drawing.Text.Base.DocTextZero

import Wumpus.Basic.Kernel                      -- package: wumpus-basic

import Wumpus.Core                              -- package: wumpus-core

-- | Note - this is likely to be moved...
-- 
-- Also, reversed argument order would be more convenient as 
-- RectAddress always short but String could be long. 
--
textline :: InterpretUnit u => RectAddress -> String -> BoundedLocGraphic u
textline addr ss = runPosObjectBBox addr (posText ss)


-- | Note - this is likely to be moved too...
--
rtextline :: (Real u, Floating u, InterpretUnit u)
          => Radian -> RectAddress -> String -> BoundedLocGraphic u
rtextline ang addr ss = runPosObjectBBox addr (rposText ang ss)


multilineText :: (Fractional u, InterpretUnit u)
              => VAlign -> RectAddress -> String -> BoundedLocGraphic u
multilineText va addr ss = runPosObjectBBox addr (multilinePosText va ss)


