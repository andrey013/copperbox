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
textline :: InterpretUnit u => String -> RectAddress -> BoundedLocGraphic u
textline ss addr = runPosObjectBBox (posText ss) addr


-- | Note - this is likely to be moved too...
--
rtextline :: (Real u, Floating u, InterpretUnit u)
          => Radian -> String -> RectAddress -> BoundedLocGraphic u
rtextline ang ss addr = runPosObjectBBox (rposText ang ss) addr


multilineText :: (Fractional u, InterpretUnit u)
              => VAlign -> String -> RectAddress -> BoundedLocGraphic u
multilineText va ss addr = runPosObjectBBox (multilinePosText va ss) addr


