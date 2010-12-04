{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Graphic.AdvanceGraphic
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  highly unstable
-- Portability :  GHC 
--
-- Refined instances of of the Drawing type modelling specific
-- graphic types.
-- 
-- \*\* WARNING \*\* - some names are expected to change.
--
--------------------------------------------------------------------------------

module Wumpus.Basic.Graphic.AdvanceGraphic
  (



  -- * Advance vector
    AdvanceVec

  -- * Advance-vector graphic
  , AdvGraphic
  , DAdvGraphic

  -- * Extract from an Advance vector
  , advanceH
  , advanceV

  , makeAdvGraphic


  ) where

import Wumpus.Basic.Graphic.ContextFunction
import Wumpus.Basic.Graphic.GraphicTypes

import Wumpus.Core                      -- package: wumpus-core



type AdvanceVec u = Vec2 u










--------------------------------------------------------------------------------


-- | /Advance vector/ graphic - this partially models the 
-- PostScript @show@ command which moves the /current point/ by the
-- width (advance) vector as each character is drawn.
--
type AdvGraphic u      = LocImage u (Point2 u)

type DAdvGraphic       = AdvGraphic Double


type instance DUnit (AdvGraphic u) = u






--------------------------------------------------------------------------------

-- | Extract the horizontal component of an advance vector.
--
-- For left-to-right latin text, the vertical component of an
-- advance vector is expected to be 0. Ingoring it seems 
-- permissible, e.g. when calculating bounding boxes for 
-- left-to-right text.
--
advanceH :: Num u => AdvanceVec u -> u
advanceH (V2 w _)  = w

-- | Extract the verticaltal component of an advance vector.
--
-- For left-to-right latin text, the vertical component of an
-- advance vector is expected to be 0.
--
advanceV :: Num u => AdvanceVec u -> u
advanceV (V2 _ h)  = h




-- | Construction is different to intoZZ functions hence the 
-- different name.
--
makeAdvGraphic :: PointDisplace u
               -> LocGraphic u 
               -> AdvGraphic u
makeAdvGraphic pf df = postcomb1 (,) (postpro1 pf locPoint) df





