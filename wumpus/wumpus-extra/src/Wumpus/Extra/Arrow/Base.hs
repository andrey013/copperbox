{-# LANGUAGE FlexibleContexts           #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Extra.Arrow.Base
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC with TypeFamilies and more
--
-- Arrowheads...
--
--------------------------------------------------------------------------------

module Wumpus.Extra.Arrow.Base
  (
    LineWidth 
  , ArrowTip(..)

  ) where

import Wumpus.Core
import Wumpus.Extra.Shape.Base

type LineWidth u = u

-- End point extension has to be a distance not a vector, 
-- so it can be used on curves as well as lines...


data ArrowTip u = ArrowTip 
       { arrowtip_right_extend  :: LineWidth u -> u
       , arrowtip_drawing       :: LineWidth u -> Radian -> Point2 u -> Composite u
       }