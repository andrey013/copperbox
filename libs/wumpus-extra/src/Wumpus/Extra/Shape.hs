{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Extra.Shape
-- Copyright   :  (c) Stephen Tetley 2009-2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC with TypeFamilies and more
--
-- Drawable graphics objects (coordinate points, rectangles...)
-- 
--------------------------------------------------------------------------------

module Wumpus.Extra.Shape
  ( 

    module Wumpus.Extra.Shape.Base
  , module Wumpus.Extra.Shape.Circle
  , module Wumpus.Extra.Shape.Coordinate
  , module Wumpus.Extra.Shape.Diamond
  , module Wumpus.Extra.Shape.FreeLabel
  , module Wumpus.Extra.Shape.Rectangle


  ) where


import Wumpus.Extra.Shape.Base
import Wumpus.Extra.Shape.Circle
import Wumpus.Extra.Shape.Coordinate
import Wumpus.Extra.Shape.Diamond
import Wumpus.Extra.Shape.FreeLabel
import Wumpus.Extra.Shape.Rectangle


