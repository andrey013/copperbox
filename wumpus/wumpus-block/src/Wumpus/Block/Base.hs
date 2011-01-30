{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Block.Base
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Base objects - input and output ports on objects (like anchors).
--
--------------------------------------------------------------------------------

module Wumpus.Block.Base
  ( 

  -- * Block anchors
    Outport1(..)
  , Inport1(..)
  , Inport2(..)

  ) where

import Wumpus.Basic.Kernel              -- package: wumpus-basic
import Wumpus.Core                      -- package: wumpus-core


-- | \"Outport\" of an object - usually @south@.
--
class Outport1 t where
  outport1 :: DUnit t ~ u => t -> Point2 u



class Inport1 t where
  inport1  :: DUnit t ~ u => t -> Point2 u

class Inport2 t where
  inport2a :: DUnit t ~ u => t -> Point2 u
  inport2b :: DUnit t ~ u => t -> Point2 u

