{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GADTs                      #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.AnchorDots
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC with TypeFamilies, GADTs and more
--
-- Dots with anchors.
--
--------------------------------------------------------------------------------

module Wumpus.Basic.AnchorDots
  ( 


  -- * Dots with anchor points
    adotSquare
  , adotCircle

  ) where

import Wumpus.Basic.Anchors
import Wumpus.Basic.Dots
import Wumpus.Basic.Graphic

import Wumpus.Core                      -- package: wumpus-core


-- ADots support radial and center anchor

-- Note - is this any better than shapes which have different 
-- types but a /unified/ type class interface?
-- 
data ADot u = forall t. (CenterAnchor t, DUnit t ~ u) => ADot
      { dot_drawf    :: MarkAttr -> GraphicF u
      , dot_repr     :: t
      }

data Square u = Sq1 deriving (Eq,Show)

type instance DUnit (Square u) = u
type instance DUnit (ADot u)   = u
 
instance Num u => CenterAnchor (Square u) where
  center (Sq1) = P2 0 0 

instance CenterAnchor (ADot u) where
  center (ADot {dot_repr=body}) = center body


adotSquare :: Fractional u => ADot u
adotSquare = ADot { dot_drawf = dotSquare, dot_repr = Sq1 }


data Circle u = Cir1 deriving (Eq,Show)

type instance DUnit (Circle u) = u

instance Num u => CenterAnchor (Circle u) where
  center (Cir1) = P2 0 0 

adotCircle :: Fractional u => ADot u
adotCircle = ADot { dot_drawf = dotCircle, dot_repr = Cir1 }

