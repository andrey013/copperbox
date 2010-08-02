{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE TypeFamilies               #-}
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
    dotCircle

  ) where

import Wumpus.Basic.Anchors
import qualified Wumpus.Basic.Dots as BD
import Wumpus.Basic.Graphic
import Wumpus.Basic.Monads.STraceMonad

import Wumpus.Core                      -- package: wumpus-core

import Data.AffineSpace                 -- package: vector-space
import MonadLib                         -- package: monadLib

import Control.Applicative




dotCircle :: ( Monad m, STraceM m (Primitive u)
             , Fractional u) 
          => BD.MarkAttr -> Point2 u -> m (Circle u)
dotCircle attr pt = strace (BD.dotCircle attr pt) >> 
               return (Circle (0.5*BD.markHeight attr) pt)

data Circle u = Circle 
      { circ_radius :: u
      , circ_ctr    :: Point2 u 
      }

type instance DUnit (Circle u) = u

instance CenterAnchor (Circle u) where
  center (Circle _ ctr) = ctr

instance Floating u => RadialAnchor (Circle u) where
  radialAnchor theta (Circle r ctr) = ctr .+^ (avec theta r)



{-

data RadialGraphic u = RadialGraphic 
      { rg_draw     :: GraphicF u
      , rg_anchor   :: Radian -> (Point2 u -> Point2 u)
      }

type RadialGraphicF u = Point2 u -> RadialGraphic u 


adotCircle :: Floating u => WD.MarkAttr -> RadialGraphic u
adotCircle attr = RadialGraphic { rg_draw   = dotCircle attr
                                , rg_anchor = anchorF }
  where
    anchorF theta ctr = radialAnchor theta (Circle (0.5*markHeight attr) ctr)

-}




{-

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

-}