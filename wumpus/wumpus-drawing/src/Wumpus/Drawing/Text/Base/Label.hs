{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Drawing.Text.Base.Label
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Annotation labels.
-- 
--------------------------------------------------------------------------------

module Wumpus.Drawing.Text.Base.Label
  ( 

    locImageLabel

  , label_center_of
  , label_left_of
  , label_right_of
  , label_above
  , label_below

  , connectorPathLabel
  , label_midway_of
  , label_atstart_of
  , label_atend_of

  , centerRelative
  , right_of
  , left_of
  , above_right_of
  , below_right_of
  , above_left_of
  , below_left_of

  ) where


import Wumpus.Drawing.Paths

import Wumpus.Basic.Kernel                      -- package: wumpus-basic

import Wumpus.Core                              -- package: wumpus-core


type BoundedLocRectGraphic u = RectAddress -> LocImage u (BoundingBox u)


locImageLabel :: InterpretUnit u 
              => (a -> Anchor u) 
              -> RectAddress 
              -> (RectAddress -> LocImage u (BoundingBox u)) 
              -> LocImage u a 
              -> LocImage u a
locImageLabel fn rpos mklabel obj = promoteLoc $ \pt -> 
    elaborateAbove (obj `at` pt)  (\a -> ignoreAns $ mklabel rpos `at` fn a)



label_center_of :: (InterpretUnit u, CenterAnchor a, u ~ DUnit a) 
                => BoundedLocRectGraphic u -> LocImage u a -> LocImage u a
label_center_of = locImageLabel center CENTER


label_left_of :: (InterpretUnit u, CardinalAnchor a, u ~ DUnit a) 
              => BoundedLocRectGraphic u -> LocImage u a -> LocImage u a
label_left_of = locImageLabel west EE

label_right_of :: (InterpretUnit u, CardinalAnchor a, u ~ DUnit a) 
               => BoundedLocRectGraphic u -> LocImage u a -> LocImage u a
label_right_of = locImageLabel east WW


label_above :: (InterpretUnit u, CardinalAnchor a, u ~ DUnit a) 
            => BoundedLocRectGraphic u -> LocImage u a -> LocImage u a
label_above = locImageLabel north SS


label_below :: (InterpretUnit u, CardinalAnchor a, u ~ DUnit a) 
            => BoundedLocRectGraphic u -> LocImage u a -> LocImage u a
label_below = locImageLabel south NN




connectorPathLabel :: InterpretUnit u 
                   => (AbsPath u -> Point2 u) 
                   -> RectAddress
                   -> BoundedLocRectGraphic u
                   -> Image u (AbsPath u) 
                   -> Image u (AbsPath u)
connectorPathLabel fn rpos lbl img =  
    elaborateAbove img  (\a -> ignoreAns $ lbl rpos `at` (fn a))


label_midway_of :: (Real u, Floating u, InterpretUnit u) 
                => RectAddress 
                -> BoundedLocRectGraphic u 
                -> Image u (AbsPath u) -> Image u (AbsPath u)
label_midway_of = connectorPathLabel midway_


label_atstart_of :: (Real u, Floating u, InterpretUnit u) 
                 => RectAddress 
                 -> BoundedLocRectGraphic u 
                 -> Image u (AbsPath u) -> Image u (AbsPath u)
label_atstart_of = connectorPathLabel atstart_


label_atend_of :: (Real u, Floating u, InterpretUnit u) 
               => RectAddress 
               -> BoundedLocRectGraphic u
               -> Image u (AbsPath u) -> Image u (AbsPath u)
label_atend_of = connectorPathLabel atend_




-- | Absolute units.
-- 
centerRelative :: (CenterAnchor a, Fractional u, InterpretUnit u, u ~ DUnit a) 
               => (Int,Int) -> a -> Query u (Anchor u)
centerRelative coord a = 
    snapmove coord >>= \v -> return $ displace v (center a)

-- TODO - These are really for Anchors.
--
-- Should the have a separate module or be rolled into the same
-- module as the classes?
--

-- | Value is 1 snap unit right.
--
-- This function should be considered obsolete, pending a 
-- re-think.
-- 
right_of        :: (CenterAnchor a, Fractional u, InterpretUnit u, u ~ DUnit a) 
                => a -> Query u (Anchor u)
right_of        = centerRelative (1,0)

-- | Value is 1 snap move left.
--
-- This function should be considered obsolete, pending a 
-- re-think.
-- 
left_of         :: (CenterAnchor a, Fractional u, InterpretUnit u, u ~ DUnit a) 
                => a -> Query u (Anchor u)
left_of         = centerRelative ((-1),0)

-- | Value is 1 snap move up, 1 snap move right.
--
-- This function should be considered obsolete, pending a 
-- re-think.
-- 
above_right_of  :: (CenterAnchor a, Fractional u, InterpretUnit u, u ~ DUnit a) 
                => a -> Query u (Anchor u)
above_right_of  = centerRelative (1,1)

-- | Value is 1 snap move below, 1 snap move right.
--
-- This function should be considered obsolete, pending a 
-- re-think.
-- 
below_right_of  :: (CenterAnchor a, Fractional u, InterpretUnit u, u ~ DUnit a) 
                => a -> Query u (Anchor u)
below_right_of  = centerRelative (1, (-1))

-- | Value is 1 snap move up, 1 snap move left.
--
-- This function should be considered obsolete, pending a 
-- re-think.
-- 
above_left_of   :: (CenterAnchor a, Fractional u, InterpretUnit u, u ~ DUnit a) 
                => a -> Query u (Anchor u)
above_left_of   = centerRelative ((-1),1)

-- | Value is 1 snap move down, 1 snap move left.
--
-- This function should be considered obsolete, pending a 
-- re-think.
-- 
below_left_of   :: (CenterAnchor a, Fractional u, InterpretUnit u, u ~ DUnit a) 
                => a -> Query u (Anchor u)
below_left_of   = centerRelative ((-1),(-1))
 



-- TikZ has label=below etc.
-- 
-- This would probably translate to a functions:
-- @labelBelow@
--


-- Design note - there aren\'t many Images that support anchors,
-- except for LocImages that have been /saturated/ (i.e. applied 
-- to a point with @at@).
-- 
-- For a saturated Image, getting at the anchors via bind does 
-- not seem so bad (indeed, this was the original point of 
-- anchors). Obviously it is important to add labels to LocImages
-- (the original point of the label functions) but what about 
-- LocThetaImages and LocRectImages. Is it acceptable to /saturate/
-- them to LocImages before labelling them?
-- 
-- Connectors support different /anchor-like/ positions so they 
-- will different labelling functions.
-- 
  

