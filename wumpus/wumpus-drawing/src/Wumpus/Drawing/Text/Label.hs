{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Drawing.Text.Label
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

module Wumpus.Drawing.Text.Label
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

  ) where


import Wumpus.Drawing.Paths

import Wumpus.Basic.Kernel                      -- package: wumpus-basic

import Wumpus.Core                              -- package: wumpus-core





locImageLabel :: Floating u 
         => (a -> Point2 u) -> RectPosition 
         -> PosImage u z -> LocImage u a -> LocImage u a
locImageLabel fn rpos lbl obj = promoteR1 $ \pt -> 
    annotate (obj `at` pt)  (\a -> atStartPos lbl (fn a) rpos)


label_center_of :: (Floating u, CenterAnchor a, u ~ DUnit a) 
                => PosImage u no -> LocImage u a -> LocImage u a
label_center_of = locImageLabel center CENTER


label_left_of :: (Floating u, CardinalAnchor a, u ~ DUnit a) 
              => PosImage u no -> LocImage u a -> LocImage u a
label_left_of = locImageLabel west EE

label_right_of :: (Floating u, CardinalAnchor a, u ~ DUnit a) 
               => PosImage u no -> LocImage u a -> LocImage u a
label_right_of = locImageLabel east WW


label_above :: (Floating u, CardinalAnchor a, u ~ DUnit a) 
              => PosImage u no -> LocImage u a -> LocImage u a
label_above = locImageLabel north SS


label_below :: (Floating u, CardinalAnchor a, u ~ DUnit a) 
              => PosImage u no -> LocImage u a -> LocImage u a
label_below = locImageLabel south NN




connectorPathLabel :: Floating u 
                   => (Path u -> Point2 u) -> RectPosition 
                   -> PosImage u z 
                   -> ConnectorImage u (Path u) -> ConnectorImage u (Path u)
connectorPathLabel fn rpos lbl obj = promoteR2 $ \p0 p1 -> 
    annotate (connect obj p0 p1)  (\a -> atStartPos lbl (fn a) rpos)


label_midway_of :: (Real u, Floating u) 
                => RectPosition -> PosImage u z 
                -> ConnectorImage u (Path u) -> ConnectorImage u (Path u)
label_midway_of = connectorPathLabel midway_


label_atstart_of :: (Real u, Floating u) 
                 => RectPosition -> PosImage u z 
                 -> ConnectorImage u (Path u) -> ConnectorImage u (Path u)
label_atstart_of = connectorPathLabel atstart_


label_atend_of :: (Real u, Floating u) 
                 => RectPosition -> PosImage u z 
                 -> ConnectorImage u (Path u) -> ConnectorImage u (Path u)
label_atend_of = connectorPathLabel atend_



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
-- LocThetaImages and PosImages. Is it acceptable to /saturate/
-- them to LocImages before labelling them?
-- 
-- Connectors support different /anchor-like/ positions so they 
-- will different labelling functions.
-- 
  

