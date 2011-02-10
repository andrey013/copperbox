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

  , connectorPathLabel
  , label_midway_of
  , label_atstart_of
  , label_atend_of
    
  -- * Probably obsolete...
  , Label(..)
  , labelAbove
  , labelBelow
  , labelLeft
  , labelRight

  ) where


import Wumpus.Drawing.Paths

import Wumpus.Basic.Kernel                      -- package: wumpus-basic

import Wumpus.Core                              -- package: wumpus-core





locImageLabel :: Floating u 
         => (a -> Point2 u) -> RectPosition 
         -> PosImage u z -> LocImage u a -> LocImage u a
locImageLabel fn rpos lbl obj = promoteR1 $ \pt -> 
    sdecorate (obj `at` pt)  (\a -> atStartPos lbl (fn a) rpos)


label_center_of :: (Floating u, CenterAnchor a, u ~ DUnit a) 
                => PosImage u no -> LocImage u a -> LocImage u a
label_center_of = locImageLabel center CENTER


label_left_of :: (Floating u, CardinalAnchor a, u ~ DUnit a) 
              => PosImage u no -> LocImage u a -> LocImage u a
label_left_of = locImageLabel west WW



connectorPathLabel :: Floating u 
                   => (Path u -> Point2 u) -> RectPosition 
                   -> PosImage u z 
                   -> ConnectorImage u (Path u) -> ConnectorImage u (Path u)
connectorPathLabel fn rpos lbl obj = promoteR2 $ \p0 p1 -> 
    sdecorate (connect obj p0 p1)  (\a -> atStartPos lbl (fn a) rpos)


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
  

class Label m where
  label :: (Floating u, u ~ DUnit a)
        => (a -> Point2 u) -> RectPosition 
        -> PosImage u z -> m (ImageAns u a) -> m (ImageAns u a)

-- | This is the instance for CF.
-- (Graphic, Image)
--
instance Label CF where
  label fn rpos lbl obj = sdecorate obj (\a -> atStartPos lbl (fn a) rpos)



-- | This is the instance for LocCF.
-- (LocGraphic, LocImage)
--
instance Label (CF1 (Point2 u)) where
  label fn rpos lbl obj = promoteR1 $ \pt -> 
      sdecorate (obj `at` pt)  (\a -> atStartPos lbl (fn a) rpos)

{-
-- Note - this is just an experiment...
instance Label (LocCF u) where
  label fn rpos lbl obj = promoteR1 $ \pt -> 
      sdecorate (obj `at` pt)  (\a -> atStartPos lbl (fn a) rpos)
-}

-- | This is the instance for LocThetaCF.
--
instance Label (CF2 (Point2 u) Radian) where
  label fn rpos lbl obj = promoteR2 $ \pt theta -> 
      sdecorate (atRot obj pt theta)  (\a -> atStartPos lbl (fn a) rpos)



-- NOTE - the instance for PosImage does not work, so the Label
-- class obviously isn\'t totally correct...
{-
instance Label (CF2 (Point2 u) RectPosition) where
  label fn rpos lbl obj = promoteR2 $ \pt dpos -> 
      sdecorate (atStartPos obj pt dpos)  (\a -> atStartPos lbl (fn a) rpos)
-}

labelAbove :: (CardinalAnchor a, Floating u, Label m, u ~ DUnit a)
           => PosImage u z -> m (ImageAns u a) -> m (ImageAns u a)
labelAbove = label north SS


labelBelow :: (CardinalAnchor a, Floating u, Label m, u ~ DUnit a)
           => PosImage u z -> m (ImageAns u a) -> m (ImageAns u a)
labelBelow = label south NN


labelLeft  :: (CardinalAnchor a, Floating u, Label m, u ~ DUnit a)
           => PosImage u z -> m (ImageAns u a) -> m (ImageAns u a)
labelLeft  = label west EE


labelRight :: (CardinalAnchor a, Floating u, Label m, u ~ DUnit a)
           => PosImage u z -> m (ImageAns u a) -> m (ImageAns u a)
labelRight = label east WW

