{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleInstances          #-}
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
    
    Label(..)
  , labelAbove
  , labelBelow
  , labelLeft
  , labelRight

  ) where

import Wumpus.Basic.Kernel                      -- package: wumpus-basic

import Wumpus.Core                              -- package: wumpus-core






-- TikZ has label=below etc.
-- 
-- This would probably translate to a functions:
-- @labelBelow@
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



-- | This is the instance for LocThetaCF.
--
instance Label (CF2 (Point2 u) Radian) where
  label fn rpos lbl obj = promoteR2 $ \pt theta -> 
      sdecorate (atRot obj pt theta)  (\a -> atStartPos lbl (fn a) rpos)

{-

-- NOTE - the isnatnce for PosImage does not work, so the Label
-- class obviously isn\'t totally correct...

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

