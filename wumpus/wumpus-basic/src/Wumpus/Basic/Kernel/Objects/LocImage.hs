{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Kernel.Objects.LocImage
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  highly unstable
-- Portability :  GHC 
--
-- Extended Image object - the image has an implicitly supplied
-- origin (start point).
--
--------------------------------------------------------------------------------

module Wumpus.Basic.Kernel.Objects.LocImage
  (

  -- * LocImage
    LocImage
  , DLocImage

  , runLocImage
  , extrLocGraphic
  , intoLocImage

  , textlineMulti

  ) where

import Wumpus.Basic.Kernel.Base.BaseDefs
import Wumpus.Basic.Kernel.Base.ContextFun
import Wumpus.Basic.Kernel.Base.DrawingContext
import Wumpus.Basic.Kernel.Base.QueryDC
import Wumpus.Basic.Kernel.Base.WrappedPrimitive
import Wumpus.Basic.Kernel.Objects.LocGraphic

import Wumpus.Core                              -- package: wumpus-core

import Data.AffineSpace                         -- package: vector-space

import Control.Applicative




--------------------------------------------------------------------------------
-- Image

-- | Images return a value as well as drawing. A /node/ is a 
-- typical example - nodes are drawn but the also support 
-- taking anchor points.
--

type LocImage u a       = LocCF u (a,PrimGraphic u)

type DLocImage a        = LocImage Double a

-- type instance DUnit (Image u a) = u

--------------------------------------------------------------------------------
-- Run functions

runLocImage :: DrawingContext -> Point2 u -> LocImage u a -> (a, PrimGraphic u)
runLocImage ctx pt img = runCF ctx (unCF1 pt img)





-------------------------------------------------------------------------------
-- Dropping /answers/


extrLocGraphic :: LocImage u a -> LocGraphic u
extrLocGraphic = postpro1 snd 





intoLocImage :: LocCF u a -> LocGraphic u -> LocImage u a
intoLocImage = postcomb1 (,)



--------------------------------------------------------------------------------

-- | Point is the baseline left of the bottom line, text is 
-- left-aligned.
--
-- Internally this uses LocImage.
-- 
textlineMulti :: Fractional u => [String] -> LocGraphic u
textlineMulti xs = baselineSpacing >>= \dy -> 
    extrLocGraphic $ go (tmStep dy) xs
  where
    -- go /starts/ at the end of the list and works back.
    go fn []      = fn ""       -- not ideal, better than error
    go fn [s]     = fn s
    go fn (s:ss)  = let ans = go fn ss in ans `feedPt` fn s

-- LocImage u (Point2 u) deserved to be a new type synonym
-- as it models PostScript\'s @show@ 


tmStep :: Num u => u -> String -> LocImage u (Point2 u) 
tmStep dy str = intoLocImage (pure $ \pt -> pt .+^ vvec dy) (textline str)

feedPt :: LocImage u (Point2 u) -> LocImage u (Point2 u) -> LocImage u (Point2 u) 
feedPt = accumulate1 oplus
