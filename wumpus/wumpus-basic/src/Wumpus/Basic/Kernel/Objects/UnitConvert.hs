{-# LANGUAGE TypeSynonymInstances       #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Kernel.Objects.UnitConvert
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  highly unstable
-- Portability :  GHC 
--
-- Conversion between different unit types.
--
-- Unfortunately this is rather clunky as the primary objects in 
-- Wumpus-Basic are not usefully functors.
-- 
--------------------------------------------------------------------------------

module Wumpus.Basic.Kernel.Objects.UnitConvert
  (

    converti
  , convertli
  , convertlti
  , convertconn
  , convertpti


  ) where


import Wumpus.Basic.Kernel.Base.BaseDefs
import Wumpus.Basic.Kernel.Base.ContextFun
import Wumpus.Basic.Kernel.Base.QueryDC
import Wumpus.Basic.Kernel.Objects.BaseObjects
import Wumpus.Basic.Kernel.Objects.Connector
import Wumpus.Basic.Kernel.Objects.PosImage

import Wumpus.Core                              -- package: wumpus-core


-- | This translates from a non-contextual size (e.g Double) to a 
-- possibly contextual one.
--
-- Its use case is converting a start point for LocGraphics.
--
ptCnv :: (CxSize cxu, PsDouble u) => FontSize -> Point2 u -> Point2 cxu
ptCnv sz = fmap (csSize sz . toPsDouble)


converti :: (Functor t, CxSize u, PsDouble u1) 
         => Image t u -> Image t u1
converti img = getFontSize >>= \sz -> cxConverti sz img




convertli :: (Functor t, CxSize u, PsDouble u1) 
          => LocImage t u -> LocImage t u1
convertli img = promoteR1 $ \pt -> 
    getFontSize >>= \sz -> cxConverti sz (img `at` ptCnv sz pt)


convertlti :: (Functor t, CxSize u, PsDouble u1) 
           => LocThetaImage t u -> LocThetaImage t u1
convertlti img = promoteR2 $ \pt ang -> 
    getFontSize >>= \sz -> cxConverti sz $ atRot img (ptCnv sz pt) ang


convertconn :: (Functor t, CxSize u, PsDouble u1) 
            => ConnectorImage t u -> ConnectorImage t u1
convertconn img = promoteR2 $ \p1 p2 -> 
    getFontSize >>= \sz -> cxConverti sz $ connect img (ptCnv sz p1) (ptCnv sz p2)


convertpti :: (Functor t, CxSize u, PsDouble u1) 
           => PosThetaImage t u -> PosThetaImage t u1
convertpti img = promoteR3 $ \pt rpos ang -> 
    getFontSize >>= \sz -> cxConverti sz $ apply3R3 img (ptCnv sz pt) rpos ang


