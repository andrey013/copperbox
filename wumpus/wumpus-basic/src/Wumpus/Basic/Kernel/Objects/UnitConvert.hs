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
import Wumpus.Basic.Kernel.Objects.BaseObjects
import Wumpus.Basic.Kernel.Objects.Connector
import Wumpus.Basic.Kernel.Objects.PosImage

import Wumpus.Core                              -- package: wumpus-core


-- Design note - it looks like /contextual/ units (em, en, ...) 
-- will need to clone the Displacement and DrawingPrimtives 
-- modules and potentially all drawing objects...
--
-- A practical alternative might be to make all units contextual
-- but Centimeter, PostScript Point etc. ignore the the current
-- font size.
--


ptCnv :: (PtSize u0, PtSize u) => Point2 u0 -> Point2 u    
ptCnv = fmap (fromPsPoint . toPsPoint)


converti :: (a -> b) -> Image u1 a -> Image u b
converti fa img = 
    img >>= \a -> return $ imageAns (fa $ answer a) (imageOutput a)

convertli :: (PtSize u0, PtSize u) => (a -> b) -> LocImage u0 a -> LocImage u b
convertli fa img = 
    promoteR1 $ \pt -> converti fa (img `at` ptCnv pt)


convertlti :: (PtSize u0, PtSize u) 
           => (a -> b) -> LocThetaImage u0 a -> LocThetaImage u b
convertlti fa img = 
    promoteR2 $ \pt ang -> converti fa $ atRot img (ptCnv pt) ang


convertconn :: (PtSize u0, PtSize u) 
            => (a -> b) -> ConnectorImage u0 a -> ConnectorImage u b
convertconn fa img = 
    promoteR2 $ \p1 p2 -> converti fa $ connect img (ptCnv p1) (ptCnv p2)


convertpti :: (PtSize u0, PtSize u) 
           => (a -> b) -> PosThetaImage u0 a -> PosThetaImage u b
convertpti fa img = promoteR3 $ \pt rpos ang -> 
                    converti fa $ apply3R3 img (ptCnv pt) rpos ang

