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


converti :: (t u1 -> t u) -> Image t u1 -> Image t u
converti fa img = 
    img >>= \a -> return $ imageAns (fa $ answer a) (imageOutput a)

convertli :: (PtSize u0, PtSize u) 
          => (t u0 -> t u) -> LocImage t u0 -> LocImage t u
convertli fa img = 
    promoteR1 $ \pt -> converti fa (img `at` ptCnv pt)


convertlti :: (PtSize u0, PtSize u) 
           => (t u0 -> t u) -> LocThetaImage t u0 -> LocThetaImage t u
convertlti fa img = 
    promoteR2 $ \pt ang -> converti fa $ atRot img (ptCnv pt) ang


convertconn :: (PtSize u0, PtSize u) 
            => (t u0 -> t u) -> ConnectorImage t u0 -> ConnectorImage t u
convertconn fa img = 
    promoteR2 $ \p1 p2 -> converti fa $ connect img (ptCnv p1) (ptCnv p2)


convertpti :: (PtSize u0, PtSize u) 
           => (t u0 -> t u) -> PosThetaImage t u0 -> PosThetaImage t u
convertpti fa img = promoteR3 $ \pt rpos ang -> 
                    converti fa $ apply3R3 img (ptCnv pt) rpos ang

