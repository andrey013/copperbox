{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Objects.LocTheta
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  highly unstable
-- Portability :  GHC 
--
-- Graphic and Image types taking an implicit origin /and/ an
-- implicit direction.
--
--------------------------------------------------------------------------------

module Wumpus.Basic.Kernel.Objects.LocTheta
  (

  -- * LocThetaGraphic  
    LocThetaGraphic
  , DLocThetaGraphic


  -- * LocThetaImage
  , LocThetaImage
  , DLocThetaImage

  , intoLocThetaImage

  , rtextline

  ) where

import Wumpus.Basic.Kernel.Base.ContextFun
import Wumpus.Basic.Kernel.Base.QueryDC
import Wumpus.Basic.Kernel.Base.WrappedPrimitive

import Wumpus.Core                              -- package: wumpus-core

--------------------------------------------------------------------------------
-- LocThetaGraphic



-- | A function from @point * angle -> graphic@
--
type LocThetaGraphic u          = LocThetaCF u (PrimGraphic u)


type DLocThetaGraphic   = LocThetaGraphic Double




--------------------------------------------------------------------------------
-- LocThetaImage


type LocThetaImage u a  = LocThetaCF u (a,PrimGraphic u)


type DLocThetaImage a   = LocThetaImage Double a 




intoLocThetaImage :: LocThetaCF u a 
                  -> LocThetaGraphic u 
                  -> LocThetaImage u a
intoLocThetaImage = postcomb2 (,)


thetaLocPrimGraphic :: (Point2 u -> Radian -> Primitive u) 
                    -> (Point2 u -> Radian -> PrimGraphic u) 
thetaLocPrimGraphic fn = \pt theta -> primGraphic (fn pt theta)


rtextline :: Num u => String -> LocThetaGraphic u
rtextline ss = 
    withTextAttr $ \rgb attr -> thetaLocPrimGraphic 
                                  (\pt ang -> rtextlabel rgb attr ss pt ang)




