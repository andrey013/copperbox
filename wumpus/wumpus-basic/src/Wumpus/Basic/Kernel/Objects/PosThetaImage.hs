{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Kernel.Objects.PosThetaImage
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  highly unstable
-- Portability :  GHC 
--
-- PosImage object extended with angle of inclination.
-- 
--------------------------------------------------------------------------------

module Wumpus.Basic.Kernel.Objects.PosThetaImage
  (

  -- * Positionable image


    PosThetaImage
  , PosThetaGraphic

  , DPosThetaGraphic
  , DPosThetaImage

  , PosThetaQuery

  , atStartPosRot
  , startPosRot
  , ptRot

  , uconvertPosThetaImg 

  ) where


import Wumpus.Basic.Kernel.Base.BaseDefs
import Wumpus.Basic.Kernel.Base.ContextFun
import Wumpus.Basic.Kernel.Objects.Basis
import Wumpus.Basic.Kernel.Objects.Image
import Wumpus.Basic.Kernel.Objects.LocImage
import Wumpus.Basic.Kernel.Objects.PosImage

import Wumpus.Core                              -- package: wumpus-core






-- | A positionable Image that supports drawing at some angle of 
-- inclination.
-- 
-- Note - the rectangle frame is expected to represent an 
-- orthogonal frame bounding the maximum hull of the Image, the 
-- frame is not intended to be inclined itself.
--
type PosThetaImage t u = CF3 (Point2 u) RectPosition Radian (ImageAns t u)

    
-- | Version of PosThetaImage specialized to Double for the unit type.
--
type DPosThetaImage r = PosThetaImage r Double



-- | A positionable Graphic that supports drawing at some angle of
-- inclination.
--
-- Note - the rectangle frame is expected to represent an 
-- orthogonal frame bounding the maximum hull of the Image, the 
-- frame is not intended to be inclined itself.
--
type PosThetaGraphic u = PosThetaImage UNil u
    
-- | Version of PosThetaGraphic specialized to Double for the unit type.
--
type DPosThetaGraphic = PosThetaGraphic Double


type PosThetaQuery u ans = Query (Point2 u -> RectPosition -> Radian -> ans)






-- should @rot@ be a overloaded?


-- | 'atStartPos' : @ pos_image * start_point * rect_pos -> LocImage @
--
-- /Downcast/ a 'PosGraphic' to an 'Image' by supplying it 
-- with an initial point and a 'RectPosition' (start position).
--  
atStartPosRot :: PosThetaImage r u -> Point2 u -> RectPosition -> Radian 
              -> Image r u
atStartPosRot = apply3R3


startPosRot :: PosThetaImage r u -> RectPosition -> Radian -> LocImage r u
startPosRot = apply2R3


infixr 1 `ptRot`

ptRot :: PosThetaImage r u -> Radian -> PosImage r u
ptRot = apply1R3



-- | Use this to convert both 'PosThetaImage' and 
-- 'PosThetaGraphic'.
--
uconvertPosThetaImg :: (InterpretUnit u, InterpretUnit u1, Functor t) 
                    => PosThetaImage t u -> PosThetaImage t u1
uconvertPosThetaImg = uconvertR3a

--------------------------------------------------------------------------------
