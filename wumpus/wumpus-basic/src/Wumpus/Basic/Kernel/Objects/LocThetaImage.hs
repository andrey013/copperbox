{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Kernel.Objects.LocThetaImage
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  highly unstable
-- Portability :  GHC 
--
-- LocThetaImage and LocThetaGraphic types - these are functional 
-- types from the DrawingContext, start point and angle of 
-- inclination to a graphic /primitive/.
--
--------------------------------------------------------------------------------

module Wumpus.Basic.Kernel.Objects.LocThetaImage
   (
     LocThetaGraphic
   , LocThetaImage

   , runLocThetaImage
   , rawLocThetaImage
   , rot
   , atRot
   , intoLocThetaImage
   , makeLocThetaGraphic
   
   )

   where

import Wumpus.Basic.Kernel.Base.BaseDefs
import Wumpus.Basic.Kernel.Base.DrawingContext
import Wumpus.Basic.Kernel.Base.WrappedPrimitive
import Wumpus.Basic.Kernel.Objects.Basis
import Wumpus.Basic.Kernel.Objects.Image
import Wumpus.Basic.Kernel.Objects.LocImage
import Wumpus.Basic.Kernel.Objects.Query

import Wumpus.Core                              -- package: wumpus-core

import Data.Monoid


-- | Graphic - function from DrawingContext and start point to a 
-- polymorphic /answer/ and a graphic /primitive/.
--
-- The answer is expected to be a Functor.
--
newtype LocThetaImage r u = LocThetaImage { 
    getLocThetaImage :: DrawingContext -> Point2 u -> Radian -> (r u, CatPrim) }

type instance Answer (LocThetaImage r u) = r u

type instance ArgDiff (LocThetaImage r u) (Image r u) = (Point2 u, Radian)


-- | LocThetaGraphic - function from DrawingContext and start point to 
-- a graphic /primitive/.
--
type LocThetaGraphic u = LocThetaImage UNil u



--------------------------------------------------------------------------------



instance PromoteR2 (LocThetaImage r u) (Image r u) where
  promoteR2 = promote_lti2


instance Lift0R2 (LocThetaImage r u) (Image r u) where
  lift0R2 = lift_lti2

instance Lift1R2 (LocThetaImage r u) (LocImage r u) where
  lift1R2 = lift_lti1



instance BindQuery (LocThetaImage r u) where
   (&=>) = bindQuery


instance BindQueryR2 (LocThetaImage r u) (Image r u) where
   (&===>) = bindR2


--------------------------------------------------------------------------------

instance OPlus (r u) => OPlus (LocThetaImage r u) where
  fa `oplus` fb = LocThetaImage $ \ctx pt ang -> 
      getLocThetaImage fa ctx pt ang `oplus` getLocThetaImage fb ctx pt ang

-- LocThetaImage is not a Functor - functor access would need to change 
-- the unit of the start point.

-- bimap has to be unit preserving as unit is a parmater of the 
-- input as well as the output.
--
bimapLocThetaImage :: (r u -> r1 u) -> (Primitive -> Primitive) 
                   -> LocThetaImage r u -> LocThetaImage r1 u
bimapLocThetaImage l r gf = LocThetaImage $ \ctx pt ang -> 
    bimap l (cpmap r) $ getLocThetaImage gf ctx pt ang


-- This needs drawing context so cannot be done with 'bimapLocThetaImage'.
--
instance Functor r => UnitConvert (LocThetaImage r) where
  uconvert gf = LocThetaImage $ \ctx pt ang -> 
      let sz    = dc_font_size ctx
          (a,o) = getLocThetaImage gf ctx (uconvertF sz pt) ang
      in (uconvertF sz a, o)



-- LocTheta objects do not have Affine instances.
--
-- Downcast them to LocImage before transforming...
--





instance MoveStart LocThetaImage where
  moveStart fn gf = LocThetaImage $ \ctx pt ang -> 
                      getLocThetaImage gf ctx (fn pt) ang


instance MoveStartTheta LocThetaImage where
  moveStartTheta fn gf = LocThetaImage $ \ctx pt ang -> 
                           getLocThetaImage gf ctx (fn ang pt) ang

  moveStartThetaAngle fn gf = LocThetaImage $ \ctx pt ang -> 
                                getLocThetaImage gf ctx pt (fn ang)



instance Object LocThetaImage where
  local_ctx     = localLocThetaImg
  ignoreAns     = bimapLocThetaImage (const UNil) id
  replaceAns o  = bimapLocThetaImage (const o) id
  mapAns f      = bimapLocThetaImage f id
  hyperlink hyp = bimapLocThetaImage id (xlinkPrim hyp)
  clipObject pp = bimapLocThetaImage id (clip pp)
  annotate      = annoLocThetaImg
  decorate      = decoLocThetaImg
  bind          = bindLocThetaImg
  unit          = unitLocThetaImg
  
localLocThetaImg :: (DrawingContext -> DrawingContext) 
                 -> LocThetaImage r u 
                 -> LocThetaImage r u
localLocThetaImg upd gf = LocThetaImage $ \ctx pt ang -> 
                          getLocThetaImage gf (upd ctx) pt ang



decoLocThetaImg :: LocThetaImage r u -> LocThetaGraphic u -> LocThetaImage r u
decoLocThetaImg fa fb = LocThetaImage $ \ctx pt ang -> 
    let (a,o1) = getLocThetaImage fa ctx pt ang
        (_,o2) = getLocThetaImage fb ctx pt ang
    in (a, o1 `oplus` o2)
                        
annoLocThetaImg :: LocThetaImage r u -> (r u -> LocThetaGraphic u) 
                -> LocThetaImage r u
annoLocThetaImg fa mf = LocThetaImage $ \ctx pt ang -> 
    let (a,o1) = getLocThetaImage fa ctx pt ang
        (_,o2) = getLocThetaImage (mf a) ctx pt ang
    in (a, o1 `oplus` o2)


bindLocThetaImg :: LocThetaImage r u 
                -> (r u -> LocThetaImage r1 u) 
                -> LocThetaImage r1 u
bindLocThetaImg gf fn = LocThetaImage $ \ctx pt ang -> 
    let (a,o1) = getLocThetaImage gf ctx pt ang
        (b,o2) = getLocThetaImage (fn a) ctx pt ang
    in (b, o1 `oplus` o2)

unitLocThetaImg :: r u -> LocThetaImage r u
unitLocThetaImg a = LocThetaImage $ \_ _ _ -> (a, mempty)



--------------------------------------------------------------------------------
-- builders and destructors

runLocThetaImage :: LocThetaImage r u 
                 -> DrawingContext -> Point2 u -> Radian -> (r u, CatPrim)
runLocThetaImage gf ctx pt ang = getLocThetaImage gf ctx pt ang

-- This seems to be the one for down casting...
-- 
rawLocThetaImage :: (DrawingContext -> Point2 u -> Radian -> (r u, CatPrim)) 
                 -> LocThetaImage r u
rawLocThetaImage fn = LocThetaImage $ \ctx pt ang -> fn ctx pt ang



infixr 1 `rot`

-- | Downcast a 'LocThetaImage' by applying it to the supplied 
-- angle, making a 'LocImage'. 
-- 
rot :: LocThetaImage r u -> Radian -> LocImage r u
rot gf ang = rawLocImage $ \ctx pt -> getLocThetaImage gf ctx pt ang


-- | Downcast a 'LocThetaImage' by applying it to the supplied 
-- point and angle, making an 'Image'. 
--
atRot :: LocThetaImage r u -> Point2 u -> Radian -> Image r u
atRot gf pt ang = rawImage $ \ctx -> getLocThetaImage gf ctx pt ang


-- | 'intoLocThetaImage' : @ loc_theta_query * 
--          loc_theta_graphic -> LocThetaImage @
--
-- /LocTheta/ version of 'intoImage'. 
-- 
-- The 'LocThetaImage' is built as a function from an implicit 
-- start point and angle of inclination to the answer.
--
intoLocThetaImage :: LocThetaQuery u (r u) -> LocThetaGraphic u 
                  -> LocThetaImage r u
intoLocThetaImage fn gf = LocThetaImage $ \ctx pt ang -> 
   let ans   = runQuery fn ctx pt ang
       (_,o) = getLocThetaImage gf ctx pt ang
   in (ans,o)

makeLocThetaGraphic :: InterpretUnit u
                    => Query a 
                    -> (a -> DPoint2 -> Radian -> Primitive) 
                    -> LocThetaGraphic u
makeLocThetaGraphic qry fn = LocThetaImage $ \ctx pt ang -> 
    let ans = runQuery qry ctx 
        sz  = dc_font_size ctx
    in (UNil, prim1 $ fn ans (uconvertF sz pt) ang)




-- name 1 indicates lifting of 1 layer
-- name 2 indicates lifting of 2 layers

-- equivalent to promoteR2

promote_lti2 :: (Point2 u -> Radian -> Image r u) -> LocThetaImage r u
promote_lti2 gf = 
    LocThetaImage $ \ctx pt ang -> runImage (gf pt ang) ctx

-- is ltiPromoteR2 a better name? 

lift_lti1 :: LocImage r u -> LocThetaImage r u
lift_lti1 gf = LocThetaImage $ \ctx pt _ -> runLocImage gf ctx pt

lift_lti2 :: Image r u -> LocThetaImage r u
lift_lti2 gf = LocThetaImage $ \ctx _ _ -> runImage gf ctx




bindQuery :: Query ans -> (ans -> LocThetaImage r u) -> LocThetaImage r u
bindQuery qy fn = LocThetaImage $ \ctx pt ang -> 
    let a = runQuery qy ctx in runLocThetaImage (fn a) ctx pt ang


-- | Use a Loc query to generate ans @ans@ turn the @ans@ into an
-- @Image@ projecting up to a @LocImage@.
--
bindR2 :: LocThetaQuery u ans -> (ans -> Image r u) -> LocThetaImage r u
bindR2 qy fn = LocThetaImage $ \ctx pt ang -> 
    let f1 = runQuery qy ctx in runImage (fn $ f1 pt ang) ctx

