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
   
   , promote_lti1
   , promote_lti2
   , lift_lti1
   , lift_lti2

   , bindQuery_lti
   , bindLocQuery_lti
   , bindLocThetaQuery_lti

   )

   where

import Wumpus.Basic.Kernel.Base.BaseDefs
import Wumpus.Basic.Kernel.Base.DrawingContext
import Wumpus.Basic.Kernel.Base.WrappedPrimitive
import Wumpus.Basic.Kernel.Objects.Image
import Wumpus.Basic.Kernel.Objects.ImageBasis
import Wumpus.Basic.Kernel.Objects.LocImage
import Wumpus.Basic.Kernel.Objects.Query

import Wumpus.Core                              -- package: wumpus-core

import Data.Monoid


-- | Graphic - function from DrawingContext and start point to a 
-- polymorphic /answer/ and a graphic /primitive/.
--
-- The answer is expected to be a Functor.
--
newtype LocThetaImage t u = LocThetaImage { 
    getLocThetaImage :: DrawingContext -> Point2 u -> Radian -> (t u, CatPrim) }


-- | LocThetaGraphic - function from DrawingContext and start point to 
-- a graphic /primitive/.
--
type LocThetaGraphic u = LocThetaImage UNil u


instance OPlus (t u) => OPlus (LocThetaImage t u) where
  fa `oplus` fb = LocThetaImage $ \ctx pt ang -> 
      getLocThetaImage fa ctx pt ang `oplus` getLocThetaImage fb ctx pt ang

-- LocThetaImage is not a Functor - functor access would need to change 
-- the unit of the start point.

-- bimap has to be unit preserving as unit is a parmater of the 
-- input as well as the output.
--
bimapLocThetaImage :: (t u -> t' u) -> (Primitive -> Primitive) 
           -> LocThetaImage t u -> LocThetaImage t' u
bimapLocThetaImage l r gf = LocThetaImage $ \ctx pt ang -> 
    bimap l (cpmap r) $ getLocThetaImage gf ctx pt ang


-- This needs drawing context so cannot be done with 'bimapLocThetaImage'.
--
instance UnitConvertExt t => UnitConvert (LocThetaImage t) where
  uconvert gf = LocThetaImage $ \ctx pt ang -> 
      let sz    = dc_font_size ctx
          (a,o) = getLocThetaImage gf ctx (uconvertExt sz pt) ang
      in (uconvertExt sz a, o)



-- LocTheta objects do not have Affine instances.
--
-- Downcast them to LocImage before transforming...
--


instance Localize LocThetaImage where
   localize upd gf = LocThetaImage $ \ctx pt ang -> 
                       getLocThetaImage gf (upd ctx) pt ang


instance Hyperlink (LocThetaImage t u) where
  hyperlink hyp = bimapLocThetaImage id (xlinkPrim hyp)


instance UMonad LocThetaImage where
  bind = bindLocThetaImg
  unit = unitLocThetaImg

bindLocThetaImg :: LocThetaImage t u 
                -> (t u -> LocThetaImage t1 u) 
                -> LocThetaImage t1 u
bindLocThetaImg gf fn = LocThetaImage $ \ctx pt ang -> 
    let (a,o1) = getLocThetaImage gf ctx pt ang
        (b,o2) = getLocThetaImage (fn a) ctx pt ang
    in (b, o1 `oplus` o2)

unitLocThetaImg :: t u -> LocThetaImage t u
unitLocThetaImg a = LocThetaImage $ \_ _ _ -> (a, mempty)

instance MoveStart LocThetaImage where
  moveStart fn gf = LocThetaImage $ \ctx pt ang -> 
                      getLocThetaImage gf ctx (fn pt) ang


instance MoveStartTheta LocThetaImage where
  moveStartTheta fn gf = LocThetaImage $ \ctx pt ang -> 
                           getLocThetaImage gf ctx (fn ang pt) ang

  moveStartThetaAngle fn gf = LocThetaImage $ \ctx pt ang -> 
                                getLocThetaImage gf ctx pt (fn ang)



instance IgnoreAns LocThetaImage where
  ignoreAns    = bimapLocThetaImage (const UNil) id
  replaceAns o = bimapLocThetaImage (const o) id
  mapAns f     = bimapLocThetaImage f id


instance Annotate LocThetaImage where
  annotate = annoLocThetaImg
  decorate = decoLocThetaImg



decoLocThetaImg :: LocThetaImage t u -> LocThetaGraphic u -> LocThetaImage t u
decoLocThetaImg fa fb = LocThetaImage $ \ctx pt ang -> 
    let (a,o1) = getLocThetaImage fa ctx pt ang
        (_,o2) = getLocThetaImage fb ctx pt ang
    in (a, o1 `oplus` o2)
                        
annoLocThetaImg :: LocThetaImage t u -> (t u -> LocThetaGraphic u) 
                -> LocThetaImage t u
annoLocThetaImg fa mf = LocThetaImage $ \ctx pt ang -> 
    let (a,o1) = getLocThetaImage fa ctx pt ang
        (_,o2) = getLocThetaImage (mf a) ctx pt ang
    in (a, o1 `oplus` o2)




--------------------------------------------------------------------------------
-- builders and destructors

runLocThetaImage :: LocThetaImage t u 
                 -> DrawingContext -> Point2 u -> Radian -> (t u, CatPrim)
runLocThetaImage gf ctx pt ang = getLocThetaImage gf ctx pt ang

-- This seems to be the one for down casting...
-- 
rawLocThetaImage :: (DrawingContext -> Point2 u -> Radian -> (t u, CatPrim)) 
                 -> LocThetaImage t u
rawLocThetaImage fn = LocThetaImage $ \ctx pt ang -> fn ctx pt ang



infixr 1 `rot`

-- | Downcast a 'LocThetaImage' by applying it to the supplied 
-- angle, making a 'LocImage'. 
-- 
rot :: LocThetaImage t u -> Radian -> LocImage t u
rot gf ang = rawLocImage $ \ctx pt -> getLocThetaImage gf ctx pt ang


-- | Downcast a 'LocThetaImage' by applying it to the supplied 
-- point and angle, making an 'Image'. 
--
atRot :: LocThetaImage t u -> Point2 u -> Radian -> Image t u
atRot gf pt ang = rawImage $ \ctx -> getLocThetaImage gf ctx pt ang


-- | 'intoLocThetaImage' : @ loc_theta_query * 
--          loc_theta_graphic -> LocThetaImage @
--
-- /LocTheta/ version of 'intoImage'. 
-- 
-- The 'LocThetaImage' is built as a function from an implicit 
-- start point and angle of inclination to the answer.
--
intoLocThetaImage :: LocThetaQuery u (t u) -> LocThetaGraphic u 
                  -> LocThetaImage t u
intoLocThetaImage fn gf = LocThetaImage $ \ctx pt ang -> 
   let ans   = runQuery fn ctx pt ang
       (_,o) = getLocThetaImage gf ctx pt ang
   in (ans,o)

makeLocThetaGraphic :: InterpretUnit u
                    => (DrawingContext -> a) 
                    -> (a -> DPoint2 -> Radian -> Primitive) 
                    -> LocThetaGraphic u
makeLocThetaGraphic qry fn = LocThetaImage $ \ctx pt ang -> 
    let ans = qry ctx 
        sz  = dc_font_size ctx
    in (UNil, prim1 $ fn ans (uconvertExt sz pt) ang)




-- name 1 indicates lifting of 1 layer
-- name 2 indicates lifting of 2 layers

promote_lti1 :: (Radian -> LocImage t u) -> LocThetaImage t u
promote_lti1 gf = 
    LocThetaImage $ \ctx pt ang -> runLocImage (gf ang) ctx pt

-- equivalent to promoteR2

promote_lti2 :: (Point2 u -> Radian -> Image t u) -> LocThetaImage t u
promote_lti2 gf = 
    LocThetaImage $ \ctx pt ang -> runImage (gf pt ang) ctx


lift_lti1 :: LocImage t u -> LocThetaImage t u
lift_lti1 gf = LocThetaImage $ \ctx pt _ -> runLocImage gf ctx pt

lift_lti2 :: Image t u -> LocThetaImage t u
lift_lti2 gf = LocThetaImage $ \ctx _ _ -> runImage gf ctx



bindQuery_lti :: InterpretUnit u 
              => Query ans -> (ans -> DPoint2 -> Radian -> Image t u) 
              -> LocThetaImage t u
bindQuery_lti qy fn = LocThetaImage $ \ctx pt ang -> 
    let ans = runQuery qy ctx 
        sz  = dc_font_size ctx        
    in runImage (fn ans (uconvertExt sz pt) ang) ctx


-- | Use a Loc query to generate ans @ans@ turn the @ans@ into an
-- @Image@ projecting up to a @LocImage@.
--
bindLocQuery_lti :: LocQuery u ans 
                 -> (ans -> Radian -> Image t u) 
                 -> LocThetaImage t u
bindLocQuery_lti qry fn = LocThetaImage $ \ctx pt ang -> 
    let f1 = runQuery qry ctx in runImage (fn (f1 pt) ang) ctx


bindLocThetaQuery_lti :: LocThetaQuery u ans 
                      -> (ans -> Image t u) 
                      -> LocThetaImage t u
bindLocThetaQuery_lti qry fn = LocThetaImage $ \ctx pt ang -> 
    let f1 = runQuery qry ctx in runImage (fn $ f1 pt ang) ctx



