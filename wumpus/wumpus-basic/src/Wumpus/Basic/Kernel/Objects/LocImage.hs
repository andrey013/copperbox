{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Kernel.Objects.LocImage
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  highly unstable
-- Portability :  GHC 
--
-- LocImage and LocGraphic types - these are functional types from the 
-- DrawingContext and start point to a graphic /primitive/.
--
--------------------------------------------------------------------------------

module Wumpus.Basic.Kernel.Objects.LocImage
   (
     LocGraphic
   , LocImage

   , runLocImage
   , rawLocImage
   , at
   , intoLocImage
   , makeLocGraphic

   , promote_li1
   , lift_li1

   , bindQuery_li
   , bindLocQuery_li

   )

   where

import Wumpus.Basic.Kernel.Base.BaseDefs
import Wumpus.Basic.Kernel.Base.DrawingContext
import Wumpus.Basic.Kernel.Base.WrappedPrimitive
import Wumpus.Basic.Kernel.Objects.Basis
import Wumpus.Basic.Kernel.Objects.Image
import Wumpus.Basic.Kernel.Objects.Query

import Wumpus.Core                              -- package: wumpus-core

import Data.Monoid


-- | Graphic - function from DrawingContext and start point to a 
-- polymorphic /answer/ and a graphic /primitive/.
--
-- The answer is expected to be a Functor.
--
newtype LocImage r u = LocImage { 
          getLocImage :: DrawingContext -> Point2 u -> (r u, CatPrim) }

type instance Answer (LocImage r u)  = r u

-- | LocGraphic - function from DrawingContext and start point to 
-- a graphic /primitive/.
--
type LocGraphic u = LocImage UNil u




--------------------------------------------------------------------------------



type instance Arg1 (LocImage r u) (Image r u) = Point2 u

instance PromoteR1 (LocImage r u) (Image r u) where
  promoteR1 = promote_li1


instance Lift0R1 (LocImage r u) (Image r u) where
  lift0R1 = lift_li1


--------------------------------------------------------------------------------

instance OPlus (r u) => OPlus (LocImage r u) where
  fa `oplus` fb = LocImage $ \ctx pt -> 
                    getLocImage fa ctx pt `oplus` getLocImage fb ctx pt

--
-- DESIGN NOTE:
--
-- LocImage is not a Functor - we can change the current type 
-- slightly and have a Functor instance @a@ for answer rather 
-- than @t u@, but then we can\'t have a UnitConvert instance.
--
-- To unit transform a LocImage we need to transform the input and
-- the output.
-- 
-- In short - LocImage simply cannot be a Functor.
--

-- unitmap :: Functor r => (u -> u1) -> (u1 -> u) -> LocImage r u -> LocImage r u1
-- unitmap f g mf = LocImage $ \ctx pt -> post $ getLocImage mf ctx (fmap g pt) 
--    where
--      post (ans,prim) = (fmap f ans,prim)

-- unitmap does not seem to be able to do anything more than
-- UnitConvert.
--

-- bimap has to be unit preserving as unit is a parmater of the 
-- input as well as the output.
--
bimapLocImage :: (r u -> r1 u) -> (Primitive -> Primitive) 
           -> LocImage r u -> LocImage r1 u
bimapLocImage l r gf = LocImage $ \ctx pt -> 
    bimap l (cpmap r) $ getLocImage gf ctx pt




-- This needs drawing context so cannot be done with 'bimapLocImage'.
--
instance Functor r => UnitConvert (LocImage r) where
  uconvert gf = LocImage $ \ctx pt -> 
      let sz    = dc_font_size ctx
          (a,o) = getLocImage gf ctx (uconvertF sz pt)
      in (uconvertF sz a, o)

-- movestartLocImage :: (Point2 u -> Point2 u) -> LocImage r u -> LocImage r u
-- movestartLocImage fn gf = LocImage $ \ctx pt -> getLocImage gf ctx (fn pt) 


-- Affine instances transfom the start point as well as the 
-- answer.

instance (InterpretUnit u, Rotate (r u)) => Rotate (LocImage r u) where
  rotate ang gf = LocImage $ \ctx pt -> 
      let trafo = intraMapPoint (dc_font_size ctx) (rotate ang)
          (a,o) = getLocImage gf ctx (trafo pt)
      in (rotate ang a, rotate ang o)


instance (InterpretUnit u, RotateAbout (r u)) => 
    RotateAbout (LocImage r u) where
  rotateAbout ang p0 gf = LocImage $ \ctx pt -> 
      let dP0   = uconvertF (dc_font_size ctx) p0
          trafo = intraMapPoint (dc_font_size ctx) (rotateAbout ang dP0)
          (a,o) = getLocImage gf ctx (trafo pt)
      in (rotateAbout ang dP0 a, rotateAbout ang dP0 o)


instance (InterpretUnit u, Scale (r u)) => Scale (LocImage r u) where
  scale sx sy gf = LocImage $ \ctx pt -> 
      let trafo = intraMapPoint (dc_font_size ctx) (scale sx sy)
          (a,o) = getLocImage gf ctx (trafo pt)
      in (scale sx sy a, scale sx sy o)



instance (InterpretUnit u, Translate (r u)) => Translate (LocImage r u) where
  translate dx dy gf = LocImage $ \ctx pt -> 
      let trafo = intraMapPoint (dc_font_size ctx) (translate dx dy)
          (a,o) = getLocImage gf ctx (trafo pt)
      in (translate dx dy a, translate dx dy o)


instance MoveStart LocImage where
  moveStart fn gf = LocImage $ \ctx pt -> getLocImage gf ctx (fn pt)


instance Object LocImage where
  local_ctx upd gf = LocImage $ \ctx pt -> getLocImage gf (upd ctx) pt
  ignoreAns        = bimapLocImage (const UNil) id
  replaceAns o     = bimapLocImage (const o) id
  mapAns f         = bimapLocImage f id
  hyperlink hyp    = bimapLocImage id (xlinkPrim hyp)
  annotate         = annoLocImg
  decorate         = decoLocImg
  bind             = bindLocImg
  unit             = unitLocImg



decoLocImg :: LocImage r u -> LocGraphic u -> LocImage r u
decoLocImg fa fb = LocImage $ \ctx pt -> 
    let (a,o1) = getLocImage fa ctx pt
        (_,o2) = getLocImage fb ctx pt
    in (a, o1 `oplus` o2)
                        
annoLocImg :: LocImage r u -> (r u -> LocGraphic u) -> LocImage r u
annoLocImg fa mf = LocImage $ \ctx pt -> 
    let (a,o1) = getLocImage fa ctx pt
        (_,o2) = getLocImage (mf a) ctx pt
    in (a, o1 `oplus` o2)



bindLocImg :: LocImage r u -> (r u -> LocImage t1 u) -> LocImage t1 u
bindLocImg gf fn = LocImage $ \ctx pt -> 
    let (a,o1) = getLocImage gf ctx pt
        (b,o2) = getLocImage (fn a) ctx pt
    in (b, o1 `oplus` o2)

unitLocImg :: r u -> LocImage r u
unitLocImg a = LocImage $ \_ _ -> (a,mempty)



--------------------------------------------------------------------------------
-- builders and destructors


runLocImage :: LocImage r u -> DrawingContext -> Point2 u -> (r u, CatPrim)
runLocImage gf ctx pt = getLocImage gf ctx pt

-- This seems to be the one for down casting...
-- 
rawLocImage :: (DrawingContext -> Point2 u -> (r u, CatPrim)) -> LocImage r u
rawLocImage fn = LocImage $ \ctx pt -> fn ctx pt

infixr 1 `at`

at :: LocImage r u -> Point2 u -> Image r u
gf `at` pt = rawImage $ \ctx ->  getLocImage gf ctx pt


-- | 'intoLocImage' : @ loc_query * loc_graphic -> LocImage @
--
-- /Loc/ version of 'intoImage'. 
-- 
-- The 'LocImage' is built as a function from an implicit start 
-- point to the answer.
--
intoLocImage :: LocQuery u (r u) -> LocGraphic u -> LocImage r u
intoLocImage fn gf = LocImage $ \ctx pt -> 
   let ans   = runQuery fn ctx pt
       (_,o) = getLocImage gf ctx pt
   in (ans,o)

makeLocGraphic :: InterpretUnit u
               => Query a
               -> (a -> DPoint2 -> Primitive) 
               -> LocGraphic u
makeLocGraphic qry fn = LocImage $ \ctx pt -> 
    let ans = runQuery qry ctx 
        sz  = dc_font_size ctx
    in (UNil, prim1 $ fn ans (uconvertF sz pt))



promote_li1 :: (Point2 u -> Image r u) -> LocImage r u
promote_li1 gf = 
    LocImage $ \ctx pt -> runImage (gf pt) ctx


lift_li1 :: Image r u -> LocImage r u
lift_li1 gf = LocImage $ \ctx _ -> runImage gf ctx 


-- WARNING - naming scheme for bindQuery needs attention 

-- | Use a Query to generate ans @ans@ turn the @ans@ with the
-- builder.
--
bindQuery_li ::  Query ans -> (ans -> LocImage r u) -> LocImage r u
bindQuery_li qy fn = LocImage $ \ctx pt -> 
    let ans = runQuery qy ctx 
    in runLocImage (fn ans) ctx pt



{-
-- OUT OF DATE....

-- | Use a Query to generate ans @ans@ turn the @ans@ with the
-- builder.
--
bindQuery_li1 :: InterpretUnit u 
              => Query ans -> (ans -> DPoint2 -> Image r u) -> LocImage r u
bindQuery_li1 qy fn = LocImage $ \ctx pt -> 
    let ans = runQuery qy ctx 
        sz  = dc_font_size ctx        
    in runImage (fn ans $ uconvertExt sz pt) ctx
-}

-- | Use a Loc query to generate ans @ans@ turn the @ans@ into an
-- @Image@ projecting up to a @LocImage@.
--
bindLocQuery_li :: LocQuery u ans -> (ans -> Image r u) -> LocImage r u
bindLocQuery_li qry fn = LocImage $ \ctx pt -> 
    let f1 = runQuery qry ctx in runImage (fn $ f1 pt) ctx




