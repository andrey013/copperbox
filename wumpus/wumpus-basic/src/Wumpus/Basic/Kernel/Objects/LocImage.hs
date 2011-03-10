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

   , withLocQuery
   , withLocQuery2 -- clearly needs a new name

   , makeLocGraphic
   , uptoLocImage
   , intoLocImage
   , intoLocImage2     -- needs a new name
   , rawLocImage
   , runLocImage
   , at

   )

   where

import Wumpus.Basic.Kernel.Base.BaseDefs
import Wumpus.Basic.Kernel.Base.DrawingContext
import Wumpus.Basic.Kernel.Objects.Image
import Wumpus.Basic.Kernel.Objects.ImageBasis
import Wumpus.Basic.Kernel.Objects.Query

import Wumpus.Core                              -- package: wumpus-core




-- | Graphic - function from DrawingContext and start point to a 
-- polymorphic /answer/ and a graphic /primitive/.
--
-- The answer is expected to be a Functor.
--
newtype LocImage t u = LocImage { 
          getLocImage :: DrawingContext -> Point2 u -> (t u, Primitive) }


-- | LocGraphic - function from DrawingContext and start point to 
-- a graphic /primitive/.
--
type LocGraphic u = LocImage UNil u


instance OPlus (t u) => OPlus (LocImage t u) where
  fa `oplus` fb = LocImage $ \ctx pt -> 
                    getLocImage fa ctx pt `oplus` getLocImage fb ctx pt

--
-- DESIGN NOTE:
--
-- LocImage is not a Functor - we can change the current type 
-- slightly and have a Functor instance (@a@ for answer rather 
-- than @t u@, but then we can\'t have a UnitConvert instance.
--


-- bimap has to be unit preserving as unit is a parmater of the 
-- input as well as the output.
--
bimapLocImage :: (t u -> t' u) -> (Primitive -> Primitive) 
           -> LocImage t u -> LocImage t' u
bimapLocImage l r gf = LocImage $ \ctx pt -> 
    bimap l r $ getLocImage gf ctx pt


-- This needs drawing context so cannot be done with 'bimapLocImage'.
--
instance UnitConvertExt t => UnitConvert (LocImage t) where
  uconvert gf = LocImage $ \ctx pt -> 
      let sz    = dc_font_size ctx
          (a,o) = getLocImage gf ctx (uconvertExt sz pt)
      in (uconvertExt sz a, o)

-- movestartLocImage :: (Point2 u -> Point2 u) -> LocImage t u -> LocImage t u
-- movestartLocImage fn gf = LocImage $ \ctx pt -> getLocImage gf ctx (fn pt) 


-- Affine instances transfom the start point as well as the 
-- answer.

instance (InterpretUnit u, Rotate (t u)) => Rotate (LocImage t u) where
  rotate ang gf = LocImage $ \ctx pt -> 
      let trafo = intraMapPoint (dc_font_size ctx) (rotate ang)
          (a,o) = getLocImage gf ctx (trafo pt)
      in (rotate ang a, rotate ang o)


instance (InterpretUnit u, Scale (t u)) => Scale (LocImage t u) where
  scale sx sy gf = LocImage $ \ctx pt -> 
      let trafo = intraMapPoint (dc_font_size ctx) (scale sx sy)
          (a,o) = getLocImage gf ctx (trafo pt)
      in (scale sx sy a, scale sx sy o)


instance (InterpretUnit u, RotateAbout (t u)) => 
    RotateAbout (LocImage t u) where
  rotateAbout ang p0 gf = LocImage $ \ctx pt -> 
      let dP0   = uconvertExt (dc_font_size ctx) p0
          trafo = intraMapPoint (dc_font_size ctx) (rotateAbout ang dP0)
          (a,o) = getLocImage gf ctx (trafo pt)
      in (rotateAbout ang dP0 a, rotateAbout ang dP0 o)


instance (InterpretUnit u, Translate (t u)) => Translate (LocImage t u) where
  translate dx dy gf = LocImage $ \ctx pt -> 
      let trafo = intraMapPoint (dc_font_size ctx) (translate dx dy)
          (a,o) = getLocImage gf ctx (trafo pt)
      in (translate dx dy a, translate dx dy o)


instance Localize LocImage where
   localize upd gf = LocImage $ \ctx pt -> getLocImage gf (upd ctx) pt


instance MoveStart LocImage where
  moveStart fn gf = LocImage $ \ctx pt -> getLocImage gf ctx (fn pt)

instance Hyperlink (LocImage t u) where
  hyperlink hyp = bimapLocImage id (xlinkPrim hyp)



instance IgnoreAns LocImage where
  ignoreAns    = bimapLocImage (const UNil) id
  replaceAns o = bimapLocImage (const o) id


instance Annotate LocImage where
  annotate = annoLocImg
  decorate = decoLocImg



decoLocImg :: LocImage t u -> LocGraphic u -> LocImage t u
decoLocImg fa fb = LocImage $ \ctx pt -> 
    let (a,o1) = getLocImage fa ctx pt
        (_,o2) = getLocImage fb ctx pt
    in (a, o1 `oplus` o2)
                        
annoLocImg :: LocImage t u -> (t u -> LocGraphic u) -> LocImage t u
annoLocImg fa mf = LocImage $ \ctx pt -> 
    let (a,o1) = getLocImage fa ctx pt
        (_,o2) = getLocImage (mf a) ctx pt
    in (a, o1 `oplus` o2)


instance OpBind LocImage where
  opbind = opbindLocImg

opbindLocImg :: (t u -> t u -> t u) 
             -> LocImage t u -> (t u -> LocImage t u) -> LocImage t u
opbindLocImg op gf fn = LocImage $ \ctx pt -> 
    let (a,o1) = getLocImage gf ctx pt
        (b,o2) = getLocImage (fn a) ctx pt
    in (a `op` b, o1 `oplus` o2)



-- NOTE - Image building needs sorting out into a prime setof 
-- functions.
--

-- | Use a Loc query to generate ans @ans@ turn the @ans@ into an
-- @Image@ projecting up to a @LocImage@.
--
withLocQuery :: LocQuery u ans -> (ans -> Image t u) -> LocImage t u
withLocQuery qry fn = LocImage $ \ctx pt -> 
    let ans = runLocQuery qry ctx pt in runImage (fn ans) ctx


-- | Use a Query to generate ans @ans@ turn the @ans@ with the
-- builder.
--
withLocQuery2 :: Query ans -> (ans -> LocImage t u) -> LocImage t u
withLocQuery2 qry fn = LocImage $ \ctx pt -> 
    let ans = runQuery qry ctx in runLocImage (fn ans) ctx pt



makeLocGraphic :: InterpretUnit u
               => (DrawingContext -> a) 
               -> (a -> DPoint2 -> Primitive) 
               -> LocGraphic u
makeLocGraphic qry fn = LocImage $ \ctx pt -> 
    let ans = qry ctx 
        sz  = dc_font_size ctx
    in (UNil, fn ans (uconvertExt sz pt))



uptoLocImage :: Image t u -> LocImage t u
uptoLocImage gf = LocImage $ \ctx _ -> runImage gf ctx


intoLocImage :: (Point2 u -> t u) 
             -> LocGraphic u
             -> LocImage t u
intoLocImage fn gf = LocImage $ \ctx pt -> 
   let ans   = fn pt
       (_,o) = getLocImage gf ctx pt
   in (ans,o)


intoLocImage2 :: InterpretUnit u
              => (DrawingContext -> a) 
              -> (a -> Point2 u -> t u) 
              -> LocGraphic u
              -> LocImage t u
intoLocImage2 extr fn gf = LocImage $ \ctx pt -> 
   let a     = extr ctx
       ans   = fn a pt
       (_,o) = getLocImage gf ctx pt
   in (ans,o)


-- This seems to be the one for down casting...
-- 
rawLocImage :: (DrawingContext -> Point2 u -> (t u, Primitive)) -> LocImage t u
rawLocImage fn = LocImage $ \ctx pt -> fn ctx pt


runLocImage :: LocImage t u -> DrawingContext -> Point2 u -> (t u, Primitive)
runLocImage gf ctx pt = getLocImage gf ctx pt



infixr 1 `at`

at :: LocImage t u -> Point2 u -> Image t u
gf `at` pt = rawImage $ \ctx ->  getLocImage gf ctx pt