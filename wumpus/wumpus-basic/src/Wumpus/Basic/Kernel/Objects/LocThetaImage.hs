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

   , ignoreTheta -- needs new name
   , makeLocThetaGraphic
   , uptoLocThetaImage2
   , uptoLocThetaImage1

   , intoLocThetaImage
   , intoLocThetaImage2
   , withLocThetaQuery
   )

   where

import Wumpus.Basic.Kernel.Base.BaseDefs
import Wumpus.Basic.Kernel.Base.DrawingContext
import Wumpus.Basic.Kernel.Objects.Image
import Wumpus.Basic.Kernel.Objects.ImageBasis
import Wumpus.Basic.Kernel.Objects.LocImage
import Wumpus.Basic.Kernel.Objects.Query

import Wumpus.Core                              -- package: wumpus-core




-- | Graphic - function from DrawingContext and start point to a 
-- polymorphic /answer/ and a graphic /primitive/.
--
-- The answer is expected to be a Functor.
--
newtype LocThetaImage t u = LocThetaImage { 
    getLocThetaImage :: DrawingContext -> Point2 u -> Radian -> (t u, Primitive) }


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
    bimap l r $ getLocThetaImage gf ctx pt ang


-- This needs drawing context so cannot be done with 'bimapLocThetaImage'.
--
instance UnitConvertExt t => UnitConvert (LocThetaImage t) where
  uconvert gf = LocThetaImage $ \ctx pt ang -> 
      let sz    = dc_font_size ctx
          (a,o) = getLocThetaImage gf ctx (uconvertExt sz pt) ang
      in (uconvertExt sz a, o)

-- movestartLocThetaImage :: (Point2 u -> Point2 u) -> LocThetaImage t u -> LocThetaImage t u
-- movestartLocThetaImage fn gf = LocThetaImage $ \ctx pt -> getLocThetaImage gf ctx (fn pt) 


-- LocTheta objects do not have Affine instances.
--
-- Downcast them to LocImage before transforming...
--


instance Localize LocThetaImage where
   localize upd gf = LocThetaImage $ \ctx pt ang -> 
                       getLocThetaImage gf (upd ctx) pt ang


instance Hyperlink (LocThetaImage t u) where
  hyperlink hyp = bimapLocThetaImage id (xlinkPrim hyp)



instance IgnoreAns LocThetaImage where
  ignoreAns    = bimapLocThetaImage (const UNil) id
  replaceAns o = bimapLocThetaImage (const o) id


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


ignoreTheta :: LocImage u a -> LocThetaImage u a
ignoreTheta gf = LocThetaImage $ \ctx pt _ -> runLocImage gf ctx pt


makeLocThetaGraphic :: InterpretUnit u
               => (DrawingContext -> a) 
               -> (a -> DPoint2 -> Radian -> Primitive) 
               -> LocThetaGraphic u
makeLocThetaGraphic qry fn = LocThetaImage $ \ctx pt ang -> 
    let ans = qry ctx 
        sz  = dc_font_size ctx
    in (UNil, fn ans (uconvertExt sz pt) ang)

-- Name convention with arity suffixes is good for the upto functions!

uptoLocThetaImage2 :: Image t u -> LocThetaImage t u
uptoLocThetaImage2 gf = LocThetaImage $ \ctx _ _ -> runImage gf ctx

uptoLocThetaImage1 :: LocImage t u -> LocThetaImage t u
uptoLocThetaImage1 gf = LocThetaImage $ \ctx pt _ -> runLocImage gf ctx pt




intoLocThetaImage :: (Point2 u -> Radian -> t u) 
                  -> LocThetaGraphic u
                  -> LocThetaImage t u
intoLocThetaImage fn gf = LocThetaImage $ \ctx pt ang -> 
   let ans   = fn pt ang
       (_,o) = getLocThetaImage gf ctx pt ang
   in (ans,o)


intoLocThetaImage2 :: InterpretUnit u
              => (DrawingContext -> a) 
              -> (a -> Point2 u -> Radian -> t u) 
              -> LocThetaGraphic u
              -> LocThetaImage t u
intoLocThetaImage2 extr fn gf = LocThetaImage $ \ctx pt ang -> 
   let a     = extr ctx
       ans   = fn a pt ang
       (_,o) = getLocThetaImage gf ctx pt ang
   in (ans,o)


-- | Use a Loc query to generate ans @ans@ turn the @ans@ into an
-- @Image@ projecting up to a @LocImage@.
--
withLocThetaQuery :: LocThetaQuery u ans -> (ans -> Image t u) -> LocThetaImage t u
withLocThetaQuery qry fn = LocThetaImage $ \ctx pt ang -> 
    let ans = runLocThetaQuery qry ctx pt ang in runImage (fn ans) ctx

