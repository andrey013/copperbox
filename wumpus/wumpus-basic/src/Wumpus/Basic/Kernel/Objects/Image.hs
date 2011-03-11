{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Kernel.Objects.Image
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  highly unstable
-- Portability :  GHC 
--
-- Image and Graphic types - these are functional types from the 
-- DrawingContext to a graphic /primitive/.
--
--------------------------------------------------------------------------------

module Wumpus.Basic.Kernel.Objects.Image
   (
     Graphic
   , Image

   , runImage
   , rawImage
   , intoImage

   , bindQuery_i

   , makeGraphic
   
   )

   where

import Wumpus.Basic.Kernel.Base.BaseDefs
import Wumpus.Basic.Kernel.Base.DrawingContext
import Wumpus.Basic.Kernel.Base.WrappedPrimitive
import Wumpus.Basic.Kernel.Objects.ImageBasis
import Wumpus.Basic.Kernel.Objects.Query

import Wumpus.Core                              -- package: wumpus-core

import Data.Monoid


-- | Image - function from the DrawingContext to a polymorphic 
-- /answer/ and a graphic /primitive/.
--
-- The answer is expected to be a Functor.
--
newtype Image t u = Image { getImage :: DrawingContext -> (t u, CatPrim) }

-- | Graphic - function from the DrawingContext to a graphic 
-- /primitive/.
--
type Graphic u = Image UNil u



instance OPlus (t u) => OPlus (Image t u) where
  fa `oplus` fb = Image $ \ctx -> 
                    getImage fa ctx `oplus` getImage fb ctx


instance Functor t => Functor (Image t) where
  fmap f gf = Image $ \ctx -> let (a,o) = getImage gf ctx
                              in (fmap f a, o)

-- Image cannot be Applicative - we cannot synthesize an empty
-- primitive without a start point.
--

bimapImage :: (t u -> t' u) -> (Primitive -> Primitive) 
           -> Image t u -> Image t' u
bimapImage l r gf = Image $ \ctx -> bimap l (cpmap r) $ getImage gf ctx

-- This needs drawing context so cannot be done with 'ansMapImage'.
--
instance UnitConvertExt t => UnitConvert (Image t) where
  uconvert gf = Image $ \ctx -> let (a,o) = getImage gf ctx
                                    sz    = dc_font_size ctx
                                in (uconvertExt sz a, o)



instance Rotate (t u) => Rotate (Image t u) where
  rotate ang = bimapImage (rotate ang) (rotate ang)

instance Scale (t u) => Scale (Image t u) where
  scale sx sy = bimapImage (scale sx sy) (scale sx sy)

instance RotateAbout (t u) => RotateAbout (Image t u) where
  rotateAbout ang pt = bimapImage (rotateAbout ang pt) (rotateAbout ang pt)

instance Translate (t u) => Translate (Image t u) where
  translate dx dy = bimapImage (translate dx dy) (translate dx dy)



instance Localize Image where
   localize upd gf = Image $ \ctx -> getImage gf (upd ctx) 

instance Hyperlink (Image t u) where
  hyperlink hyp = bimapImage id (xlinkPrim hyp)



instance IgnoreAns Image where
  ignoreAns    = bimapImage (const UNil) id
  replaceAns o = bimapImage (const o) id


instance UMonad Image where
  bind = bindImg
  unit = unitImg

bindImg :: Image t u -> (t u -> Image t1 u) -> Image t1 u
bindImg gf fn = Image $ \ctx -> 
    let (a,o1) = getImage gf ctx
        (b,o2) = getImage (fn a) ctx
    in (b, o1 `oplus` o2)


unitImg :: t u -> Image t u
unitImg a = Image $ \_ -> (a, mempty)

instance Annotate Image where
  annotate = annoImg
  decorate = decoImg

decoImg :: Image t u -> Graphic u -> Image t u
decoImg fa fb = Image $ \ctx -> 
    let (a,o1) = getImage fa ctx 
        (_,o2) = getImage fb ctx
    in (a,o1 `mappend` o2)
                        
annoImg :: Image t u -> (t u -> Graphic u) -> Image t u
annoImg fa mf = Image $ \ctx -> 
    let (a,o1) = getImage fa ctx 
        (_,o2) = getImage (mf a) ctx
    in (a,o1 `mappend` o2)


--------------------------------------------------------------------------------
-- builders and destructors

runImage :: Image t u -> DrawingContext -> (t u, CatPrim)
runImage gf ctx = getImage gf ctx

-- | This is for donwcasting LocImages, Connectors, etc. into Image.
--
rawImage :: (DrawingContext -> (t u, CatPrim)) -> Image t u
rawImage fn = Image $ \ctx -> fn ctx


-- Design note - there are no promoters or lifters, discounting 
-- the DrawingContext, Images are considered arity zero.
--

intoImage :: Query (t u) -> Graphic u -> Image t u
intoImage fn gf = Image $ \ctx -> 
   let ans   = runQuery fn ctx 
       (_,o) = getImage gf ctx
   in (ans,o)


makeGraphic :: (DrawingContext -> a) -> (a -> Primitive) -> Graphic u
makeGraphic qry fn = Image $ \ctx -> let a = qry ctx in (UNil, prim1 $ fn a)




-- | Use a Query to generate ans @ans@ turn the @ans@ with the
-- builder.
--
bindQuery_i :: Query ans -> (ans -> Image t u) -> Image t u
bindQuery_i qy fn = Image $ \ctx -> 
    let ans = runQuery qy ctx in runImage (fn ans) ctx



