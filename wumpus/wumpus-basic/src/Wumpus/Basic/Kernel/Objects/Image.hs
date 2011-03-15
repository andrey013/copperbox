{-# LANGUAGE TypeFamilies               #-}
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
import Wumpus.Basic.Kernel.Objects.Basis
import Wumpus.Basic.Kernel.Objects.Query

import Wumpus.Core                              -- package: wumpus-core

import Data.Monoid


-- | Image - function from the DrawingContext to a polymorphic 
-- /answer/ and a graphic /primitive/.
--
-- The answer is expected to be a Functor.
--
newtype Image r u = Image { getImage :: DrawingContext -> (r u, CatPrim) }

type instance Answer (Image r u)     = r u


-- | Graphic - function from the DrawingContext to a graphic 
-- /primitive/.
--
type Graphic u = Image UNil u


--------------------------------------------------------------------------------

instance BindQuery (Image r u) where
  (&=>) = bindQuery_i

-- Images have no arity except the DrawingContext so there is no
-- BindQueryRN instance.



--------------------------------------------------------------------------------
instance OPlus (r u) => OPlus (Image r u) where
  fa `oplus` fb = Image $ \ctx -> 
                    getImage fa ctx `oplus` getImage fb ctx


instance Functor r => Functor (Image r) where
  fmap f gf = Image $ \ctx -> let (a,o) = getImage gf ctx
                              in (fmap f a, o)

-- Image cannot be Applicative - we cannot synthesize an empty
-- primitive without a start point.
--

bimapImage :: (r u -> r1 u) -> (Primitive -> Primitive) 
           -> Image r u -> Image r1 u
bimapImage l r gf = Image $ \ctx -> bimap l (cpmap r) $ getImage gf ctx

-- This needs drawing context so cannot be done with 'ansMapImage'.
--
instance Functor r => UnitConvert (Image r) where
  uconvert gf = Image $ \ctx -> let (a,o) = getImage gf ctx
                                    sz    = dc_font_size ctx
                                in (uconvertF sz a, o)



instance Rotate (r u) => Rotate (Image r u) where
  rotate ang = bimapImage (rotate ang) (rotate ang)

instance Scale (r u) => Scale (Image r u) where
  scale sx sy = bimapImage (scale sx sy) (scale sx sy)

instance RotateAbout (r u) => RotateAbout (Image r u) where
  rotateAbout ang pt = bimapImage (rotateAbout ang pt) (rotateAbout ang pt)

instance Translate (r u) => Translate (Image r u) where
  translate dx dy = bimapImage (translate dx dy) (translate dx dy)



instance Object Image where
  local_ctx upd gf = Image $ \ctx -> getImage gf (upd ctx) 
  ignoreAns        = bimapImage (const UNil) id
  replaceAns o     = bimapImage (const o) id
  mapAns f         = bimapImage f id
  hyperlink hyp    = bimapImage id (xlinkPrim hyp)
  clipObject pp    = bimapImage id (clip pp)
  annotate         = annoImg
  decorate         = decoImg
  bind             = bindImg
  unit             = unitImg

bindImg :: Image r u -> (r u -> Image r1 u) -> Image r1 u
bindImg gf fn = Image $ \ctx -> 
    let (a,o1) = getImage gf ctx
        (b,o2) = getImage (fn a) ctx
    in (b, o1 `oplus` o2)


unitImg :: r u -> Image r u
unitImg a = Image $ \_ -> (a, mempty)


decoImg :: Image r u -> Graphic u -> Image r u
decoImg fa fb = Image $ \ctx -> 
    let (a,o1) = getImage fa ctx 
        (_,o2) = getImage fb ctx
    in (a,o1 `mappend` o2)
                        
annoImg :: Image r u -> (r u -> Graphic u) -> Image r u
annoImg fa mf = Image $ \ctx -> 
    let (a,o1) = getImage fa ctx 
        (_,o2) = getImage (mf a) ctx
    in (a,o1 `mappend` o2)


--------------------------------------------------------------------------------
-- builders and destructors

runImage :: Image r u -> DrawingContext -> (r u, CatPrim)
runImage gf ctx = getImage gf ctx

-- | This is for donwcasting LocImages, Connectors, etc. into Image.
--
rawImage :: (DrawingContext -> (r u, CatPrim)) -> Image r u
rawImage fn = Image $ \ctx -> fn ctx


-- Design note - there are no promoters or lifters, discounting 
-- the DrawingContext, Images are considered arity zero.
--

intoImage :: Query (r u) -> Graphic u -> Image r u
intoImage fn gf = Image $ \ctx -> 
   let ans   = runQuery fn ctx 
       (_,o) = getImage gf ctx
   in (ans,o)


makeGraphic :: Query a -> (a -> Primitive) -> Graphic u
makeGraphic qry fn = Image $ \ctx -> 
    let a = runQuery qry ctx in (UNil, prim1 $ fn a)




-- | Use a Query to generate ans @ans@ turn the @ans@ with the
-- builder.
--
bindQuery_i :: Query ans -> (ans -> Image r u) -> Image r u
bindQuery_i qy fn = Image $ \ctx -> 
    let ans = runQuery qy ctx in runImage (fn ans) ctx



