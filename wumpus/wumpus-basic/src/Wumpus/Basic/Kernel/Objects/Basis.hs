{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Kernel.Objects.Basis
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  highly unstable
-- Portability :  GHC 
--
-- Experimental extras for BaseDefs...
-- 
--
--------------------------------------------------------------------------------

module Wumpus.Basic.Kernel.Objects.Basis
  (

    ImageAns(..)
  , GraphicAns

  , ignoreAns
  , replaceAns
  , mapAns

  , decorate
  , annotate
  , hyperlink
  , clipObject

  ) where

import Wumpus.Basic.Kernel.Base.BaseDefs
import Wumpus.Basic.Kernel.Base.WrappedPrimitive

import Wumpus.Core                              -- package: wumpus-core


-- | An Image always returns a pair of some polymorphic answer @a@
-- and a PrimGraphic.
--
data ImageAns t u       = Ans (t u) CatPrim


type GraphicAns u       = ImageAns UNil u


instance Functor t => Functor (ImageAns t) where
  fmap f (Ans a prim) = Ans (fmap f a) prim

instance OPlus (t u) => OPlus (ImageAns t u) where
  Ans a p1 `oplus` Ans b p2 = Ans (a `oplus` b) (p1 `oplus` p2)


-- | Ignore the answer produced by an Image (or LocImage, etc.)
-- and form a Graphic instead.
--
ignoreAns :: Functor cf
          => cf (ImageAns t u) -> cf (GraphicAns u)
ignoreAns = fmap (\(Ans _ prim) -> Ans UNil prim)


-- | Replace the answer produced by a graphic object.
--
-- Note - the new answer must share the same unit type as the
-- initial answer, although it does not need to have the same
-- wrapper type.
--
replaceAns :: Functor cf
          => t1 u -> cf (ImageAns t u) -> cf (ImageAns t1 u)
replaceAns ans = fmap (\(Ans _ prim) -> Ans ans prim)


-- | Map the answer produced by a graphic object.
--
-- Note - the new answer must share the same unit type as the
-- initial answer, although it does not need to have the same
-- wrapper type.
--
mapAns :: Functor cf
          => (t u -> t1 u) -> cf (ImageAns t u) -> cf (ImageAns t1 u)
mapAns f = fmap (\(Ans a prim) -> Ans (f a) prim)


-- | Decorate an Image by superimposing a Graphic.
--
-- Note - this function has a very general type signature and
-- supports various graphic types:
--
-- > decorate :: Image u a -> Graphic u -> Image u a
-- > decorate :: LocImage u a -> LocGraphic u -> LocImage u a
-- > decorate :: LocThetaImage u a -> LocThetaGraphic u -> LocTheteImage u a
--
decorate :: Monad cf
         => cf (ImageAns t u) -> cf (GraphicAns u) -> cf (ImageAns t u) 
decorate img gf = 
    img >>= \(Ans a g1) -> 
    gf  >>= \(Ans _ g2) -> 
    return $ Ans a (g1 `oplus` g2)


-- | Version of 'decorate' where the annotation Graphic has access 
-- to the result produced by the Image.
--
-- Again, this function has a very general type signature and
-- supports various graphic types:
--
-- > annotate :: Image u a -> Graphic u -> Image u a
-- > annotate :: LocImage u a -> LocGraphic u -> LocImage u a
-- > annotate :: LocThetaImage u a -> LocThetaGraphic u -> LocTheteImage u a
--
annotate :: Monad cf 
         => cf (ImageAns t u) -> (t u -> cf (GraphicAns u)) -> cf (ImageAns t u)
annotate img f = 
    img >>= \(Ans a g1) -> 
    f a >>= \(Ans _ g0) -> 
    return $ Ans a (g0 `oplus` g1)


-- | Hyperlink a graphic object.
-- 
-- This function has a very general type signature and supports 
-- various graphic types:
--
-- > hyperlink :: XLink -> Graphic u -> Graphic u
-- > hyperlink :: XLink -> Image u a -> Image u a
-- > hyperlink :: XLink -> LocImage u a -> LocImage u a
-- > hyperlink :: XLink -> LocThetaImage u a -> LocThetaImage u a
--
hyperlink :: Functor cf 
          => XLink -> cf (ImageAns t u) -> cf (ImageAns t u)
hyperlink hypl = 
    fmap (\(Ans a prim) -> Ans a (cpmap (xlinkPrim hypl) prim))


-- | Clip a graphic object.
-- 
clipObject :: Functor cf 
           => PrimPath -> cf (ImageAns t u) -> cf (ImageAns t u)
clipObject pp = 
    fmap (\(Ans a prim) -> Ans a (cpmap (clip pp) prim))


--------------------------------------------------------------------------------


-- affine trans

instance Rotate (t Double) => Rotate (ImageAns t Double) where
  rotate ang (Ans ma p) = Ans (rotate ang ma) (rotate ang p)


instance RotateAbout (t Double) => RotateAbout (ImageAns t Double) where
  rotateAbout ang pt (Ans ma p) = 
    Ans (rotateAbout ang pt ma) (rotateAbout ang pt p)


instance Scale (t Double) => Scale (ImageAns t Double) where
  scale sx sy (Ans ma p) = Ans (scale sx sy ma) (scale sx sy p)


instance Translate (t Double) => Translate (ImageAns t Double) where
  translate dx dy (Ans ma p) = Ans (translate dx dy ma) (translate dx dy p)
