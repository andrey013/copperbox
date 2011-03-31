{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE UndecidableInstances       #-}
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
  , trafoImageAns

  , decorate
  , annotate
  , hyperlink
  , clipObject

  , combind

  ) where

import Wumpus.Basic.Kernel.Base.BaseDefs
import Wumpus.Basic.Kernel.Base.WrappedPrimitive

import Wumpus.Core                              -- package: wumpus-core


-- Not exported - thanks to Max Bollingbroke.
--
type family   GuardEqAns a b :: *
type instance GuardEqAns a a = a

-- | An Image always returns a pair of some polymorphic answer @a@
-- and a PrimGraphic.
--
data ImageAns t u       = Ans (t u) CatPrim

type instance DUnit (ImageAns t u) = GuardEqAns u (DUnit (t u))

type GraphicAns u       = ImageAns UNil u


instance Functor t => Functor (ImageAns t) where
  fmap f (Ans a prim) = Ans (fmap f a) prim

instance OPlus (t u) => OPlus (ImageAns t u) where
  Ans a p1 `oplus` Ans b p2 = Ans (a `oplus` b) (p1 `oplus` p2)


--------------------------------------------------------------------------------
-- Affine instances 

-- 
-- Design Note
--
-- Translate and RotateAbout require the unit to be /scalar/ 
-- e.g. Double, Centimeter, Pica.
--
-- This is annoying and a limitation, but an alternative would
-- need access to current-font-size which cannot be a pure 
-- function.
-- 

instance Rotate (t u) => Rotate (ImageAns t u) where
  rotate ang (Ans a p) = Ans (rotate ang a) (rotate ang p)


instance (RotateAbout (t u), ScalarUnit u, u ~ DUnit (t u)) => 
    RotateAbout (ImageAns t u) where
  rotateAbout ang pt@(P2 x y) (Ans a p) = 
    Ans (rotateAbout ang pt a) 
        (rotateAbout ang (P2 (toPsPoint x) (toPsPoint y)) p)


instance Scale (t u) => Scale (ImageAns t u) where
  scale sx sy (Ans a p) = Ans (scale sx sy a) (scale sx sy p)


instance (Translate (t u), ScalarUnit u, u ~ DUnit (t u)) => 
    Translate (ImageAns t u) where
  translate dx dy (Ans a p) = 
    Ans (translate dx dy a) (translate (toPsPoint dx) (toPsPoint dy) p)


--------------------------------------------------------------------------------


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
mapAns f = fmap (trafoImageAns f id) 



-- | Transform both the answer produced by a graphic object and 
-- transform the primitive drawing.
--
-- Note - the new answer must share the same unit type as the
-- initial answer, although it does not need to have the same
-- wrapper type. Also this function is specifically exposed to
-- enable affine transofrmations - it is not expected to be 
-- generally useful.
--
trafoImageAns :: (t u -> t1 u) -> (CatPrim -> CatPrim) 
              -> ImageAns t u -> ImageAns t1 u
trafoImageAns f g (Ans a prim) = Ans (f a) (g prim)


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
decorate img gf = combind const img (const gf) 

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
annotate img gf = combind const img gf


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




-- | This is a very general monadic combiner.
-- 
-- The first argument is a pure combiner cf. @liftM2@, @liftA2@
-- 
-- The second argument is an Image to be evaluated.
--
-- The third argument, uses the ouput from the first Image to 
-- build a second Image.
-- 
-- The function concatenates the CatPrims formed by both Images
-- and uses the pure combiner to build an answer from the
-- intermediate answers.
--
combind :: Monad cf 
        => (t1 u -> t2 u -> t3 u)
        -> cf (ImageAns t1 u) 
        -> (t1 u -> cf (ImageAns t2 u)) 
        -> cf (ImageAns t3 u) 
combind op gf fn = gf   >>= \(Ans a p1) -> 
                   fn a >>= \(Ans b p2) -> 
                   return $ Ans (a `op` b) (p1 `oplus` p2)

