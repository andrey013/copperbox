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
-- Common types and operations.
-- 
--
--------------------------------------------------------------------------------

module Wumpus.Basic.Kernel.Objects.Basis
  (

    LocQuery
  , LocThetaQuery
  , ConnectorQuery 

  , GraphicAns
  , ImageAns(..)

  , graphicAns
  , mapAns
  , replaceAns
  , ignoreAns
  , answer
  , hyperlink  
  , clipObject 
  
  , szconvGraphicAns 
  , szconvImageAnsF
  , szconvImageAnsZ


  , at
  , incline
  , atIncline
  , connect

  , decorateR0
  , decorateR1
  , decorateR2

  , elaborateR0
  , elaborateR1
  , elaborateR2
  

  ) where

import Wumpus.Basic.Kernel.Base.BaseDefs
import Wumpus.Basic.Kernel.Base.ContextFun
import Wumpus.Basic.Kernel.Base.WrappedPrimitive

import Wumpus.Core                              -- package: wumpus-core


import Control.Applicative

type LocQuery u a               = CF (Point2 u -> a)
type LocThetaQuery u a          = CF (Point2 u -> Radian -> a)
type ConnectorQuery u a         = CF (Point2 u -> Point2 u -> a)


-- Design note - GraphicAns needs a unit for consistency even 
-- though it is never scrutinized.
-- 


data ImageAns u a = Ans CatPrim a

type GraphicAns u = ImageAns u (UNil u)



type instance DUnit (ImageAns u a) = u


instance OPlus a => OPlus (ImageAns u a) where
  Ans cp0 a `oplus` Ans cp1 b = Ans (cp0 `oplus` cp1) (a `oplus` b)



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

instance Rotate a => Rotate (ImageAns u a) where
  rotate ang (Ans cp a) = Ans (rotate ang cp) (rotate ang a)


instance (RotateAbout a, ScalarUnit u, u ~ DUnit a) => 
    RotateAbout (ImageAns u a) where
  rotateAbout ang pt@(P2 x y) (Ans cp a) = 
    Ans (rotateAbout ang (P2 (toPsPoint x) (toPsPoint y)) cp)
        (rotateAbout ang pt a) 
        


instance Scale a => Scale (ImageAns u a) where
  scale sx sy (Ans cp a) = Ans (scale sx sy cp) (scale sx sy a)


instance (Translate a, ScalarUnit u, u ~ DUnit a) => 
    Translate (ImageAns u a) where
  translate dx dy (Ans cp a) = 
    Ans (translate (toPsPoint dx) (toPsPoint dy) cp) (translate dx dy a) 


--------------------------------------------------------------------------------



-- | Map the answer produced by a graphic object.
--
-- Note - the new answer must share the same unit type as the
-- initial answer, although it does not need to have the same
-- wrapper type.
--
mapAns :: (a -> a1) -> ImageAns u a -> ImageAns u a1
mapAns f (Ans cp a) = Ans cp (f a) 



-- | Replace the answer produced by a graphic object.
--
-- Note - the new answer must share the same unit type as the
-- initial answer, although it does not need to have the same
-- wrapper type.
--
replaceAns :: ans -> ImageAns u a -> ImageAns u ans
replaceAns ans (Ans prim _) = Ans prim ans


-- | Turn an imageAns into a GraphicAns by ignoring the 
-- result.
-- 
-- Usually this function will be used with one of the @push@ 
-- family of combinators.
--
-- > LocImage-to-LocGraphic = pushR1 ignoreAns 
--
ignoreAns :: ImageAns u a -> GraphicAns u
ignoreAns(Ans prim _) = Ans prim UNil



graphicAns :: CatPrim -> GraphicAns u
graphicAns prim = Ans prim UNil

-- | Extractor for the answer part of an image.
--
answer :: ImageAns u a -> a
answer (Ans _ a) = a


-- | Note - maybe this requires an arity family instead?
--
hyperlink :: XLink -> ImageAns u a -> ImageAns u a
hyperlink hypl (Ans prim a) = Ans (cpmap (xlinkPrim hypl) prim) a


-- | Clip a graphic object.
-- 
-- Note - maybe this requires an arity family instead?
--
clipObject :: PrimPath -> ImageAns t u -> ImageAns t u
clipObject pp (Ans prim a) =  Ans (cpmap (clip pp) prim) a


--------------------------------------------------------------------------------
-- Helpers for unit conversion...


szconvGraphicAns :: FontSize -> GraphicAns u -> GraphicAns u1
szconvGraphicAns _ (Ans prim _) = Ans prim UNil


szconvImageAnsF :: (Functor t, InterpretUnit u, InterpretUnit u1) 
                => FontSize -> ImageAns u (t u) -> ImageAns u1 (t u1)
szconvImageAnsF sz (Ans prim a) = Ans prim (uconvertF sz a)

szconvImageAnsZ :: FontSize -> ImageAns u a -> ImageAns u1 a
szconvImageAnsZ _ (Ans prim a) = Ans prim a



infixr 1 `at`


-- | Downcast a 'LocCF' function by applying it to the supplied 
-- point, making an arity-zero Context Function. 
-- 
-- Remember a 'LocCF' function is a 'CF1' context function where
-- the /static argument/ is specialized to a start point.
--
at :: LocQuery u a -> Point2 u -> CF a
at = apply1R1



infixr 1 `incline`


-- | Downcast a 'LocThetaQuery' function by applying it to the 
-- supplied angle, making an arity-one Context Function (a 
-- 'LocCF'). 
-- 
incline :: LocThetaQuery u a -> Radian -> LocQuery u a
incline = apply1R2


-- | Downcast a LocThetaQuery function by applying it to the 
-- supplied point and angle, making an arity-zero Context Function 
-- (a CF). 
--
atIncline :: LocThetaQuery u a -> Point2 u -> Radian -> CF a
atIncline = apply2R2


-- | Downcast a 'ConnectorQuery' function by applying it to the 
-- start and end point, making an arity-zero Context Function 
-- (a 'CF'). 
-- 
connect :: ConnectorQuery u a -> Point2 u -> Point2 u -> CF a
connect = apply2R2




-- | Decorate an Image by superimposing a Graphic.
--
-- Note - this function has a very general type signature and
-- supports various graphic types:
--
decorateR0 :: CF (ImageAns u a) -> CF (GraphicAns u) -> CF (ImageAns u a) 
decorateR0 img gf = op <$> img <*> gf
  where
    op (Ans cp a) (Ans cp1 _) = Ans (cp `oplus` cp1) a


decorateR1 :: CF (r1 -> ImageAns u a) 
           -> CF (r1 -> GraphicAns u) 
           -> CF (r1 -> ImageAns u a) 
decorateR1 img gf = promoteR1 $ \r1 ->
    op <$> apply1R1 img r1 <*> apply1R1 gf r1
  where
    op (Ans cp a) (Ans cp1 _) = Ans (cp `oplus` cp1) a


decorateR2 :: CF (r1 -> r2 -> ImageAns u a) 
           -> CF (r1 -> r2 -> GraphicAns u) 
           -> CF (r1 -> r2 -> ImageAns u a) 
decorateR2 img gf = promoteR2 $ \r1 r2 ->
    op <$> apply2R2 img r1 r2 <*> apply2R2 gf r1 r2
  where
    op (Ans cp a) (Ans cp1 _) = Ans (cp `oplus` cp1) a


-- | Decorate an Image by superimposing a Graphic.
--
-- Note - this function has a very general type signature and
-- supports various graphic types:
--
elaborateR0 :: CF (ImageAns u a) -> (a -> CF (GraphicAns u)) -> CF (ImageAns u a) 
elaborateR0 img gf = 
    img  >>= \(Ans p1 a) ->
    gf a >>= \(Ans p2 _) -> 
    return $ Ans (p1 `oplus` p2) a




-- | Decorate an Image by superimposing a Graphic.
--
-- Note - this function has a very general type signature and
-- supports various graphic types:
--
elaborateR1 :: CF (r1 -> ImageAns u a) 
            -> (a -> CF (r1 -> GraphicAns u)) 
            -> CF (r1 -> ImageAns u a) 
elaborateR1 img gf = promoteR1 $ \r1 -> 
    apply1R1 img r1    >>= \(Ans p1 a) ->
    apply1R1 (gf a) r1 >>= \(Ans p2 _) -> 
    return $ Ans (p1 `oplus` p2) a



elaborateR2 :: CF (r1 -> r2 -> ImageAns u a) 
            -> (a -> CF (r1 -> r2 -> GraphicAns u)) 
            -> CF (r1 -> r2 -> ImageAns u a) 
elaborateR2 img gf = promoteR2 $ \r1 r2 -> 
    apply2R2 img r1 r2    >>= \(Ans p1 a) ->
    apply2R2 (gf a) r1 r2 >>= \(Ans p2 _) -> 
    return $ Ans (p1 `oplus` p2) a




{-

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
ignoreAns :: Functor f
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


-- | Ante-decorate - version of 'decorate' where the decoration is 
-- drawn behind the Image.
--
adecorate :: Monad cf
          => cf (ImageAns t u) -> cf (GraphicAns u) -> cf (ImageAns t u) 
adecorate img gf = acombind const img (const gf)


-- | Version of 'elaborate' where the decorating Graphic has access 
-- to the result produced by the Image.
--
-- Again, this function has a very general type signature and
-- supports various graphic types:
--
-- > elaborate :: Image u a -> Graphic u -> Image u a
-- > elaborate :: LocImage u a -> LocGraphic u -> LocImage u a
-- > elaborate :: LocThetaImage u a -> LocThetaGraphic u -> LocTheteImage u a
--
elaborate :: Monad cf 
          => cf (ImageAns t u) 
          -> (t u -> cf (GraphicAns u)) 
          -> cf (ImageAns t u)
elaborate img gf = combind const img gf

-- | Ante-elaborate - version of 'elaborate' where the decoration 
-- is drawn behind the Image.
--
aelaborate :: Monad cf 
           => cf (ImageAns t u) -> (t u -> cf (GraphicAns u)) -> cf (ImageAns t u)
aelaborate img gf = acombind const img gf


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
-- NOTE - note useful with CF representation change.
--
combind :: Monad cf 
        => (t1 u -> t2 u -> t3 u)
        -> cf (ImageAns t1 u) 
        -> (t1 u -> cf (ImageAns t2 u)) 
        -> cf (ImageAns t3 u) 
combind op gf fn = gf   >>= \(Ans a p1) -> 
                   fn a >>= \(Ans b p2) -> 
                   return $ Ans (a `op` b) (p1 `oplus` p2)


-- | Version of combind where the drawing order is flipped.
--
acombind :: Monad cf 
         => (t1 u -> t2 u -> t3 u)
         -> cf (ImageAns t1 u) 
         -> (t1 u -> cf (ImageAns t2 u)) 
         -> cf (ImageAns t3 u) 
acombind op gf fn = gf   >>= \(Ans a p1) -> 
                    fn a >>= \(Ans b p2) -> 
                    return $ Ans (a `op` b) (p2 `oplus` p1)

-}