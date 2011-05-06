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
  
  , szconvAnsF
  , szconvAnsZ


  , at
  , incline
  , atIncline
  , connect

  , replaceAnsR0
  , replaceAnsR1
  , replaceAnsR2

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
import Data.Monoid


type LocQuery u a               = CF (Point2 u -> a)
type LocThetaQuery u a          = CF (Point2 u -> Radian -> a)
type ConnectorQuery u a         = CF (Point2 u -> Point2 u -> a)


-- Design note - GraphicAns needs a unit for consistency even 
-- though it is never scrutinized.
-- 


data ImageAns u a = Ans CatPrim a

type GraphicAns u = ImageAns u (UNil u)



type instance DUnit (ImageAns u a) = u


--------------------------------------------------------------------------------
-- OPlus and monoid


instance OPlus a => OPlus (ImageAns u a) where
  Ans cp0 a `oplus` Ans cp1 b = Ans (cp0 `oplus` cp1) (a `oplus` b)

-- Note - like CF the mconcat definition avoids starting with 
-- mempty.
--
instance Monoid a => Monoid (ImageAns u a) where
  mempty                        = Ans mempty mempty
  Ans cp0 a `mappend` Ans cp1 b = Ans (cp0 `mappend` cp1) (a `mappend` b)

  mconcat []      = mempty
  mconcat (a:as)  = step a as
    where
      step ac []     = ac
      step ac (x:xs) = step (ac `mappend` x) xs


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



szconvAnsF :: (Functor t, InterpretUnit u, InterpretUnit u1) 
                => FontSize -> ImageAns u (t u) -> ImageAns u1 (t u1)
szconvAnsF sz (Ans prim a) = Ans prim (uconvertF sz a)

szconvAnsZ :: FontSize -> ImageAns u a -> ImageAns u1 a
szconvAnsZ _ (Ans prim a) = Ans prim a



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



-- | Replace the ans - arity 0.
-- 
replaceAnsR0 :: ans -> CF (ImageAns u a) -> CF (ImageAns u ans)
replaceAnsR0 ans = fmap (replaceAns ans)


-- | Replace the ans - arity 1.
--
replaceAnsR1 :: ans -> CF (r1 -> ImageAns u a) -> CF (r1 -> ImageAns u ans)
replaceAnsR1 ans = fmap $ fmap (replaceAns ans)


-- | Replace the ans - arity 2.
--
replaceAnsR2 :: ans 
             -> CF (r1 -> r2 -> ImageAns u a) 
             -> CF (r1 -> r2 -> ImageAns u ans)
replaceAnsR2 ans = fmap $ fmap $ fmap (replaceAns ans) 


--
-- DESIGN NOTE
--
-- The argument orders for decorate and elaborate are 
-- uncharacteristic. Whilst the functions are essentially 
-- transformers:
-- 
-- >  a -> (b -> b)
-- 
-- ... the dataflow is better represented by the order they use:
--
-- > b -> a -> b
-- 


-- | Decorate an Image by superimposing a Graphic.
--
-- Note - this function has a very general type signature and
-- supports various graphic types:
--
decorateR0 :: CF (ImageAns u a) -> CF (GraphicAns u) -> CF (ImageAns u a) 
decorateR0 img gf = op <$> img <*> gf
  where
    op (Ans cp a) (Ans cp1 _) = Ans (cp `oplus` cp1) a


-- | Decorate an arity 1 Image by superimposing a arity 1 Graphic.
--
-- Note - this function has a very general type signature and
-- supports various graphic types. The usual case is to decorate
-- a LocGraphic with a LocImage.
--
decorateR1 :: CF (r1 -> ImageAns u a) 
           -> CF (r1 -> GraphicAns u) 
           -> CF (r1 -> ImageAns u a) 
decorateR1 img gf = promoteR1 $ \r1 ->
    op <$> apply1R1 img r1 <*> apply1R1 gf r1
  where
    op (Ans cp a) (Ans cp1 _) = Ans (cp `oplus` cp1) a


-- | Decorate an arity 2 Image by superimposing a arity 2 Graphic.
--
-- Note - this function has a very general type signature and
-- supports various graphic types. 
--
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



