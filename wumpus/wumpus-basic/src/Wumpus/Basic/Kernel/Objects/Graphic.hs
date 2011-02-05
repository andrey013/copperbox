{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Kernel.Objects.Graphic
-- Copyright   :  (c) Stephen Tetley 2010-2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  highly unstable
-- Portability :  GHC 
--
-- Graphic type - this is largely equivalent to Primitive in
-- Wumpus-Core, but drawing attributes are implicitly supplied 
-- by the DrawingContext.
--
-- API in @Wumpus.Core@, but here they exploit the implicit 
-- @DrawingContext@.
--
--------------------------------------------------------------------------------

module Wumpus.Basic.Kernel.Objects.Graphic
  (

    Graphic
  , DGraphic


  -- * LocGraphic  
  , LocGraphic
  , DLocGraphic


  , LocThetaGraphic
  , DLocThetaGraphic

  -- * Functions
  , safeconcat
  , ignoreAns
  , replaceAns
  , mapAns

  , intoImage
  , intoLocImage
  , intoLocThetaImage

  , emptyLocGraphic

  , decorate
  , sdecorate
  , adecorate
  
  , hyperlink

  ) where

import Wumpus.Basic.Kernel.Base.BaseDefs
import Wumpus.Basic.Kernel.Base.ContextFun
import Wumpus.Basic.Kernel.Base.WrappedPrimitive
import Wumpus.Basic.Kernel.Objects.BaseObjects

import Wumpus.Core                              -- package: wumpus-core


import Control.Applicative

--------------------------------------------------------------------------------
-- Graphic

-- | Simple drawing - produce a primitive, access the DrawingContext
-- as required, e.g for fill colour, stroke colur, line width, etc.
--
type Graphic u          = Image u (UNil u)

-- | Alias of 'Graphic' where the unit type is specialized to 
-- Double. 
--
type DGraphic           = Graphic Double


-- | /Originated/ drawing - produce a primitive respective to the 
-- supplied start-point, access the DrawingContext as required.
--
type LocGraphic u       = LocImage u (UNil u)

-- | Alias of 'LocGraphic' where the unit type is specialized to 
-- Double. 
--
type DLocGraphic        = LocGraphic Double




-- | /Originated/ drawing - produce a primitive respective to the 
-- supplied start-point, access the DrawingContext as required.
--
type LocThetaGraphic u       = LocThetaImage u (UNil u)


-- | Alias of 'LocThetaGraphic' where the unit type is specialized 
-- to Double. 
--
type DLocThetaGraphic        = LocThetaGraphic Double


--------------------------------------------------------------------------------
-- Functions


-- | 'safeconcat' : @ alternative * [image] -> Image@
-- 
-- 'safeconcat' produces a composite 'Image' from a list of 
-- @Image@\'s. If the list is empty the alternative @Image@ is 
-- used.
--
-- This contrasts to 'oconcat' - when used for @Image@\'s, 
-- @oconcat@ has the same type signature as @safeconcat@ but 
-- @oconcat@ considers its arguments to be an already destructured 
-- list:
-- 
-- > oconcat (head::Image) (rest::[Image])
-- 
safeconcat :: OPlus a => Image u a -> [Image u a] -> Image u a
safeconcat _   (x:xs) = oconcat x xs
safeconcat alt []     = alt


-- | Ignore the answer produced by an 'Image', a 'LocImage' etc.
--
-- Use this function to turn an 'Image' into a 'Graphic', a 
-- 'LocImage into a 'LocGraphic'.
--
ignoreAns :: Functor f => f (a,b) -> f (UNil u, b)
ignoreAns = fmap (replaceL uNil)


-- | Replace the answer produced by an 'Image', a 'LocImage' etc.
--
replaceAns :: Functor f => z -> f (a,b) -> f (z, b)
replaceAns = fmap . replaceL


-- | Apply the supplied function to the answer produced by an 
-- 'Image', a 'LocImage' etc.
--
mapAns :: Functor f => (a -> z) -> f (a,b) -> f (z,b)
mapAns f = fmap (\(a,b) -> (f a ,b))


-- | Build an Image...
--
intoImage :: CF a -> Graphic u -> Image u a
intoImage = liftA2 (\a (_,b) -> (a,b))


-- | Build a LocImage...
--
intoLocImage :: LocCF u a -> LocGraphic u -> LocImage u a
intoLocImage = liftA2 (\a (_,b) -> (a,b))

-- | Build a LocThetaImage...
--
intoLocThetaImage :: LocThetaCF u a -> LocThetaGraphic u -> LocThetaImage u a
intoLocThetaImage = liftA2 (\a (_,b) -> (a,b))



-- | 'emptyLocGraphic' : @ LocGraphic @
--
-- Build an empty LocGraphic (i.e. a function 
-- /from Point to Graphic/). This is a path with a start point 
-- but no path segments. 
-- 
-- The 'emptyLocGraphic' is treated as a /null primitive/ by 
-- @Wumpus-Core@ and is not drawn, although it does generate a 
-- minimum bounding box at the implicit start point.
-- 
emptyLocGraphic :: Num u => LocGraphic u
emptyLocGraphic = promoteR1 $ \pt -> 
                    return $ (uNil, primGraphic $ zostroke $ emptyPath pt)




-- | Decorate an Image by superimposing a Graphic.
--
-- Note - this function has a very general type signature and
-- supports various graphic types:
--
-- > decorate :: Image u a -> Graphic u -> Image u a
-- > decorate :: LocImage u a -> LocGraphic u -> LocImage u a
-- > decorate :: LocThetaImage u a -> LocThetaGraphic u -> LocTheteImage u a
--
decorate :: Monad m 
         => m (ImageAns u a) -> m (ImageAns u zz) -> m (ImageAns u a) 
decorate img gf = 
    img >>= \(a,g1) -> gf >>= \(_,g2) -> return (a, g1 `oplus` g2)



-- | /Anterior decorate/ - decorate an Image by superimposing it 
-- on a Graphic.
--
-- Note - here the Graphic has access to the result produced by the 
-- the Image unlike 'decorate'.
--
-- Again, this function has a very general type signature and
-- supports various graphic types:
--
-- > adecorate :: Image u a -> Graphic u -> Image u a
-- > adecorate :: LocImage u a -> LocGraphic u -> LocImage u a
-- > adecorate :: LocThetaImage u a -> LocThetaGraphic u -> LocTheteImage u a
--
adecorate :: Monad m 
          => m (ImageAns u a) -> (a -> m (ImageAns u zz)) -> m (ImageAns u a)
adecorate img f = 
    img >>= \(a,g1) -> f a >>= \(_,g0) -> return (a, g0 `oplus` g1)


-- | /Superior decorate/ - decorate an image by superimposing a 
-- graphic on top of it.
--
-- Note, here the Graphic has access to the result produced by the 
-- the Image unlike 'decorate'.
--
-- Again, this function has a very general type signature and
-- supports various graphic types:
--
-- > sdecorate :: Image u a -> Graphic u -> Image u a
-- > sdecorate :: LocImage u a -> LocGraphic u -> LocImage u a
-- > sdecorate :: LocThetaImage u a -> LocThetaGraphic u -> LocTheteImage u a
--
sdecorate :: Monad m 
          => m (ImageAns u a) -> (a -> m (ImageAns u zz)) -> m (ImageAns u a)
sdecorate img f = 
    img >>= \(a,g1) -> f a >>= \(_,g2) -> return (a, g1 `oplus` g2)


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
hyperlink :: Functor m => XLink -> m (ImageAns u a) -> m (ImageAns u a)
hyperlink hypl = 
    fmap (\(a,prim) -> (a, metamorphPrim (xlink hypl) prim))

