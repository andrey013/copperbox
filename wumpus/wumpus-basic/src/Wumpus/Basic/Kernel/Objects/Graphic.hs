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
  , ignoreAns
  , replaceAns
  , mapAns

  , intoImage
  , intoLocImage
  , intoLocThetaImage

  , emptyLocGraphic
  , emptyLocThetaGraphic 

  , decorate
  , annotate
  
  , hyperlink

  ) where

import Wumpus.Basic.Kernel.Base.BaseDefs
import Wumpus.Basic.Kernel.Base.ContextFun
import Wumpus.Basic.Kernel.Base.QueryDC
import Wumpus.Basic.Kernel.Objects.BaseObjects

import Wumpus.Core                              -- package: wumpus-core


import Control.Applicative

--------------------------------------------------------------------------------
-- Graphic

-- | Simple drawing - produce a primitive, access the DrawingContext
-- as required, e.g for fill colour, stroke colur, line width, etc.
--
type Graphic u          = Image (Const ()) u

-- | Alias of 'Graphic' where the unit type is specialized to 
-- Double. 
--
type DGraphic           = Graphic Double


-- | /Originated/ drawing - produce a primitive respective to the 
-- supplied start-point, access the DrawingContext as required.
--
type LocGraphic u       = LocImage (Const ()) u

-- | Alias of 'LocGraphic' where the unit type is specialized to 
-- Double. 
--
type DLocGraphic        = LocGraphic Double




-- | /Originated/ drawing - produce a primitive respective to the 
-- supplied start-point, access the DrawingContext as required.
--
type LocThetaGraphic u       = LocThetaImage (Const ()) u


-- | Alias of 'LocThetaGraphic' where the unit type is specialized 
-- to Double. 
--
type DLocThetaGraphic        = LocThetaGraphic Double


--------------------------------------------------------------------------------
-- Functions



-- | Ignore the answer produced by an 'Image', a 'LocImage' etc.
--
-- Use this function to turn an 'Image' into a 'Graphic', a 
-- 'LocImage into a 'LocGraphic'.
--
ignoreAns :: Functor f => f (ImageAns t u) -> f (ImageAns (Const ()) u)
ignoreAns = fmap (bimapImageAns (const noAns) id)


-- | Replace the answer produced by an 'Image', a 'LocImage' etc.
--
replaceAns :: Functor f => (t1 u1) -> f (ImageAns t u) -> f (ImageAns t1 u1)
replaceAns a = fmap (bimapImageAns (const a) id)


-- | Apply the supplied function to the answer produced by an 
-- 'Image', a 'LocImage' etc.
--
mapAns :: Functor f => (t u -> t1 u1) -> f (ImageAns t u) -> f (ImageAns t1 u1)
mapAns f = fmap (bimapImageAns f id)


-- | 'intoImage' : @ context_function * graphic -> Image @
--
-- Build an 'Image' from a context function ('CF') that generates 
-- the answer and a 'Graphic' that draws the 'Image'.
--
intoImage :: CF (t u) -> Graphic u -> Image t u
intoImage = liftA2 (\a ans -> bimapImageAns (const a) id ans)


-- | 'intoLocImage' : @ loc_context_function * loc_graphic -> LocImage @
--
-- /Loc/ version of 'intoImage'. 
-- 
-- The 'LocImage' is built as a function from an implicit start 
-- point to the answer.
--
intoLocImage :: LocCF u (t u) -> LocGraphic u -> LocImage t u
intoLocImage = liftA2 (\a ans -> bimapImageAns (const a) id ans)

-- | 'intoLocThetaImage' : @ loc_theta_cf * loc_theta_graphic -> LocThetaImage @
--
-- /LocTheta/ version of 'intoImage'. 
-- 
-- The 'LocThetaImage' is built as a function from an implicit 
-- start point and angle of inclination to the answer.
--
intoLocThetaImage :: LocThetaCF u (t u) -> LocThetaGraphic u -> LocThetaImage t u
intoLocThetaImage = liftA2 (\a ans -> bimapImageAns (const a) id ans)



-- | 'emptyLocGraphic' : @ LocGraphic @
--
-- Build an empty 'LocGraphic' (i.e. a function 
-- /from Point to Graphic/). This is a path with a start point 
-- but no path segments. 
-- 
-- The 'emptyLocGraphic' is treated as a /null primitive/ by 
-- @Wumpus-Core@ and is not drawn, although it does generate a 
-- minimum bounding box at the implicit start point.
-- 
emptyLocGraphic :: CxSize u => LocGraphic u
emptyLocGraphic = promoteR1 $ \pt -> 
                    ctxSizeF pt >>= \pt1 -> 
                    return $ imageAns noAns (zostroke $ emptyPath pt1)



-- | 'emptyLocThetaGraphic' : @ LocThetaGraphic @
--
-- Build an empty 'LocThetaGraphic' (i.e. a function 
-- /from Point and Inclination to Graphic/). 
-- 
-- The 'emptyLocThetaGraphic' is treated as a /null primitive/ by 
-- @Wumpus-Core@ and is not drawn, although it does generate a 
-- minimum bounding box at the implicit start point.
-- 
emptyLocThetaGraphic :: CxSize u => LocThetaGraphic u
emptyLocThetaGraphic = lift1R2 emptyLocGraphic




-- | 'decorate' : @ image * graphic -> Image @
-- 
-- Decorate an Image by super-imposing a Graphic.
--
-- Note - this function has a very general type signature and
-- supports various graphic types:
--
-- > decorate :: Image t u -> Graphic u -> Image t u
-- > decorate :: LocImage t u -> LocGraphic u -> LocImage t u
-- > decorate :: LocThetaImage t u -> LocThetaGraphic u -> LocTheteImage t u
--
decorate :: Monad m 
         => m (ImageAns t u) -> m (ImageAns t0 u0) -> m (ImageAns t u) 
decorate img gf = 
    img >>= \a1 -> gf >>= \a2 -> 
    return $ imageAns (answer a1) (imageOutput a1 `oplus` imageOutput a2)


-- | 'annotate' : @ image * (result -> graphic) -> Image @
-- 
-- | Annotate an image by super-imposing a graphic on top of it - 
-- the annotation function has access to the /result/ of the Image
-- before it is super-imposed.
--
-- Again, this function has a very general type signature and
-- supports various graphic types:
--
-- > annotate :: Image t u -> ((t u) -> Graphic u) -> Image t u
-- > annotate :: LocImage t u -> ((t u) -> LocGraphic u) -> LocImage t u
-- > annotate :: LocThetaImage t u -> ((t u) -> LocThetaGraphic u) -> LocTheteImage t u
--
annotate :: Monad m 
         => m (ImageAns t u) -> ((t u) -> m (ImageAns t0 u0)) -> m (ImageAns t u)
annotate img f = 
    img >>= \a1 -> f (answer a1) >>= \a2 -> 
    return $ imageAns (answer a1) (imageOutput a1 `oplus` imageOutput a2)




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
hyperlink :: Functor m => XLink -> m (ImageAns t u) -> m (ImageAns t u)
hyperlink hypl = fmap (bimapImageAns id (xlink hypl))

