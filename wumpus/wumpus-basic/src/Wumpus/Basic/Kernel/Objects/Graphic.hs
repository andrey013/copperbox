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
  , intoImage
  , intoLocImage
  , intoLocThetaImage



  ) where

import Wumpus.Basic.Kernel.Base.ContextFun
import Wumpus.Basic.Kernel.Base.BaseDefs
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

