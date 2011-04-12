{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Kernel.Objects.LocImage
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  highly unstable
-- Portability :  GHC 
--
-- LocImage and LocGraphic types - these are functional types from the 
-- DrawingContext and start point to a graphic /primitive/.
--
--------------------------------------------------------------------------------

module Wumpus.Basic.Kernel.Objects.LocImage
   (
     LocGraphic
   , LocImage

   , DLocImage
   , DLocGraphic

   , intoLocImage
   , locGraphic_

   , emptyLocGraphic

   , uconvLocImageF
   , uconvLocImageZ

   -- * /Distribution/
   , distrib
   , distribH 
   , distribV

   )

   where

import Wumpus.Basic.Kernel.Base.BaseDefs
import Wumpus.Basic.Kernel.Base.ContextFun
import Wumpus.Basic.Kernel.Base.QueryDC
import Wumpus.Basic.Kernel.Base.WrappedPrimitive
import Wumpus.Basic.Kernel.Objects.Basis


import Wumpus.Core                              -- package: wumpus-core

import Data.AffineSpace                         -- package: vector-space

import Control.Applicative
import Data.Monoid


-- | Graphic - function from DrawingContext and start point to a 
-- polymorphic /answer/ and a graphic /primitive/.
--
-- The answer is expected to be a Functor.
--
type LocImage u a       = LocQuery u (ImageAns u a)




-- | LocGraphic - function from DrawingContext and start point to 
-- a graphic /primitive/.
--
type LocGraphic u       = LocQuery u (GraphicAns u)


-- | Type specialized version of 'LocImage'.
--
type DLocImage a        = LocImage Double a

-- | Type specialized version of 'LocGraphic'.
--
type DLocGraphic        = LocGraphic Double 






-- | 'intoLocImage' : @ loc_query * loc_graphic -> LocImage @
--
-- /Loc/ version of 'intoImage'. 
-- 
-- The 'LocImage' is built as a function from an implicit start 
-- point to the answer.
--
intoLocImage :: LocQuery u a -> LocGraphic u -> LocImage u a
intoLocImage ma gf = promoteR1 $ \pt -> 
                     replaceAns <$> apply1R1 ma pt <*> apply1R1 gf pt


-- | /Downcast/ an 'LocImage' to a 'LocGraphic'.
-- 
-- This means forgetting the answer of the LocImage, replacing it 
-- with @()@.
--
locGraphic_ :: LocImage u a -> LocGraphic u
locGraphic_ = (fmap . fmap) ignoreAns


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
emptyLocGraphic :: InterpretUnit u => LocGraphic u
emptyLocGraphic = promoteR1 $ \pt -> 
                  uconvertCtxF pt >>= \dpt -> 
                  return $ graphicAns $ prim1 $ zostroke $ emptyPrimPath dpt





-- | Use this to convert 'LocGraphic' or 'LocImage' with Functor 
-- answer.
--
uconvLocImageF :: (InterpretUnit u, InterpretUnit u1, Functor t) 
               => LocImage u (t u) -> LocImage u1 (t u1)
uconvLocImageF = uconvR1 szconvAnsF



-- | Use this to convert 'LocImage' with unit-less answer.
--
uconvLocImageZ :: (InterpretUnit u, InterpretUnit u1) 
               => LocImage u a -> LocImage u1 a
uconvLocImageZ = uconvR1 szconvAnsZ



--------------------------------------------------------------------------------
-- Combining LocImages 

-- LocImages have no concept of /border/ or /next/, so they can 
-- only be combined by manipulating the start point of successive
-- drawings.

-- 'oplus' gives super-imposition - Locimages are drawn at the same
-- start point.



distrib :: (Monoid a, InterpretUnit u) 
        => Vec2 u -> [LocImage u a]  -> LocImage u a
distrib _  [] = pushR1 (replaceAns mempty) $ emptyLocGraphic
distrib v1 (x:xs) = promoteR1 $ \pt -> 
    go (x `at` pt) (pt .+^ v1) xs
  where
    go acc _  []     = acc
    go acc pt (a:as) = go (acc `mappend` apply1R1 a pt) (pt .+^ v1) as

distribH :: (Monoid a, InterpretUnit u) 
         => u -> [LocImage u a]  -> LocImage u a
distribH dx = distrib (hvec dx)

distribV :: (Monoid a, InterpretUnit u) 
         => u -> [LocImage u a]  -> LocImage u a
distribV dy = distrib (hvec dy)


