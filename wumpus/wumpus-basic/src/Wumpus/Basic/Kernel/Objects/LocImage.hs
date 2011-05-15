{-# LANGUAGE TypeFamilies               #-}
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

     LocImage
   , LocGraphic

   , DLocImage
   , DLocGraphic

   , runLocImage

   , uconvLocImageF
   , uconvLocImageZ

   , emptyLocImage

   , at

   -- * Composing LocImages
   , distrib
   , distribH 
   , distribV
   
   , duplicate
   , duplicateH
   , duplicateV


   )

   where

import Wumpus.Basic.Kernel.Base.BaseDefs
import Wumpus.Basic.Kernel.Base.DrawingContext
import Wumpus.Basic.Kernel.Base.QueryDC
import Wumpus.Basic.Kernel.Objects.Basis


import Wumpus.Core                              -- package: wumpus-core

import Data.AffineSpace                         -- package: vector-space
import Data.VectorSpace

import Control.Applicative
import Data.Monoid



-- | 'LocThetaImage' - function from  start point and 
-- DrawingContext to a polymorphic /answer/ and a graphic 
-- /primitive/ (PrimW).
--
newtype LocImage u a = LocImage { 
          getLocImage :: Point2 u -> Image u a }

type instance DUnit (LocImage u a) = u
type instance Answer (LocImage u a) = a

type LocGraphic u = LocImage u (UNil u)


-- | Type specialized version of 'LocImage'.
--
type DLocImage a        = LocImage Double a

-- | Type specialized version of 'LocGraphic'.
--
type DLocGraphic        = LocGraphic Double 



instance Functor (LocImage u) where
  fmap f ma = LocImage $ \pt -> fmap f $ getLocImage ma pt

instance Applicative (LocImage u) where
  pure a    = LocImage $ \_  -> pure a
  mf <*> ma = LocImage $ \pt -> getLocImage mf pt <*> getLocImage ma pt


instance Monad (LocImage u) where
  return a  = LocImage $ \_  -> return a
  ma >>= k  = LocImage $ \pt -> getLocImage ma pt >>= \ans -> 
                                    getLocImage (k ans) pt


instance Monoid a => Monoid (LocImage u a) where
  mempty          = pure mempty
  ma `mappend` mb = LocImage $ \pt -> 
                      getLocImage ma pt `mappend` getLocImage mb pt 



instance DrawingCtxM (LocImage u) where
  askDC           = LocImage $ \_  -> askDC
  asksDC fn       = LocImage $ \_  -> asksDC fn
  localize upd ma = LocImage $ \pt -> localize upd (getLocImage ma pt)


instance Num u => MoveStart (LocImage u a) where
  moveStart v1 ma = LocImage $ \pt -> getLocImage ma (pt .+^ v1) 


instance UnaryObj (LocImage u a) where
  type UnaryR (LocImage u a) = Point2 u 
  
  promoteU fn = LocImage $ \pt -> fn pt
  applyU mf pt = getLocImage mf pt
  

runLocImage :: Point2 u -> DrawingContext -> LocImage u a -> PrimW u a
runLocImage pt ctx mf = runImage ctx (getLocImage mf pt)


--------------------------------------------------------------------------------
-- Affine instances

instance (Real u, Floating u, Rotate a) => Rotate (LocImage u a) where
  rotate ang ma = promoteU $ \pt -> 
                     fmap (rotate ang) $ getLocImage ma (rotate ang pt)


instance (Real u, Floating u, RotateAbout a, ScalarUnit u, u ~ DUnit a) => 
    RotateAbout (LocImage u a) where
  rotateAbout ang pt ma = promoteU $ \p0 -> 
                            fmap (rotateAbout ang pt) $ 
                              getLocImage ma (rotateAbout ang pt p0)


instance (Fractional u, Scale a) => Scale (LocImage u a) where
  scale sx sy ma = promoteU $ \pt -> 
                   fmap (scale sx sy) $ getLocImage ma (scale sx sy pt)

instance (Num u, Translate a, ScalarUnit u, u ~ DUnit a) => 
    Translate (LocImage u a) where
  translate dx dy ma = promoteU $ \pt -> 
                         fmap (translate dx dy) $ 
                           getLocImage ma (translate dx dy pt)

--------------------------------------------------------------------------------

-- | Use this to convert 'LocGraphic' or 'LocImage' with Functor 
-- answer.
--
uconvLocImageF :: (InterpretUnit u, InterpretUnit u1, Functor t) 
               => LocImage u (t u) -> LocImage u1 (t u1)
uconvLocImageF ma = LocImage $ \pt -> 
    getFontSize >>= \sz -> 
    let ptu = uconvertF sz pt
    in uconvImageF $ getLocImage ma ptu



-- | Use this to convert 'LocImage' with unit-less answer.
--
uconvLocImageZ :: (InterpretUnit u, InterpretUnit u1) 
               => LocImage u a -> LocImage u1 a
uconvLocImageZ ma = LocImage $ \pt -> 
    getFontSize >>= \sz ->  
    let ptu = uconvertF sz pt
    in uconvImageZ $ getLocImage ma ptu


-- | Having /empty/ at the specific 'LocImage' type is useful.
-- 
emptyLocImage :: Monoid a => LocImage u a
emptyLocImage = mempty




infixr 1 `at`


-- | Downcast a 'LocCF' function by applying it to the supplied 
-- point, making an arity-zero Context Function. 
-- 
-- Remember a 'LocCF' function is a 'CF1' context function where
-- the /static argument/ is specialized to a start point.
--
at :: LocImage u a -> Point2 u -> Image u a
at mf pt = getLocImage mf pt


--------------------------------------------------------------------------------
-- Combining LocImages 

-- LocImages have no concept of /border/ or /next/, so they can 
-- only be combined by manipulating the start point of successive
-- drawings.

-- 'oplus' gives super-imposition - Locimages are drawn at the same
-- start point.



distrib :: (Monoid a, InterpretUnit u) 
        => Vec2 u -> [LocImage u a]  -> LocImage u a
distrib _  []     = mempty
distrib v1 (x:xs) = promoteU $ \pt -> 
    go (applyU x pt) (pt .+^ v1) xs
  where
    go acc _  []     = acc
    go acc pt (a:as) = go (acc `mappend` applyU a pt) (pt .+^ v1) as

distribH :: (Monoid a, InterpretUnit u) 
         => u -> [LocImage u a]  -> LocImage u a
distribH dx = distrib (hvec dx)

distribV :: (Monoid a, InterpretUnit u) 
         => u -> [LocImage u a]  -> LocImage u a
distribV dy = distrib (hvec dy)


-- | This is analogue to @replicate@ in the Prelude.
--
duplicate :: (Monoid a, InterpretUnit u) 
          => Int -> Vec2 u -> LocImage u a -> LocImage u a
duplicate n _ _   | n < 1 = mempty
duplicate n v img         = go img v (n-1)
  where
     go acc _  i | i < 1 = acc
     go acc v1 i         = let img1 = moveStart v1 img
                           in go (acc `mappend` img1) (v1 ^+^ v) (i-1)

duplicateH :: (Monoid a, InterpretUnit u) 
           => Int -> u -> LocImage u a -> LocImage u a
duplicateH n dx = duplicate n (hvec dx)

duplicateV :: (Monoid a, InterpretUnit u) 
           => Int -> u -> LocImage u a -> LocImage u a
duplicateV n dy = duplicate n (vvec dy)

