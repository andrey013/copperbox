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

   , LocQuery

   , runLocImage
   , runLocQuery

   , promoteLoc
   , applyLoc
   , qpromoteLoc
   , qapplyLoc
   , zapLocQuery
   , extrLoc

   , emptyLocImage

   , moveStart
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
          getLocImage :: DPoint2 -> Image u a }

type instance DUnit (LocImage u a) = u

type LocGraphic u = LocImage u (UNil u)


-- | Type specialized version of 'LocImage'.
--
type DLocImage a        = LocImage Double a

-- | Type specialized version of 'LocGraphic'.
--
type DLocGraphic        = LocGraphic Double 

newtype LocQuery u a = LocQuery { 
          getLocQuery :: Point2 u -> Query u a }

-- Functor

instance Functor (LocImage u) where
  fmap f ma = LocImage $ \pt -> fmap f $ getLocImage ma pt


instance Functor (LocQuery u) where
  fmap f ma = LocQuery $ \pt -> fmap f $ getLocQuery ma pt

-- Applicative

instance Applicative (LocImage u) where
  pure a    = LocImage $ \_  -> pure a
  mf <*> ma = LocImage $ \pt -> getLocImage mf pt <*> getLocImage ma pt

instance Applicative (LocQuery u) where
  pure a    = LocQuery $ \_  -> pure a
  mf <*> ma = LocQuery $ \pt -> getLocQuery mf pt <*> getLocQuery ma pt
                                


-- Monad

instance Monad (LocImage u) where
  return a  = LocImage $ \_  -> return a
  ma >>= k  = LocImage $ \pt -> getLocImage ma pt >>= \ans -> 
                                  getLocImage (k ans) pt


instance Monad (LocQuery u) where
  return a  = LocQuery $ \_  -> return a
  ma >>= k  = LocQuery $ \pt -> getLocQuery ma pt >>= \ans -> 
                                  getLocQuery (k ans) pt


-- Monoid

instance Monoid a => Monoid (LocImage u a) where
  mempty          = pure mempty
  ma `mappend` mb = LocImage $ \pt -> 
                      getLocImage ma pt `mappend` getLocImage mb pt 

instance Monoid a => Monoid (LocQuery u a) where
  mempty          = pure mempty
  ma `mappend` mb = LocQuery $ \pt -> 
                      getLocQuery ma pt `mappend` getLocQuery mb pt 

-- DrawingCtxM

instance DrawingCtxM (LocImage u) where
  askDC           = LocImage $ \_  -> askDC
  asksDC fn       = LocImage $ \_  -> asksDC fn
  localize upd ma = LocImage $ \pt -> localize upd (getLocImage ma pt)

instance DrawingCtxM (LocQuery u) where
  askDC           = LocQuery $ \_  -> askDC
  asksDC fn       = LocQuery $ \_  -> asksDC fn
  localize upd ma = LocQuery $ \pt -> localize upd (getLocQuery ma pt)



  
instance Decorate LocImage where
  decorate zo ma mz = LocImage $ \pt -> 
    decorate zo (getLocImage ma pt) (getLocImage mz pt)

  elaborate zo ma f = LocImage $ \pt -> 
    elaborate zo (getLocImage ma pt) (\a -> getLocImage (f a) pt)

  obliterate ma = LocImage $ \pt -> obliterate $ getLocImage ma pt 

  hyperlink xl ma = LocImage $ \pt -> hyperlink xl $ getLocImage ma pt 


runLocImage :: InterpretUnit u 
            => LocImage u a -> DrawingContext -> Point2 u -> PrimResult u a
runLocImage ma ctx pt = 
    let dpt = normalizeF (dc_font_size ctx) pt 
    in runImage (getLocImage ma dpt) ctx

runLocQuery :: LocQuery u a -> DrawingContext -> Point2 u -> a
runLocQuery ma ctx pt = runQuery (getLocQuery ma pt) ctx



promoteLoc ::  InterpretUnit u => (Point2 u -> Image u a) -> LocImage u a
promoteLoc k = LocImage $ \pt -> dinterpCtxF pt >>= \upt -> k upt

applyLoc :: InterpretUnit u => LocImage u a -> Point2 u -> Image u a
applyLoc mq pt = zapQuery (normalizeCtxF pt) >>= \dpt -> getLocImage mq dpt


qpromoteLoc :: (Point2 u -> Query u a) -> LocQuery u a
qpromoteLoc k = LocQuery $ \pt -> k pt

qapplyLoc :: LocQuery u a -> Point2 u -> Query u a
qapplyLoc mq pt = getLocQuery mq pt


-- | Design Note - the set of combinators to shift between Images 
-- and Queries needs sorting out - @zapLocQuery@ probably has the 
-- wrong type signature - show be LocQuery to LocImage.
--

extrLoc :: InterpretUnit u => LocImage u a -> LocQuery u a
extrLoc ma = LocQuery $ \pt ->
     askDC >>= \ctx -> 
     let (a,_) = runLocImage ma ctx pt
     in return a
     

-- qapplyLoc :: LocQuery u a -> Point2 u -> Query u a
-- qapplyLoc mq pt = getLocQuery mq pt

-- | \"zero-apply\" a LocQuery.
--
zapLocQuery :: LocQuery u a -> Point2 u -> Image u a
zapLocQuery ma pt = askDC >>= \ctx -> 
                    let a = runLocQuery ma ctx pt in return a



-- Maybe there is need for a function line qapplyLoc of this type:
--
-- > blankLoc :: LocQuery u a -> Point2 u -> Image u a
-- 
-- This then means we can have monadic bind back for the notation:
--
-- > qapplyLocTheta (rellipsePath rx ry) pt ang  `bindQ` dcClosedPath style
-- 
-- becomes
--
-- > blankLoc (rellipsePath rx ry) pt ang >>= dcClosedPath style


--------------------------------------------------------------------------------
-- Affine instances

instance (Real u, Floating u, InterpretUnit u, Rotate a) => 
    Rotate (LocImage u a) where
  rotate ang ma = promoteLoc $ \pt -> 
                     zapQuery (normalizeCtxF pt) >>= \dpt ->  
                     fmap (rotate ang) $ getLocImage ma (rotate ang dpt)


instance (Real u, Floating u, InterpretUnit u, RotateAbout a, u ~ DUnit a) => 
    RotateAbout (LocImage u a) where
  rotateAbout ang pt ma = promoteLoc $ \p0 ->
                            zapQuery (normalizeCtxF p0) >>= \dp0 ->  
                            zapQuery (normalizeCtxF pt) >>= \dpt ->  
                            fmap (rotateAbout ang pt) $ 
                              getLocImage ma (rotateAbout ang dpt dp0)


instance (Fractional u, InterpretUnit u, Scale a) => Scale (LocImage u a) where
  scale sx sy ma = promoteLoc $ \pt -> 
                     zapQuery (normalizeCtxF pt) >>= \dpt -> 
                     fmap (scale sx sy) $ getLocImage ma (scale sx sy dpt)

instance (InterpretUnit u, Translate a, ScalarUnit u, u ~ DUnit a) => 
    Translate (LocImage u a) where
  translate dx dy ma = promoteLoc $ \pt -> 
                         zapQuery (normalizeCtxF pt) >>= \dpt -> 
                         zapQuery (normalizeCtx dx)  >>= \ddx ->
                         zapQuery (normalizeCtx dy)  >>= \ddy ->
                         fmap (translate dx dy) $ 
                             getLocImage ma (translate ddx ddy dpt)

--------------------------------------------------------------------------------


instance UConvert LocImage where
  uconvF = uconvLocImageF
  uconvZ = uconvLocImageZ

-- | Use this to convert 'LocGraphic' or 'LocImage' with Functor 
-- answer.
--
uconvLocImageF :: (InterpretUnit u, InterpretUnit u1, Functor t) 
               => LocImage u (t u) -> LocImage u1 (t u1)
uconvLocImageF ma = LocImage $ \pt -> uconvF $ getLocImage ma pt



-- | Use this to convert 'LocImage' with unit-less answer.
--
uconvLocImageZ :: (InterpretUnit u, InterpretUnit u1) 
               => LocImage u a -> LocImage u1 a
uconvLocImageZ ma = LocImage $ \pt -> uconvZ $ getLocImage ma pt


-- | Having /empty/ at the specific 'LocImage' type is useful.
-- 
emptyLocImage :: Monoid a => LocImage u a
emptyLocImage = mempty




-- Note - maybe this should just be an operator on LocImage...
--

moveStart :: InterpretUnit u => Vec2 u -> LocImage u a -> LocImage u a
moveStart v1 ma = LocImage $ \pt -> 
    zapQuery (normalizeCtxF v1) >>= \dv -> getLocImage ma (pt .+^ dv) 



infixr 1 `at`


-- | Downcast a 'LocImage' function by applying it to the supplied 
-- point, making an 'Image'. 
-- 
at :: InterpretUnit u => LocImage u a -> Point2 u -> Image u a
at mf pt = zapQuery (normalizeCtxF pt) >>= \dpt -> getLocImage mf dpt


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
distrib v1 (x:xs) = promoteLoc $ \pt -> 
    go (applyLoc x pt) (pt .+^ v1) xs
  where
    go acc _  []     = acc
    go acc pt (a:as) = go (acc `mappend` applyLoc a pt) (pt .+^ v1) as

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

