{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Kernel.Objects.LocThetaImage
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  highly unstable
-- Portability :  GHC 
--
-- LocThetaImage and LocThetaGraphic types - these are functional 
-- types from the DrawingContext, start point and angle of 
-- inclination to a graphic /primitive/.
--
--------------------------------------------------------------------------------

module Wumpus.Basic.Kernel.Objects.LocThetaImage
   (
     LocThetaGraphic
   , LocThetaImage

   , DLocThetaGraphic
   , DLocThetaImage

   , intoLocThetaImage
   , emptyLocThetaGraphic
   , uconvertLocThetaImg

   -- * Combining LocThetaImages
   , catLTI
   , sepLTI
   , paraSepLTI
   , perpSepLTI
   , repeatLTI
   , paraRepeatLTI
   , perpRepeatLTI
   , concatLTI
   , paraConcatLTI
   , perpConcatLTI
   , encloseLTI
   , paraEncloseLTI
   , perpEncloseLTI
   , punctuateLTI
   , paraPunctuateLTI
   , perpPunctuateLTI
   
   )

   where

import Wumpus.Basic.Kernel.Base.BaseDefs
import Wumpus.Basic.Kernel.Base.ContextFun
import Wumpus.Basic.Kernel.Objects.Basis
import Wumpus.Basic.Kernel.Objects.Displacement
import Wumpus.Basic.Kernel.Objects.LocImage

import Wumpus.Core                              -- package: wumpus-core

import Control.Applicative


-- | 'LocThetaImage' - function from DrawingContext, start point 
-- and inclination to a polymorphic /answer/ and a graphic 
-- /primitive/ (ImageAns).
--
-- The answer is expected to be a Functor.
--
type LocThetaImage t u  = LocThetaQuery u (ImageAns t u)


-- | LocThetaGraphic - function from DrawingContext, start point 
-- and inclination to a graphic /primitive/ (GraphicAns).
--
type LocThetaGraphic u  = LocThetaImage UNil u


-- | Type specialized version of 'LocThetaImage'.
--
type DLocThetaImage t   = LocThetaImage t Double

-- | Type specialized version of 'LocThetaGraphic'.
--
type DLocThetaGraphic   = LocThetaGraphic Double 




-- | 'intoLocThetaImage' : @ loc_theta_query * loc_theta_graphic -> LocThetaImage @
--
-- /LocTheta/ version of 'intoImage'. 
-- 
-- The 'LocThetaImage' is built as a function from an implicit 
-- start point and angle of inclination to the answer.
--
intoLocThetaImage :: LocThetaQuery u (t u) 
                  -> LocThetaGraphic u 
                  -> LocThetaImage t u
intoLocThetaImage = liftA2 (\a (Ans _ p) -> Ans a p)



-- | 'emptyLocThetaGraphic' : @ LocThetaGraphic @
--
-- Build an empty 'LocThetaGraphic' (i.e. a function 
-- /from Point and Inclination to Graphic/). 
-- 
-- The 'emptyLocThetaGraphic' is treated as a /null primitive/ by 
-- @Wumpus-Core@ and is not drawn, although it does generate a 
-- minimum bounding box at the implicit start point.
-- 
emptyLocThetaGraphic :: InterpretUnit u => LocThetaGraphic u
emptyLocThetaGraphic = lift1R2 emptyLocGraphic


-- | Use this to convert both 'LocThetaImage' and 
-- 'LocThetaGraphic'.
--
uconvertLocThetaImg :: (InterpretUnit u, InterpretUnit u1, Functor t) 
                    => LocThetaImage t u -> LocThetaImage t u1
uconvertLocThetaImg = uconvertR2a




--------------------------------------------------------------------------------
-- Combining LocThetaImages



-- | Concatenate two LocThetaImages. The start point is /shared/.
--
-- This is just @oplus@.
--
catLTI :: OPlus (t u)
       => LocThetaImage t u -> LocThetaImage t u -> LocThetaImage t u
catLTI = oplus



-- | Concatenate two LocThetaImages, the second LocThetaImage is 
-- displaced /orthonormally/ from the the start point by the 
-- supplied vector (separator). 
--
-- Here, /orthonormally/ means that the x-component of the vector
-- displaces the second LocThetaImage in parallel to the angle
-- of inclination, the y-component of the vector displaces 
-- perpendicular to the incliantion.
--
-- Note - the separator is exactly a displacement of the start
-- point, LocImages have no notion of border so this function
-- can only be used to concatenate to objects side by side if
-- there boundaries are known beforehand.
-- 
-- Consider a PosThetaImage if you need more sophisticated arrangement.
-- 
sepLTI :: (Floating u, OPlus (t u))
       => Vec2 u -> LocThetaImage t u -> LocThetaImage t u 
       -> LocThetaImage t u
sepLTI v g1 g2 = g1 `oplus` moveStartTheta (displaceOrtho v) g2



-- | Concatenate two LocThetaImages, the second LocImage is 
-- displaced parallel to the inclination by the supplied distance. 
--
-- Note - this is exactly a start point displacement. See the 
-- caveat for 'sepLTI'.
-- 
paraSepLTI :: (Floating u, OPlus (t u))
           => u -> LocThetaImage t u -> LocThetaImage t u -> LocThetaImage t u
paraSepLTI u = sepLTI (hvec u)



-- | Concatenate two LocThetaImages, the second LocThetaImage is 
-- displaced perpendicular to the inclination by the supplied 
-- distance.
--
-- Note - this is exactly a start point displacement. See the 
-- caveat for 'sepLTI'.
-- 
perpSepLTI :: (Floating u, OPlus (t u))
       => u -> LocThetaImage t u -> LocThetaImage t u -> LocThetaImage t u
perpSepLTI u = sepLTI (vvec u)


-- | Repeatedly draw a LocThetaImage, moving the start point each time 
-- /orthonormally/ by the supplied vector.
--
-- Note - the first LocThetaImage argument is the /empty/ alternative
-- this is drawn if the repeat count is less than 1.
--
repeatLTI :: (Floating u, OPlus (t u))
          => LocThetaImage t u -> Int -> Vec2 u -> LocThetaImage t u 
          -> LocThetaImage t u
repeatLTI alt i _  _  | i < 1 = alt
repeatLTI _   i v  gf         = promoteR2 $ \start ang -> body start ang
  where
    body start ang = go (i-1) (drawF start) (moveF start)
      where
        drawF pt                = atIncline gf pt ang
        moveF pt                = displaceOrtho v ang pt
        go n acc pt | n < 1     = acc
                    | otherwise = go (n-1) (acc `oplus` drawF pt) (moveF pt)



-- | Repeatedly draw a LocThetaImage, moving parallel to the 
-- inclination each time by the supplied distance.
--
-- Note - this draws the alternative LocThetaImage if the repeat count 
-- is less than 1.
--
paraRepeatLTI :: (Floating u, OPlus (t u))
              => LocThetaImage t u -> Int -> u -> LocThetaImage t u 
              -> LocThetaImage t u
paraRepeatLTI alt i u = repeatLTI alt i (hvec u) 



-- | Repeatedly draw a LocThetaImage, moving perpendicular to the
-- inclination each time by the supplied distance.
--
-- Note - this draws the alternative LocThetaImage if the repeat count 
-- is less than 1.
--
perpRepeatLTI :: (Floating u, OPlus (t u))
          => LocThetaImage t u -> Int -> u -> LocThetaImage t u -> LocThetaImage t u
perpRepeatLTI alt i u = repeatLTI alt i (vvec u) 



-- | Concatenate a list of LocThetaImages, moving the start point 
-- /orthonormally/ each time by the supplied vector.
--
-- Note - this draws the /empty/ alternative if the list is empty.
--
concatLTI :: (Floating u, OPlus (t u))
         => LocThetaImage t u -> Vec2 u -> [LocThetaImage t u] 
         -> LocThetaImage t u
concatLTI alt _ []     = alt
concatLTI _   v (g:gs) = promoteR2 $ \start ang -> body start ang 
  where
    body start ang = go (drawF g start) (moveF start) gs
      where
        drawF gf pt      = atIncline gf pt ang
        moveF pt         = displaceOrtho v ang pt
        go acc _  []     = acc
        go acc pt (f:fs) = go (acc `oplus` drawF f pt) (moveF pt) fs



-- | Concatenate a list of LocThetaImages, moving the start point 
-- parallel to the inclination each time by the supplied distance.
--
-- Note - this draws the /empty/ alternative if the list is empty.
--
paraConcatLTI :: (Floating u, OPlus (t u))
              => LocThetaImage t u -> u -> [LocThetaImage t u] 
              -> LocThetaImage t u
paraConcatLTI alt u = concatLTI alt (hvec u)


-- | Concatenate a list of LocThetaImages, moving the start point 
-- perpendicular each time by the supplied distance.
--
-- Note - this draws the /empty/ alternative if the list is empty.
--
perpConcatLTI :: (Floating u, OPlus (t u))
              => LocThetaImage t u -> u -> [LocThetaImage t u] 
              -> LocThetaImage t u
perpConcatLTI alt u = concatLTI alt (vvec u)


-- | Enclose l r x
--
-- Note - the @left@ LocThetaImage is drawn at the start point, the 
-- LocThetaImage @x@ is concatenated with 'sepLTI' then the right 
-- LocThetaImage is concatenated with 'sepLi'.
--
encloseLTI :: (Floating u, OPlus (t u))
           => Vec2 u 
           -> LocThetaImage t u -> LocThetaImage t u -> LocThetaImage t u 
           -> LocThetaImage t u
encloseLTI v lft rht obj = lft `op` obj `op` rht
  where
    op = sepLTI v



-- | Parallel version of 'encloseLTI'.
--
-- Note - the @left@ LocThetaImage is drawn at the start point, the 
-- LocThetaImage @x@ is concatenated with 'sepLTI' then the right 
-- LocThetaImage is concatenated with 'sepLTI'.
--
paraEncloseLTI :: (Floating u, OPlus (t u))
               => u 
               -> LocThetaImage t u -> LocThetaImage t u -> LocThetaImage t u 
               -> LocThetaImage t u
paraEncloseLTI u = encloseLTI (hvec u)



-- | Perpendicular version of 'encloseLTI'.
--
-- Note - the @left@ LocThetaImage is drawn at the start point, the 
-- LocThetaImage @x@ is concatenated with 'sepLTI' then the right 
-- LocThetaImage is concatenated with 'sepLi'.
--
perpEncloseLTI :: (Floating u, OPlus (t u))
               => u 
               -> LocThetaImage t u -> LocThetaImage t u -> LocThetaImage t u 
               -> LocThetaImage t u
perpEncloseLTI u = encloseLTI (vvec u)



-- | Concatenate a list of LocThetaImages, punctuating with the 
-- separator.
--
-- Note - this draws the /empty/ alternative if the list is empty.
--
punctuateLTI :: (Floating u, OPlus (t u))
             => LocThetaImage t u -> Vec2 u 
             -> LocThetaImage t u -> [LocThetaImage t u] 
             -> LocThetaImage t u
punctuateLTI alt _ _   []     = alt
punctuateLTI _   v sep (g:gs) = go g gs
  where 
    go acc []     = acc
    go acc (f:fs) = go (encloseLTI v acc f sep) fs


-- | Parallel version of 'punctuateLTI'
--
paraPunctuateLTI :: (Floating u, OPlus (t u))
                 => LocThetaImage t u -> u 
                 -> LocThetaImage t u -> [LocThetaImage t u] 
                 -> LocThetaImage t u
paraPunctuateLTI alt u = punctuateLTI alt (hvec u)


-- | Perpendicular version of 'punctuateLTI'
--
perpPunctuateLTI :: (Floating u, OPlus (t u))
                 => LocThetaImage t u -> u 
                 -> LocThetaImage t u -> [LocThetaImage t u] 
                 -> LocThetaImage t u
perpPunctuateLTI alt u = punctuateLTI alt (vvec u)

