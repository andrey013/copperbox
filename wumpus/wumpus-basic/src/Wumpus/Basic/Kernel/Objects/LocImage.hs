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
   , emptyLocGraphic
   , uconvertLocImg

   -- * Combining LocImages
   , catLI
   , sepLI
   , hsepLI
   , vsepLI
   , repeatLI
   , hrepeatLI
   , vrepeatLI
   , spaceLI
   , hspaceLI
   , vspaceLI
   , encloseLI
   , hencloseLI
   , vencloseLI
   , punctuateLI
   , hpunctuateLI
   , vpunctuateLI

   )

   where

import Wumpus.Basic.Kernel.Base.BaseDefs
import Wumpus.Basic.Kernel.Base.ContextFun
import Wumpus.Basic.Kernel.Base.QueryDC
import Wumpus.Basic.Kernel.Base.WrappedPrimitive
import Wumpus.Basic.Kernel.Objects.Basis
import Wumpus.Basic.Kernel.Objects.Displacement


import Wumpus.Core                              -- package: wumpus-core

import Control.Applicative

-- | Graphic - function from DrawingContext and start point to a 
-- polymorphic /answer/ and a graphic /primitive/.
--
-- The answer is expected to be a Functor.
--
type LocImage t u = LocQuery u (ImageAns t u)




-- | LocGraphic - function from DrawingContext and start point to 
-- a graphic /primitive/.
--
type LocGraphic u = LocImage UNil u


-- | Type specialized version of 'LocImage'.
--
type DLocImage t    = LocImage t Double

-- | Type specialized version of 'LocGraphic'.
--
type DLocGraphic    = LocGraphic Double 






-- | 'intoLocImage' : @ loc_query * loc_graphic -> LocImage @
--
-- /Loc/ version of 'intoImage'. 
-- 
-- The 'LocImage' is built as a function from an implicit start 
-- point to the answer.
--
intoLocImage :: LocQuery u (t u) -> LocGraphic u -> LocImage t u
intoLocImage = liftA2 (\a (Ans _ p) -> Ans a p)




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
                  return $ Ans UNil (prim1 $ zostroke $ emptyPrimPath dpt)


-- | Use this to convert both 'LocImage' and 'LocGraphic'.
--
uconvertLocImg :: (InterpretUnit u, InterpretUnit u1, Functor t) 
               => LocImage t u -> LocImage t u1
uconvertLocImg = uconvertR1a



--------------------------------------------------------------------------------
-- Combining LocImages

infixr 6 `catLI`
infixr 5 `sepLI`


-- | Concatenate two LocImages. The start point is /shared/.
--
-- This is just @oplus@.
--
catLI :: OPlus (t u) 
      => LocImage t u -> LocImage t u -> LocImage t u
catLI = oplus


-- | Concatenate two LocImages, the second LocImage is displaced
-- from the the start point by the supplied vector (separator).
--
-- Note - the separator is exactly a displacement of the start
-- point, LocImages have no notion of border so this function
-- can only be used to concatenate to objects side by side if
-- there boundaries are known beforehand.
-- 
-- Consider a more capable object such as a PosImage or AdvGraphic
-- if you need more sophisticated arrangement.
-- 
sepLI :: (Num u, OPlus (t u))
      => Vec2 u -> LocImage t u -> LocImage t u -> LocImage t u
sepLI v g1 g2 = g1 `oplus` moveStart (displaceVec v) g2



-- | Concatenate two LocImages, the second LocImage is displaced
-- horizontally from the start point by the supplied distance.
--
-- Note - this is exactly a start point displacement. See the 
-- caveat for 'sepLI'.
-- 
hsepLI :: (Num u, OPlus (t u))
       => u -> LocImage t u -> LocImage t u -> LocImage t u
hsepLI u = sepLI (hvec u)



-- | Concatenate two LocImages, the second LocImage is displaced
-- vertically from the start point by the supplied distance.
--
-- Note - this is exactly a start point displacement. See the 
-- caveat for 'sepLI'.
-- 
vsepLI :: (Num u, OPlus (t u))
       => u -> LocImage t u -> LocImage t u -> LocImage t u
vsepLI u = sepLI (vvec u)


-- | Repeatedly draw a LocImage, moving the start point each time 
-- by the supplied vector.
--
-- Note - the first LocImage argument is the /empty/ alternative
-- this is drawn if the repeat count is less than 1.
--
repeatLI :: (Num u, OPlus (t u))
         => LocImage t u -> Int -> Vec2 u -> LocImage t u -> LocImage t u
repeatLI alt i _  _  | i < 1 = alt
repeatLI _   i v  gf         = promoteR1 $ \start -> 
    go (i-1) (gf `at` start) (mv start)
  where
    mv                      = displaceVec v
    go n acc pt | n < 1     = acc
                | otherwise = go (n-1) (acc `oplus` (gf `at` pt)) (mv pt)



-- | Repeatedly draw a LocImage, moving the start point 
-- horizontally each time by the supplied distance.
--
-- Note - this draws the alternative LocImage if the repeat count 
-- is less than 1.
--
hrepeatLI :: (Num u, OPlus (t u))
          => LocImage t u -> Int -> u -> LocImage t u -> LocImage t u
hrepeatLI alt i u = repeatLI alt i (hvec u) 



-- | Repeatedly draw a LocImage, moving vertically each time by 
-- the supplied distance.
--
-- Note - this draws the alternative LocImage if the repeat count 
-- is less than 1.
--
vrepeatLI :: (Num u, OPlus (t u))
          => LocImage t u -> Int -> u -> LocImage t u -> LocImage t u
vrepeatLI alt i u = repeatLI alt i (vvec u) 



-- | Concatenate a list of LocImages, spacing them by moving the 
-- start point each time by the supplied vector.
--
-- Note - this draws the /empty/ alternative if the list is empty.
--
spaceLI :: (Num u, OPlus (t u))
         => LocImage t u -> Vec2 u -> [LocImage t u] -> LocImage t u
spaceLI alt _ []     = alt
spaceLI _   v (g:gs) = promoteR1 $ \start -> 
    go  (g `at` start) (mv start) gs
  where
    mv               = displaceVec v
    go acc _  []     = acc
    go acc pt (f:fs) = go (acc `oplus` (f `at` pt)) (mv pt) fs



-- | Concatenate a list of LocImages, spacing them by moving the 
-- start point horizontally each time by the supplied distance.
--
-- Note - this draws the /empty/ alternative if the list is empty.
--
hspaceLI :: (Num u, OPlus (t u))
          => LocImage t u -> u -> [LocImage t u] -> LocImage t u
hspaceLI alt u = spaceLI alt (hvec u)


-- | Concatenate a list of LocImages, spacing them by moving the 
-- start point vertically each time by the supplied distance.
--
-- Note - this draws the /empty/ alternative if the list is empty.
--
vspaceLI :: (Num u, OPlus (t u))
          => LocImage t u -> u -> [LocImage t u] -> LocImage t u
vspaceLI alt u = spaceLI alt (vvec u)


-- | Enclose l r x
--
-- Note - the @left@ LocImage is drawn at the start point, the 
-- LocImage @x@ is concatenated with 'sepLI' then the right 
-- LocImage is concatenated with 'sepLi'.
--
encloseLI :: (Num u, OPlus (t u))
          => Vec2 u -> LocImage t u -> LocImage t u -> LocImage t u 
          -> LocImage t u
encloseLI v lft rht obj = lft `op` obj `op` rht
  where
    op = sepLI v



-- | Horizontal version of 'encloseLI'.
--
-- Note - the @left@ LocImage is drawn at the start point, the 
-- LocImage @x@ is concatenated with 'sepLI' then the right 
-- LocImage is concatenated with 'sepLI'.
--
hencloseLI :: (Num u, OPlus (t u))
           => u -> LocImage t u -> LocImage t u -> LocImage t u 
           -> LocImage t u
hencloseLI u = encloseLI (hvec u)


-- | Vertical version of 'encloseLI'.
--
-- Note - the @left@ LocImage is drawn at the start point, the 
-- LocImage @x@ is concatenated with 'sepLI' then the right 
-- LocImage is concatenated with 'sepLI'.
--
vencloseLI :: (Num u, OPlus (t u))
           => u -> LocImage t u -> LocImage t u -> LocImage t u 
           -> LocImage t u
vencloseLI u = encloseLI (vvec u)



-- | Concatenate a list of LocImages, punctuating with the 
-- separator.
--
-- Note - this draws the /empty/ alternative if the list is empty.
--
punctuateLI :: (Num u, OPlus (t u))
            => LocImage t u -> Vec2 u -> LocImage t u -> [LocImage t u] 
            -> LocImage t u
punctuateLI alt _ _   []     = alt
punctuateLI _   v sep (g:gs) = go g gs
  where 
    go acc []     = acc
    go acc (f:fs) = go (encloseLI v acc f sep) fs


-- | Horizontal version of 'punctuateLI'
--
hpunctuateLI :: (Num u, OPlus (t u))
             => LocImage t u -> u -> LocImage t u -> [LocImage t u] 
             -> LocImage t u
hpunctuateLI alt u = punctuateLI alt (hvec u)


-- | Vertical version of 'punctuateLI'
--
vpunctuateLI :: (Num u, OPlus (t u))
             => LocImage t u -> u -> LocImage t u -> [LocImage t u] 
             -> LocImage t u
vpunctuateLI alt u = punctuateLI alt (vvec u)

