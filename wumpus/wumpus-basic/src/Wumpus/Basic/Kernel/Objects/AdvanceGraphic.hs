{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Kernel.Objects.AdvanceGraphic
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  highly unstable
-- Portability :  GHC 
--
-- Extended Graphic object - an AdvanceGraphic is a Graphic 
-- twinned with and advance vector.
--
--------------------------------------------------------------------------------

module Wumpus.Basic.Kernel.Objects.AdvanceGraphic
  (

  -- * Advance-vector graphic
    AdvGraphic
  , DAdvGraphic


  , intoAdvGraphic
  , emptyAdvGraphic


  -- * Composition
  , advcat
  , advsep
  , advconcat
  , advspace
  , advpunctuate
  , advfill

  ) where

import Wumpus.Basic.Kernel.Base.BaseDefs
import Wumpus.Basic.Kernel.Base.ContextFun
import Wumpus.Basic.Kernel.Objects.BaseObjects
import Wumpus.Basic.Kernel.Objects.Graphic

import Wumpus.Core                              -- package: wumpus-core

import Data.AffineSpace                         -- package: vector-space
import Data.VectorSpace



-- | /Advance vector/ graphic - this partially models the 
-- PostScript @show@ command which moves the /current point/ by the
-- advance (width) vector as each character is drawn.
--
type AdvGraphic u      = LocImage u (Vec2 u)

type DAdvGraphic       = AdvGraphic Double



--------------------------------------------------------------------------------



--
intoAdvGraphic :: LocDrawingInfo u (Vec2 u)
               -> LocGraphic u 
               -> AdvGraphic u
intoAdvGraphic = intoLocImage


emptyAdvGraphic :: Num u => AdvGraphic u
emptyAdvGraphic = replaceAns (V2 0 0) $ emptyLocGraphic


-- This can be achieved with ignoreAns ...

-- extractLocGraphic :: AdvGraphic u -> LocGraphic u
-- extractLocGraphic = fmap (replaceL uNil)

-- runAdvGraphic :: DrawingContext  -> Point2 u -> AdvGraphic u 
--               -> (Point2 u, PrimGraphic u)
-- runAdvGraphic ctx pt df = runCF1 ctx pt df



--------------------------------------------------------------------------------
-- composition

-- Note there are opportunities for extra composition operators
-- like the /picture language/...


-- Naming convention - binary functions are favoured for shorter names.

infixr 6 `advcat`
infixr 5 `advsep`

-- | Concatenate the two AdvGraphics.
--
advcat :: Num u => AdvGraphic u -> AdvGraphic u -> AdvGraphic u
advcat af ag = promoteR1 $ \start -> 
                 (af `at` start)        >>= \(v1,prim1) -> 
                 (ag `at` start .+^ v1) >>= \(v2,prim2) -> 
                 return (v1 ^+^ v2, prim1 `oplus` prim2)


-- | Concatenate the two AdvGraphics spacing them by the supplied 
-- vector.
--
advsep :: Num u => Vec2 u -> AdvGraphic u -> AdvGraphic u -> AdvGraphic u
advsep sv af ag = promoteR1 $ \start -> 
                 (af `at` start)        >>= \(v1,prim1) -> 
                 (ag `at` start .+^ sv ^+^ v1) >>= \(v2,prim2) -> 
                 return (v1 ^+^ sv ^+^  v2, prim1 `oplus` prim2)


-- | Concatenate the list of AdvGraphic with 'advcat'.
--
advconcat :: Num u => [AdvGraphic u] -> AdvGraphic u
advconcat []     = emptyAdvGraphic
advconcat (x:xs) = step x xs
  where
    step a (b:bs) = step (a `advcat` b) bs
    step a []     = a


-- | Concatenate the list of AdvGraphic with 'advsep'.
--
advspace :: Num u => Vec2 u -> [AdvGraphic u] -> AdvGraphic u
advspace _  []     = emptyAdvGraphic
advspace sv (x:xs) = step x xs
  where
    step a (b:bs) = step (advsep sv a b) bs
    step a []     = a


-- | Concatenate the list of AdvGraphic with 'advsep'.
--
advpunctuate :: Num u => AdvGraphic u -> [AdvGraphic u] -> AdvGraphic u
advpunctuate _  []     = emptyAdvGraphic
advpunctuate sep (x:xs) = step x xs
  where
    step a (b:bs) = step (a `advcat` sep `advcat` b) bs
    step a []     = a


-- | Render the supplied AdvGraphic, but swap the result advance
-- for the supplied vector. This function has behaviour analogue 
-- to @fill@ in the @wl-pprint@ library.
-- 
advfill :: Num u => Vec2 u -> AdvGraphic u -> AdvGraphic u
advfill sv = replaceAns sv