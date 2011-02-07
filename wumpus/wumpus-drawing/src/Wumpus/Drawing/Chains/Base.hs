{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Drawing.Chains.Base
-- Copyright   :  (c) Stephen Tetley 2010-2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC 
--
-- Generate points in an iterated chain.
--
-- \*\* WARNING \*\* - unstable. Names are not so good, also 
-- Wumpus-Basic has a @chain1@ operator...
-- 
--------------------------------------------------------------------------------

module Wumpus.Drawing.Chains.Base
  (

    PointChain
  , LocChain
  , ConnectorChain
  , unchain
  , unchainU
  , unchainZip
  , unconnectorChain


  ) where

import Wumpus.Basic.Kernel                      -- package: wumpus-basic

import Wumpus.Core                              -- package: wumpus-core


-- CAUTION - chains are all pure rather than CF / LocCF types.
-- 
-- Maybe this does not matter, however experience with PosGraphic 
-- shows that mis-judging whether a type is pure or monadic 
-- (depending on the DrawingContext) can have very unfortunate 
-- consequences for the utility of the type.
--
-- UPDATE - changed to DrawingInfo [Point2 u]




-- | A 'PointChain' is a list of points. The list is often expected to 
-- be inifinte, but if it was a Stream  it would loose the ability
-- to use list comprehensions.
-- 
-- 
type PointChain u = [Point2 u]


-- | A LocChain is a function from a starting point to a 
-- 'PointChain'.
--
-- The list is often expected to be inifinte, but if it was a 
-- Stream it would loose the ability to use list comprehensions.
-- 
type LocChain u = LocDrawingInfo u (PointChain u)


-- | A ConnectorChain is a function from a start- and end-point 
-- to a 'Chain'.
-- 
type ConnectorChain u = ConnectorCF u (PointChain u)

--
-- TODO - unchain types should be generalized to LocCF if 
-- possible.
--
-- Also an intoLocChain function would probably be useful...
--



-- | Note - commonly a 'Chain' may be infinite, so it is only 
-- unrolled a finite number of times.
--
unchain :: Num u => Int -> LocGraphic u -> LocChain u -> LocGraphic u
unchain i _  _   | i <= 0 = emptyLocGraphic
unchain i gf chn          = promoteR1 $ \p0 -> 
    (chn `at` p0) >>= \pts -> case pts of 
      []     -> gf `at` p0         -- This is a cheat.
      [x]    -> gf `at` x
      (x:xs) -> go x (take (i-1) xs)
  where
    go x []     = gf `at` x
    go x (y:ys) = (gf `at` x) `oplus` go y ys

-- Note - if the chain produces no points the LocGraphic is drawn 
-- at p0. This is a cheat, but otherwise this function would have 
-- to throw a runtime error.
--



-- | /Unsafe/ version of chain - this function assumes the chain
-- is finite which is not usually the case.
--
-- \*\* WARNING \*\* - if the chain is infinite this function will 
-- not terminate.
--
unchainU :: Num u => LocGraphic u -> LocChain u -> LocGraphic u
unchainU gf chn = promoteR1 $ \p0 -> 
    (chn `at` p0) >>= \pts -> case pts of 
      []     -> gf `at` p0         -- This is a cheat.
      [x]    -> gf `at` x
      (x:xs) -> go x xs
  where
    go x []     = gf `at` x
    go x (y:ys) = (gf `at` x) `oplus` go y ys


-- | Note - this function uses an alternative 'LocCF' if the list 
-- or the chain are empty.
--
unchainZip :: (Num u, OPlus a, u ~ DUnit a) 
           => LocCF u (ImageAns u a) 
           -> [LocCF u (ImageAns u a)] -> LocChain u -> LocCF u (ImageAns u a)
unchainZip alt []     _   = promoteR1 $ \p0 -> alt `at` p0
unchainZip alt  (g:gs) chn = promoteR1 $ \p0 -> 
    (chn `at` p0) >>= \pts -> case pts of 
      []     -> alt `at` p0
      [x]    -> g `at` x
      (x:xs) -> go (g `at` x) gs xs
  where
    go acc _      []     = acc
    go acc []     _      = acc
    go acc (f:fs) (p:ps) = go (acc `oplus` (f `at` p)) fs ps


unconnectorChain :: Num u => LocGraphic u -> ConnectorChain u -> ConnectorGraphic u
unconnectorChain gf cchn = promoteR2 $ \p0 p1 -> 
    (connect cchn p0 p1) >>= \pts -> case pts of
      []     -> connect emptyConnectorGraphic p0 p1
      [x]    -> gf `at` x
      (x:xs) -> go (gf `at` x) xs
  where
    go acc []     = acc
    go acc (p:ps) = go (acc `oplus` (gf `at` p)) ps


-- For Wumpus-Basic...

emptyConnectorGraphic :: Num u => ConnectorGraphic u 
emptyConnectorGraphic = promoteR2 $ \start end -> 
    let a = emptyLocGraphic `at` start
        b = emptyLocGraphic `at` end
    in a `oplus` b

