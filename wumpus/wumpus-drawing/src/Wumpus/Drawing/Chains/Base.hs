{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Drawing.Chains.Base
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC 
--
-- Generate points in an iterated chain.
--
--------------------------------------------------------------------------------

module Wumpus.Drawing.Chains.Base
  (

    PointChain
  , LocChain
  , ConnectorChain

  -- * Unrolling chains
  , unchain
  , unchainU
  , unchainZip
  , unchainZipWith
  , unconnectorChain

  -- * Building chains
  , liftChainF

  ) where

import Wumpus.Basic.Kernel                      -- package: wumpus-basic

import Wumpus.Core                              -- package: wumpus-core



-- | A 'PointChain' is a list of points. 
-- 
-- The list is typically inifinte so some attention must be given 
-- to choosing a chain drawing function and using it appropriately.
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




-- | 'unchain' : @ unroll_count * alt_fun * draw_fun * chain -> LocCF @
-- 
-- Unroll the chain, applying the @draw_fun@ to each point 
-- producing a LocCF (usually a 'LocGraphic'). If the chain does 
-- not produce any points the @alt_fun@ is applied to the start 
-- point.
--
-- Note - commonly a 'Chain' may be infinite, so it is only 
-- unrolled a finite number of times - the @unrool_count@.
--
-- This function has a very general type signature commonly it 
-- will be used at these types:
--
-- > unchain :: (Num u, OPlus a) => 
-- >     Int -> LocImage u a -> LocImage u a -> LocChain u -> LocImage u a
-- >
-- > unchain :: Num u => 
-- >     Int -> LocGraphic u -> LocGraphic u -> LocChain u -> LocGraphic u
--
unchain :: (Num u, OPlus a) 
        => Int 
        -> LocCF u (ImageAns u a) 
        -> LocCF u (ImageAns u a) 
        -> LocChain u 
        -> LocCF u (ImageAns u a)
unchain i alt _  _   | i <= 0 = alt
unchain i alt gf chn          = promoteR1 $ \p0 -> 
    (chn `at` p0) >>= \pts -> case pts of 
      []     -> alt `at` p0
      [x]    -> gf  `at` x
      (x:xs) -> go x (take (i-1) xs)
  where
    go x []     = gf `at` x
    go x (y:ys) = (gf `at` x) `oplus` go y ys




-- | 'unchain' : @ alt_fun * draw_fun * chain -> LocCF @
-- 
-- /Unsafe/ version of 'unchain' - this function assumes the chain
-- is finite which is not usually the case.
--
-- This function has a very general type signature commonly it 
-- will be used at these type:
--
-- > unchainU :: (Num u, OPlus a) => 
-- >     LocImage u a -> LocImage u a -> LocChain u -> LocImage u a
-- >
-- > unchainU :: Num u => 
-- >     LocGraphic u -> LocGraphic u -> LocChain u -> LocGraphic u
--
-- \*\* WARNING \*\* - if the chain is infinite this function will 
-- not terminate.
--
unchainU :: (Num u, OPlus a)
         => LocCF u (ImageAns u a) 
         -> LocCF u (ImageAns u a) 
         -> LocChain u 
         -> LocCF u (ImageAns u a)
unchainU alt gf chn = promoteR1 $ \p0 -> 
    (chn `at` p0) >>= \pts -> case pts of 
      []     -> alt `at` p0
      [x]    -> gf  `at` x
      (x:xs) -> go x xs
  where
    go x []     = gf `at` x
    go x (y:ys) = (gf `at` x) `oplus` go y ys


-- | 'unchainZip' : @ alt_fun * [draw_fun] * chain -> LocCF @
-- 
-- Unroll the chain, zipping the list of @draw_funs@ to the list
-- of points producing a LocCF (usually a 'LocGraphic'). If the 
-- chain does not produce any points the @alt_fun@ is applied to 
-- the start point.
--
-- This function has a very general type signature commonly it 
-- will be used at these types:
--
-- > unchainZip :: (Num u, OPlus a) => 
-- >     LocImage u a -> [LocImage u a] -> LocChain u -> LocImage u a
-- >
-- > unchainZip :: Num u => 
-- >     LocGraphic u -> [LocGraphic u] -> LocChain u -> LocGraphic u
--
-- \*\* WARNING \*\* - the list of drawing functions should be 
-- finite. If both the list of drawing functions and the chain are 
-- infinite this function will not terminate.
-- 
unchainZip :: (Num u, OPlus a) 
           => LocCF u (ImageAns u a) 
           -> [LocCF u (ImageAns u a)] 
           -> LocChain u 
           -> LocCF u (ImageAns u a)
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



-- | 'unchainZipWith' : @ alt_fun * (a -> draw_fun) * [a] * chain -> LocCF @
-- 
-- Version of 'unchainZip' where the list is some data rather 
-- than a drawing function and the @(a -> draw_fun)@ builder is 
-- applied to each element as part of the unrolling.
-- 
-- Approximately this function is a @zipWith@ to the @zip@ of 
-- @unchainZip@.
--
-- This function has a very general type signature commonly it 
-- will be used at these type:
--
-- > unchainZipWith :: (Num u, OPlus a) => 
-- >     LocImage u a -> (s -> LocImage u a) -> [s] -> LocChain u -> LocImage u a
-- >
-- > unchainZipWith :: Num u => 
-- >     LocGraphic u -> (s -> LocGraphic u) -> [s] -> LocChain u -> LocGraphic u
--
-- \*\* WARNING \*\* - if the chain and list are infinite this 
-- function will not terminate.
--
unchainZipWith :: (Num u, OPlus a)
                => LocCF u (ImageAns u a) 
                -> (s -> LocCF u (ImageAns u a))
                -> [s]
                -> LocChain u 
                -> LocCF u (ImageAns u a)
unchainZipWith alt _    []     _   = promoteR1 $ \p0 -> alt `at` p0
unchainZipWith alt mkGF (s:ss) chn = promoteR1 $ \p0 -> 
    (chn `at` p0) >>= \pts -> case pts of 
      []     -> alt `at` p0
      [x]    -> mkGF s `at` x
      (x:xs) -> go (mkGF s `at` x) ss xs
  where
    go acc _      []     = acc
    go acc []     _      = acc
    go acc (t:ts) (p:ps) = go (acc `oplus` (mkGF t `at` p)) ts ps




-- | 'unconnectorChain' : @ alt_fun * draw_fun * conn_chain -> ConnectorCF @
--
-- Unroll the chain produced between the implicit start and end 
-- points. Apply the @draw_fun@ to each point producing a 
-- ConnectorCF (usually a 'ConnectorGraphic'). If the chain does 
-- not produce any points, the @alt_fun@ is applied to the start 
-- and end points.
--
-- This function has a very general type signature commonly it 
-- will be used at these types:
--
-- > unconnectorChain :: (Num u, OPlus a) => 
-- >     ConnectorImage u a -> LocImage u a -> ConnectorChain u -> ConnectorImage u a
-- >
-- > unconnectorChain :: Num u => 
-- >     ConnectorGraphic u -> LocGraphic u -> ConnectorChain u -> ConnectorGraphic u
--
--
unconnectorChain :: (Num u, OPlus a) 
                 => ConnectorCF u (ImageAns u a) 
                 -> LocCF u (ImageAns u a) 
                 -> ConnectorChain u 
                 -> ConnectorCF u (ImageAns u a)
unconnectorChain alt gf cchn = promoteR2 $ \p0 p1 -> 
    (connect cchn p0 p1) >>= \pts -> case pts of
      []     -> connect alt p0 p1
      [x]    -> gf `at` x
      (x:xs) -> go (gf `at` x) xs
  where
    go acc []     = acc
    go acc (p:ps) = go (acc `oplus` (gf `at` p)) ps




--------------------------------------------------------------------------------

-- | 'liftChainF' : @ (point -> [point]) -> LocChain @
--
-- Lift a pure chain generating function inot a 'LocChain'.
--
liftChainF :: (Point2 u -> PointChain u) -> LocChain u
liftChainF fn = promoteR1 $ \pt -> return $ fn pt

