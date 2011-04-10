{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Kernel.Objects.Chain
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  highly unstable
-- Portability :  GHC 
--
-- Chaining moveable LocGraphics.
--
--------------------------------------------------------------------------------

module Wumpus.Basic.Kernel.Objects.Chain
  (
    ChainAlg(..)

  , interpChainAlg -- TEMP
  
  , chain
  , chain_

  , chainH
  , chainV

  ) where


import Wumpus.Basic.Kernel.Base.BaseDefs
import Wumpus.Basic.Kernel.Base.ContextFun
import Wumpus.Basic.Kernel.Objects.Basis
import Wumpus.Basic.Kernel.Objects.LocImage
import Wumpus.Basic.Kernel.Objects.Image

import Wumpus.Core                              -- package: wumpus-core

import Data.AffineSpace                         -- package: vector-space


-- In TikZ chains are finite node list and iterated (infite) points

data ChainAlg u = Linear (Point2 u -> Point2 u)
                | Switch Int (ChainAlg u) (ChainAlg u) 

type instance DUnit (ChainAlg u) = u


-- | Note the result list is infinite
--
interpChainAlg :: forall u. ChainAlg u -> Point2 u -> [Point2 u]
interpChainAlg alg start = run alg start
  where
    run :: ChainAlg u -> Point2 u -> [Point2 u]
    run (Linear fn)        pt = pt : run (Linear fn) (fn pt)

    run (Switch n chl chr) pt 
        | n <  1              = run chr pt
        | n == 1              = let (a,next,_) = one chl pt 
                                in a : run chr next
        | otherwise           = let (a,next,chl') = one chl pt 
                                in a : run (Switch (n-1) chl' chr) next
 

    one :: ChainAlg u -> Point2 u -> (Point2 u, Point2 u, ChainAlg u)
    one (Linear fn)        pt = (pt, fn pt, Linear fn)
    one (Switch n chl chr) pt 
        | n <  1              = one chr pt
        | n == 1              = let (a,next,_) = one chl pt
                                in (a,next,chr)
        | otherwise           = let (a,next, chl') = one chl pt
                                in (a,next, Switch (n-1) chl' chr)




-- | Returns the end point...
--
chain :: InterpretUnit u 
      => ChainAlg u -> [LocImage u a] -> LocImage u (Point2 u)
chain _   []      = promoteR1 $ \pt -> 
    fmap (replaceAns pt) $ emptyLocGraphic `at` pt

chain alg fs = promoteR1 $ \pt -> 
    let ps = interpChainAlg  alg pt in go1 fs pt ps 
  where
    go1 (gf:gs) _  (p:ps)     = go (graphic_ $ gf `at` p) gs p ps
    go1 _       p0 _          = fmap (replaceAns p0) $ emptyLocGraphic `at` p0

    go acc (gf:gs) pt (p:ps)  = let g1 = graphic_ $ gf `at` pt
                                in go (acc `oplus` g1) gs  p ps
    go acc _       pt _       = fmap (replaceAns pt) acc


-- | Returns no answer, just a 'LocGraphic'.
chain_ :: InterpretUnit u => ChainAlg u -> [LocImage u a] -> LocGraphic u
chain_ alg xs = locGraphic_ $ chain alg xs


chainH :: Num u => u -> ChainAlg u
chainH dx = Linear (\pt -> pt .+^ hvec dx)

chainV :: Num u => u -> ChainAlg u
chainV dy = Linear (\pt -> pt .+^ vvec dy)


