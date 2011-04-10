{-# LANGUAGE TypeFamilies               #-}
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
  
  , chain
  , chain_


  ) where


import Wumpus.Basic.Kernel.Base.BaseDefs
import Wumpus.Basic.Kernel.Base.ContextFun
import Wumpus.Basic.Kernel.Objects.Basis
import Wumpus.Basic.Kernel.Objects.LocImage
import Wumpus.Basic.Kernel.Objects.Image
import Wumpus.Basic.Utils.HList

import Wumpus.Core                              -- package: wumpus-core


-- In TikZ chains are finite node list and iterated (infite) points

data ChainAlg u = Linear (Point2 u -> Point2 u)
                | Switch (Int, ChainAlg u) (ChainAlg u) 



-- | Note the result list is infinite
--
interpChainAlg :: ChainAlg u -> Point2 u -> [Point2 u]
interpChainAlg ch start = go start ch
  where
    go pt (Linear fn)        = iterate fn pt
    go pt (Switch (n,c1) c2) = let (af,ptl) = takeAndLast n $ go pt c1
                               in prefixListH af $ go ptl c2
                                  


-- | Take n elements - also return the last of element in the 
-- tuple so it can be accessed without a second traversal.
--
-- Note @(n > 0)@ 
-- 
takeAndLast :: Int -> [a] -> (H a,a)
takeAndLast _ []      = error "takeAndLast - empty list (unreachable?)"
takeAndLast n (a:as)  = go (wrapH a,a) (n-1) as
  where
    go (af,_) i (x:xs) | i > 1     = go (af `snocH` x, x) (i-1) xs
    go acc    _ _                  = acc

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