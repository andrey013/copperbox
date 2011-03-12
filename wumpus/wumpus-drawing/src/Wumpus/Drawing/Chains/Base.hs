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

    chainDisplace
  , chainIterate

  , apChainDisplace
  , apChainIterate

  , interChainDisplace

  ) where

import Wumpus.Basic.Kernel                      -- package: wumpus-basic

import Wumpus.Core                              -- package: wumpus-core




-- | chainDisplace with explicit empty.
--
-- | 'chainDisplaceAU' : @ disp_fun * [loc_fun] -> LocImage (end_point) @
--
-- Distribute the list of @loc_fun@ whilst the start point is 
-- iterated with the displacement function.
-- 
-- 'distribute' will typically be used for a LocImage or a 
-- LocGraphic.
--
-- This models applying a finite list of LocFuns to an infinite
-- supply of points.
--
chainDisplace :: InterpretUnit u 
              => PointDisplace u 
              -> [LocImage t u]
              -> LocImage Point2 u
chainDisplace _    []     = promote_li1 $ \start -> 
    replaceAns start $ emptyLocGraphic `at` start

chainDisplace next (f:fs) = promote_li1 $ \start -> 
    go (next start) (start, ignoreAns $ f `at` start) fs
  where
    go _  (pt,acc) []    = replaceAns pt $ acc
    go pt (_,acc) (g:gs) = let g1 = ignoreAns $ g `at` pt
                           in go (next pt) (pt, acc `oplus` g1) gs



-- radial seems easier if we iterate the thing we displace the 
-- start point with, rather than iterate the start point itself...



-- | chainIterate with explicit empty.
--
chainIterate :: InterpretUnit u
             => (s -> s) -> (s -> PointDisplace u) -> s -> [LocImage t u] 
             -> LocImage Point2 u
chainIterate _    _   _  []     = promote_li1 $ \start -> 
    replaceAns start $ emptyLocGraphic `at` start

chainIterate next gen s  (f:fs) = promote_li1 $ \start -> 
    let fn = gen `flip` start
        p0 = fn s
    in go fn (next s) (p0, ignoreAns $ f `at` p0) fs
  where
    go _    _  (pt,acc) []     = replaceAns pt $ acc
    go mkpt st (_, acc) (g:gs) = let p1 = mkpt st 
                                     g1 = ignoreAns $ g `at` p1
                                 in go mkpt (next st) (p1, acc `oplus` g1) gs





-- | Variant of 'chainDisplace' where a LocGraphic building 
-- function is applied to a list of values...
--
apChainDisplace :: InterpretUnit u 
                => PointDisplace u -> (a -> LocImage t u) -> [a] 
                -> LocImage Point2 u
apChainDisplace _    _  []     = promote_li1 $ \start -> 
    replaceAns start $ emptyLocGraphic `at` start

apChainDisplace next gf (x:xs) = promote_li1 $ \start -> 
    go (next start) (start, ignoreAns $ gf x `at` start) xs
  where
    go _  (pt,acc) []     = replaceAns pt $ acc
    go pt (_, acc) (y:ys) = let g1 = ignoreAns $ gf y `at` pt
                            in go (next pt) (pt, acc `oplus` g1) ys
    


-- | Variant of 'chainIterate' where a LocGraphic building 
-- function is applied to a list of values...
--
apChainIterate :: InterpretUnit u 
               => (s -> s) -> (s -> PointDisplace u) -> s 
               -> (a -> LocImage t u) -> [a] -> LocImage Point2 u
apChainIterate _    _   _ _  []     = promote_li1 $ \start -> 
    replaceAns start $ emptyLocGraphic `at` start

apChainIterate next gen s gf (x:xs) = promote_li1 $ \start -> 
    let fn = gen `flip` start
        p0 = fn s
    in go fn (next s) (p0, ignoreAns $ gf x `at` p0) xs 
  where
    go _  _  (pt,acc) []     = replaceAns pt $ acc
    go fn st (_, acc) (y:ys) = let p1 = fn st
                                   g1 = ignoreAns $ gf y `at` p1
                               in go fn (next st) (p1, acc `oplus` g1) ys





-- | Variant of 'chainDisplace' where a LocGraphic building 
-- function is applied to a list of values...
--
interChainDisplace :: InterpretUnit u 
                   => [PointDisplace u] -> [LocImage t u] -> LocImage Point2 u
interChainDisplace _   [] = promote_li1 $ \start -> 
    replaceAns start $ emptyLocGraphic `at` start

interChainDisplace mvs (z:zs) = promote_li1 $ \start ->  
    step1 start (ignoreAns $ z `at` start) mvs zs
  where
    step1 pt acc []      _      = replaceAns pt acc
    step1 pt acc (mv:ms) gs     = step2 (mv pt) acc ms gs

    step2 pt acc _       []     = replaceAns pt acc 
    step2 pt acc ms      (g:gs) = let g1 = ignoreAns $ g `at` pt
                                  in step1 pt (acc `oplus` g1) ms gs
 
