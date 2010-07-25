{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Timing.Drawing
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC 
--
-- 
--
--------------------------------------------------------------------------------

module Wumpus.Timing.Drawing
  (

    Prefix(..)
  , high
  , low
  , highImpedence
    
  , fillCamferedRect
  , fillRightCamferedRect

  , Metastasis(..)
  , metastasis

  , metastasisEven
  , metastasisOdd

  ) where

import Wumpus.Core                              -- package: wumpus-core
import Wumpus.Basic.Graphic                     -- package: wumpus-basic

import Data.AffineSpace                         -- package: vector-space

import Data.List


data Prefix = Init
            | FromTop
            | FromCtr
            | FromBtm
  deriving (Eq,Ord,Show)

-- TikZ-Timing actaull has more options than these 4...


dispLine :: (Stroke t, Num u) => t -> [Vec2 u] -> GraphicF u
dispLine t vs = \pt -> 
    wrapG $ ostroke t $ vertexPath $ map (pt .+^) vs
 
div16 :: Fractional u => u -> u
div16 = (0.0625 *)

div8 :: Fractional u => u -> u
div8 = (0.125 *)


width :: Num u  => Int -> u -> u
width w hh = hh * fromIntegral w

high :: Stroke t => t -> Prefix -> Int -> Double -> DGraphicF
high t pre wi hh = dispLine t $ fn pre
  where
    final       = vec (width wi hh) (hh) 

    fn FromCtr  = [ vec 0 0,     vec (div16 hh) hh, final ]
    fn FromBtm  = [ vec 0 (-hh), vec (div8  hh) hh, final ]
    fn _        = [ vec 0 hh,    final ] 


low :: Stroke t => t -> Prefix -> Int -> Double -> DGraphicF
low t pre wi hh = dispLine t $ fn pre
  where
    final       = vec (width wi hh) (-hh) 

    fn FromCtr  = [ vec 0 0,     vec (div16 hh) (-hh), final ]
    fn FromTop  = [ vec 0 hh,    vec (div8  hh) (-hh), final ]
    fn _        = [ vec 0 (-hh), final ] 


-- Note - Init and FromCtr are different...
--
highImpedence :: Stroke t => t -> Prefix -> Int -> Double -> DGraphicF
highImpedence t pre wi hh = dispLine t $ fn pre
  where
    final       = vec (width wi hh) 0

    fn FromTop  = [ vec 0 hh,        vec (div16 hh)    0, final ]
    fn FromBtm  = [ vec 0 (-hh),     vec (div16 (-hh)) 0, final ]
    fn FromCtr  = [ vec (div8 hh) 0, final ] 
    fn _        = [ vec 0 0,         final ]     -- aka Start 







fillCamferedRect :: (Fill t, Fractional u) => t -> Int -> u -> GraphicF u
fillCamferedRect t wi halfsize = wrapG . fill t . camferedPath wi halfsize

fillRightCamferedRect :: (Fill t, Fractional u) => t -> Int -> u -> GraphicF u
fillRightCamferedRect t wi halfsize = 
    wrapG . fill t . rightCamferedPath wi halfsize

camferedPath :: Fractional u => Int -> u -> Point2 u -> Path u
camferedPath wi hh leftvmid = 
    vertexPath $ leftvmid : sequence [tl, tr , rightvmid, br, bl] leftvmid
  where
    w           = width wi hh 
    cam         = 0.25 * hh 
    tl          = (.+^ vec cam     hh)
    tr          = (.+^ vec w       hh)
    rightvmid   = (.+^ vec (w+cam) 0)
    br          = (.+^ vec w       (-hh))
    bl          = (.+^ vec cam     (-hh))

-- right camfer extended past the width
--
rightCamferedPath :: Fractional u => Int -> u -> Point2 u -> Path u
rightCamferedPath wi hh leftvmid = 
    vertexPath $ sequence [tl, tr , rightvmid, br, bl] leftvmid
  where
    w           = width wi hh
    cam         = 0.25 * hh
    tl          = (.+^ vec 0       hh)
    tr          = (.+^ vec w       hh)
    rightvmid   = (.+^ vec (w+cam) 0)
    br          = (.+^ vec w       (-hh))
    bl          = (.+^ vec 0       (-hh))



data Metastasis = Even | Odd
  deriving (Eq,Ord,Show)


metastasis :: (Stroke t,  Fractional u) 
           => t -> Int -> Metastasis -> u -> GraphicF u 
metastasis t wi start hh = 
    wrapG . ostroke t . metastasisPath wi start hh

 
metastasisPath :: Fractional u => Int -> Metastasis -> u -> Point2 u -> Path u 
metastasisPath wi start hh leftvmid
    | start == Even = vertexPath $ expand0 $ metastasisEven wi
    | otherwise     = vertexPath $ expand0 $ metastasisOdd wi
  where
    point n h      = leftvmid .+^ vec (uw * fromIntegral n) h
    uh             = 0.5  * hh
    uw             = 0.25 * uh
  
    expand0 (x:xs) = point x 0 : expandU xs
    expand0 _      = []
    
    expandU [x,y]  = [point x uh, point y 0]
    expandU (x:xs) = point x uh : expandD xs
    expandU _      = []

    expandD [x,y]  = [point x (-uh), point y 0]
    expandD (x:xs) = point x (-uh) : expandU xs
    expandD _      = []

  

metastasisEven :: Int -> [Int]
metastasisEven n = 0 : unfoldr phi 1
  where
    nmax = 8*n
    phi i | i < nmax    = Just (i, i+2)
          | i == nmax+1 = Just (nmax, i+2)
          | otherwise   = Nothing

metastasisOdd :: Int -> [Int]
metastasisOdd n = 2 : unfoldr phi 3
  where
    nmax = 8*n
    phi i | i < nmax    = Just (i, i+2)
          | i == nmax+1 = Just (nmax, i+2)
          | otherwise   = Nothing

