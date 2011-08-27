{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Drawing.Paths.HPath
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Functional paths like Hughes Lists (DLists).
--
-- AbsPaths don\'t really support concatenation because they are 
-- always located - concatenation needs one of the paths to be 
-- relocated or a joint drawn between them.
--
-- This representation supports concatenation because HPaths have 
-- are just Hughes lists of /path instructions/.
-- 
-- This module should be simpler to use than PathBuilder if you 
-- just need paths - PathBuilder supports arbitrary decoration as 
-- well as path building.
-- 
--------------------------------------------------------------------------------

module Wumpus.Drawing.Paths.HPath
  ( 

    HPath
  , runHPath
  , snocHPath 

  , line 
  , curve

  , hline
  , vline

  , line_up
  , line_down
  , line_left
  , line_right

  , line_north
  , line_south
  , line_east
  , line_west
  , line_north_east
  , line_north_west
  , line_south_east
  , line_south_west

  , line_up_left
  , line_up_right
  , line_down_left
  , line_down_right

  , sineWave

  ) where

import Wumpus.Drawing.Paths.Base

import Wumpus.Basic.Kernel                      -- package: wumpus-basic
import Wumpus.Basic.Utils.HList

import Wumpus.Core                              -- package: wumpus-core

import Data.VectorSpace                         -- package: vector-space

import Data.Monoid


-- | A path /instruction/.
-- 
-- Paths are stored as lists of commands detailing their 
-- construction.
--
data PathCmd u = LineTo  (Vec2 u)
               | CurveTo (Vec2 u) (Vec2 u) (Vec2 u)

newtype HPath u = HPath { getHPath :: H (PathCmd u) }



instance Monoid (HPath u) where
  mempty        = HPath mempty
  a `mappend` b = HPath $ getHPath a `mappend` getHPath b

runHPath :: (Floating u, Ord u, Tolerance u) 
         => HPath u -> Point2 u -> AbsPath u
runHPath hp start = step (emptyPath start) (toListH $ getHPath hp)
  where
    step ac [] = ac
    step ac (LineTo v1 : xs)        = step (ac `snocLine` v1) xs
    step ac (CurveTo v1 v2 v3 : xs) = step (ac `snocCurve` (v1,v2,v3)) xs



snocHPath :: (Floating u, Ord u, Tolerance u) 
          => AbsPath u -> HPath u -> AbsPath u 
snocHPath absp hp = step absp (toListH $ getHPath hp)
  where
    step ac [] = ac
    step ac (LineTo v1 : xs)        = step (ac `snocLine` v1) xs
    step ac (CurveTo v1 v2 v3 : xs) = step (ac `snocCurve` (v1,v2,v3)) xs




line :: Vec2 u -> HPath u 
line = HPath . wrapH . LineTo 

curve :: Vec2 u -> Vec2 u -> Vec2 u -> HPath u
curve v1 v2 v3 = HPath $ wrapH $ CurveTo v1 v2 v3



hline :: Num u => u -> HPath u 
hline = line . hvec

vline :: Num u => u -> HPath u 
vline = line . vvec

line_up :: Num u => u -> HPath u
line_up = line . go_up

line_down :: Num u => u -> HPath u
line_down = line . go_down

line_left :: Num u => u -> HPath u
line_left = line . go_left

line_right :: Num u => u -> HPath u
line_right = line . go_right


line_north :: Num u => u -> HPath u
line_north = line_up

line_south :: Num u => u -> HPath u
line_south = line . go_down

line_east :: Num u => u -> HPath u
line_east = line . go_right

line_west :: Num u => u -> HPath u
line_west = line . go_left


line_north_east :: Floating u => u -> HPath u
line_north_east = line . go_north_east

line_north_west :: Floating u => u -> HPath u
line_north_west = line . go_north_west

line_south_east :: Floating u => u -> HPath u
line_south_east = line . go_south_east

line_south_west :: Floating u => u -> HPath u
line_south_west = line . go_south_west


line_up_left :: Num u => u -> HPath u
line_up_left = line . go_up_left

line_up_right :: Num u => u -> HPath u
line_up_right = line . go_up_right

line_down_left :: Num u => u -> HPath u
line_down_left = line . go_down_left

line_down_right :: Num u => u -> HPath u
line_down_right = line . go_down_right




sineWave :: (Real u, Floating u, Ord u, Tolerance u) 
         => u -> Vec2 u -> HPath u
sineWave h base_vec = 
    curve v1 dv2 dv3 `mappend` curve dv4  dv5  dv6  `mappend` curve dv7 dv8 dv9 
                     `mappend` curve dv10 dv11 dv12
  where
    base1 = vlength base_vec / 12
    h2    = h * (pi / 6)
    ang   = vdirection base_vec
    v1    = orthoVec     base1    h2  ang
    v2    = orthoVec  (2*base1)   h   ang
    v3    = orthoVec  (3*base1)   h   ang
    v4    = orthoVec  (4*base1)   h   ang
    v5    = orthoVec  (5*base1)   h2  ang
    v6    = orthoVec  (6*base1)   0   ang
    v7    = orthoVec  (7*base1) (-h2) ang
    v8    = orthoVec  (8*base1) (-h)  ang
    v9    = orthoVec  (9*base1) (-h)  ang
    v10   = orthoVec (10*base1) (-h)  ang
    v11   = orthoVec (11*base1) (-h2) ang
    v12   = orthoVec (12*base1)   0   ang

    dv2   = v2  ^-^ v1
    dv3   = v3  ^-^ v2
    dv4   = v4  ^-^ v3    
    dv5   = v5  ^-^ v4
    dv6   = v6  ^-^ v5
    dv7   = v7  ^-^ v6
    dv8   = v8  ^-^ v7
    dv9   = v9  ^-^ v8
    dv10  = v10 ^-^ v9
    dv11  = v11 ^-^ v10
    dv12  = v12 ^-^ v11

--
-- Implementation Note - sineWave
-- 
-- It is easier to calculate vectors from the start point, then 
-- /diff/ them with the previous vector.
--