{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Extra.Path
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC with TypeFamilies and more
--
-- Build paths with Hughes lists.
--
-- 
-- 
--------------------------------------------------------------------------------

module Wumpus.Extra.Path
  ( 
  
  -- * Simplified path construction
    HPath
  , pathFrom
  , line_to
  , curve_to


  ) where

import Wumpus.Core
import Wumpus.Extra.Utils 


--------------------------------------------------------------------------------
-- /Hughes/ paths


newtype HPath u a = HPath { 
          getHPath :: PathState u -> PathTrace u -> (a,PathState u, PathTrace u) }

data PathTrace u = PathTrace 
      { start_point     :: Point2 u
      , path_segments   :: H (PathSegment u)
      }

data PathState u = PathState
      { current_point   :: Point2 u }

instance Monad (HPath u) where
  return a = HPath $ \ s w -> (a,s,w)
  (HPath f) >>= mf  = HPath $ \s w -> 
    let (a,s',w') = f s w in (getHPath . mf) a s' w'

tellLineTo :: Point2 u -> HPath u ()
tellLineTo pt = HPath $ \ s w -> ((), updSt s, updTr w)
  where
    updSt = \s -> s { current_point = pt }
    updTr = pstar (\hf s -> s { path_segments = hf `snocH` lineTo pt} )
                  path_segments

tellCurveTo :: Point2 u -> Point2 u -> Point2 u -> HPath u ()
tellCurveTo cp1 cp2 end = HPath $ \ s w -> ((), updSt s, updTr w)
  where
    curve_seg = curveTo cp1 cp2 end
    updSt     = \s -> s { current_point = end }
    updTr     = pstar (\hf s -> s {path_segments = hf `snocH` curve_seg}) 
                      path_segments



pathFrom :: (u,u) -> HPath u a -> Path u
pathFrom xy mf = post $ (getHPath mf) zero_state zero_trace 
  where
    post (_,_,tr) = path (start_point tr) (toListH $ path_segments tr)
    
    start_pt      = mkPoint xy
    zero_state    = PathState start_pt
    zero_trace    = PathTrace start_pt emptyH

mkPoint :: (u,u) -> Point2 u 
mkPoint (x,y) = P2 x y



line_to :: (u,u) -> HPath u ()
line_to = tellLineTo . mkPoint


curve_to :: (u,u) -> (u,u) -> (u,u) -> HPath u ()
curve_to cp1 cp2 end = tellCurveTo (mkPoint cp1) (mkPoint cp2) (mkPoint end)


--
-- vertical (length) & horizontal (length) might
-- be handy...
-- 
-- But we would need to track current position, vis-a-vis a 
-- state monad, so this is taking things towards a big module.
--
--
