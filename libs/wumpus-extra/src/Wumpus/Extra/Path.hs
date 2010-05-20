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
  , makePath
  , start_path
  , line_to
  , curve_to


  ) where

import Wumpus.Core

type H a = [a] -> [a]

emptyH :: H a
emptyH = id

snocH :: H a -> a -> H a
snocH hf a = hf . (a:)

toListH :: H a -> [a]
toListH = ($ [])



--------------------------------------------------------------------------------
-- /Hughes/ paths


data HPath u = HPath 
      { start_point     :: Point2 u
      , current_point   :: Point2 u
      , path_segments   :: H (PathSegment u)
      }


makePath :: HPath u -> Path u
makePath p = path (start_point p) (toListH $ path_segments p)

mkPoint :: (u,u) -> Point2 u 
mkPoint (x,y) = P2 x y

start_path :: (u,u) -> HPath u
start_path xy = let pt = mkPoint xy in HPath pt pt emptyH

infixl 6 `line_to`, `curve_to`

line_to :: HPath u -> (u,u) -> HPath u
line_to (HPath s _ f) xy = 
    let pt = mkPoint xy in HPath s pt (f `snocH` lineTo pt)


curve_to :: HPath u -> ((u,u),(u,u),(u,u)) -> HPath u
curve_to (HPath s _ f) (cp1,cp2,xy) = let end = mkPoint xy in
    HPath s end (f `snocH` curveTo (mkPoint cp1) (mkPoint cp2) end)


--
-- vertical (length) & horizontal (length) might
-- be handy...
-- 
-- But we would need to track current position, vis-a-vis a 
-- state monad, so this is taking things towards a big module.
--
--
