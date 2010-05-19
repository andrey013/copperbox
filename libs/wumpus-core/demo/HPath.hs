{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  HPath
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC with TypeFamilies and more
--
-- Build paths with Hughes lists.
--
-- This module will probably go in Wumpus.Extra at some point. 
-- 
--------------------------------------------------------------------------------

module HPath 
  ( 
  
  -- * Simplified path construction
    HPath
  , makePath
  , start_path
  , line_to
  , curve_to

  -- * Example
  , dog_kennel

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


type HPath u = (Point2 u, H (PathSegment u))

makePath :: HPath u -> Path u
makePath (s,hf) = path s (toListH hf)


start_path :: (u,u) -> HPath u
start_path (x,y) = (P2 x y, emptyH)

infixl 6 `line_to`, `curve_to`

line_to :: HPath u -> (u,u) -> HPath u
line_to (s,f) (x,y) = (s, f `snocH` lineTo (P2 x y))

curve_to :: HPath u -> ((u,u),(u,u),(u,u)) -> HPath u
curve_to (s,f) ((c1x,c1y),(c2x,c2y),(ex,ey)) = 
    (s, f `snocH` curveTo (P2 c1x c1y) (P2 c2x c2y) (P2 ex ey))


--
-- vertical (length) & horizontal (length) might
-- be handy...
-- 
-- But we would need to track current position, vis-a-vis a 
-- state monad, so this is taking things towards a big module.
--
--

--------------------------------------------------------------------------------
-- Demo - draw a dog kennel...

dog_kennel :: DPath
dog_kennel = makePath $ 
    start_path (0,0) `line_to`  (0,60)   `line_to` (40,100)
                     `line_to`  (80,60)  `line_to` (80,0)
                     `line_to`  (60,0)   `line_to` (60,30)
                     `curve_to` ((60,50), (50,60), (40,60))
                     `curve_to` ((30,60), (20,50), (20,30))
                     `line_to`  (20,0)
                        
