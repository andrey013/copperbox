{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Extra.Drawing
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC with TypeFamilies and more
--
-- Picture transformers ...
-- 
--------------------------------------------------------------------------------

module Wumpus.Extra.Drawing 
  ( 
  -- * Blank (coloured) pictures
    backgroundRect

  -- * Picture transformers
  , backgroundFill
  , backgroundStroke

  , clipPicture
  , clipToBoundary

  , nonuniformBackgroundGrid
  , backgroundGrid
 
  ) where



import Wumpus.Core


import Data.AffineSpace

import Control.Applicative ( liftA2 ) 
import Data.List ( unfoldr )


--------------------------------------------------------------------------------



-- | Coloured but otherwise blank picture, bottom left at the 
-- origin.
backgroundRect :: (Fractional u, Ord u, PSColour c) 
           => c -> u -> u -> Picture u
backgroundRect c w h = 
  frame $ fill (psColour c) $ vertexPath $ corners $ bbox zeroPt (P2 w h)

--------------------------------------------------------------------------------
-- Picture transformers

backgroundTrafo :: (Num u, Ord u) 
                => (BoundingBox u -> Picture u) -> Picture u -> Picture u
backgroundTrafo fn p = p `over` bkgrnd where
   bkgrnd = fn $ boundary p

-- | Fill the background of a picture (where the backgound area is 
-- given by the bounding box).
backgroundFill :: (Fractional u, Ord u, PSColour c) 
               => c -> Picture u -> Picture u
backgroundFill c = backgroundTrafo fn where
  fn = frame . fill (psColour c) . vertexPath . corners


backgroundStroke :: (Fractional u, Ord u, Stroke t)
                 => t -> Picture u -> Picture u
backgroundStroke attr = backgroundTrafo fn where
  fn = frame . cstroke attr . vertexPath . corners



-- Clipping

clipPicture :: (Fractional u, Ord u) 
            => BoundingBox u -> Picture u -> Picture u
clipPicture bb p = clip (vertexPath $ corners bb) p


clipToBoundary :: (Fractional u, Ord u) 
               => Picture u -> Picture u
clipToBoundary p = clip (vertexPath $ corners $ boundary p) p


-- Grid


gencoords :: (Num a, Ord a) 
          => (a -> b -> c) -> a -> b -> a -> a -> [c]  
gencoords fn a bconst step amax = unfoldr phi a where 
    phi n | n < amax  = Just (fn n bconst,n+step)
          | otherwise = Nothing


nonuniformBackgroundGrid :: (Fractional u, Ord u, Stroke t) 
                         => t -> u -> u -> Picture u -> Picture u
nonuniformBackgroundGrid attr xstep ystep = backgroundTrafo fn where
  fn bb@(BBox (P2 x0 y0) (P2 x1 y1)) = 
      frameMulti vlines `over` frameMulti hlines
    where
      width   = boundaryWidth  bb
      height  = boundaryHeight bb  
      hpoints = gencoords P2 x0 y0 xstep x1
      vpoints = gencoords (flip P2) y0 x0 ystep y1 
      hlines  = map (liftA2 mkline id (.+^ vvec height)) hpoints
      vlines  = map (liftA2 mkline id (.+^ hvec width )) vpoints

      mkline p1 p2 = ostroke attr $ vertexPath [p1,p2]


backgroundGrid :: (Fractional u, Ord u, Stroke t) 
               => t -> u -> Picture u -> Picture u
backgroundGrid attr step = nonuniformBackgroundGrid attr step step
