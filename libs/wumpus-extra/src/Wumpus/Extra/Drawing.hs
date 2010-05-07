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
  , strokedCrossedRect


  -- * Picture transformers
  , backgroundFill
  , backgroundStroke

  , clipPicture
  , clipToBoundary

  , rectBackgroundGrid
  , backgroundGrid

  , rectGridPicture
  , gridPicture
 
  ) where


import Wumpus.Core

import Wumpus.Extra.BasicObjects
import Wumpus.Extra.Utils


import Data.AffineSpace

import Control.Applicative ( liftA2 ) 
import Data.List ( unfoldr )


--------------------------------------------------------------------------------



-- | Coloured but otherwise blank picture, bottom left at the 
-- origin.
backgroundRect :: (Fractional u, Ord u, PSColour c) 
               => c -> u -> u -> Picture u
backgroundRect c w h = 
  frame $ fillPolygon (psColour c) $ rectangle w h zeroPt


strokedCrossedRect :: (Fractional u, Ord u, Stroke t) 
                   => t -> t -> u -> u -> Picture u
strokedCrossedRect attr_border attr_cross w h = border `over` cross 
  where 
    border = frame $ strokePolygon attr_border $ rectangle w h zeroPt
    cross  = frameMulti [fn zeroPt (P2 w h), fn (P2 0 h) (P2 w 0)]
    fn     = ostroke attr_cross `oo` appro path id (return . lineTo)

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
    phi n | n <= amax = Just (fn n bconst,n+step)
          | otherwise = Nothing

mkGrid :: (Fractional u, Ord u, Stroke t) 
       => t -> u -> u -> u -> u -> u -> u -> Picture u
mkGrid attr x y w h xstep ystep = frameMulti vlines `over` frameMulti hlines
  where
      hpoints = gencoords P2 x y xstep (x+w)
      vpoints = gencoords (flip P2) y x ystep (y+h)
      hlines  = map (liftA2 mkline id (.+^ vvec h)) hpoints
      vlines  = map (liftA2 mkline id (.+^ hvec w)) vpoints

      mkline p1 p2 = ostroke attr $ vertexPath [p1,p2]
 

-- | Draw a background grid of rectangles (width and height need
-- not be the same).
--
-- The grid is drawn to the picture area so the top and right 
-- edges of the grid may be trucated and not show complete
-- squares.
--
rectBackgroundGrid :: (Fractional u, Ord u, Stroke t) 
                   => t -> u -> u -> Picture u -> Picture u
rectBackgroundGrid attr xstep ystep = backgroundTrafo fn where
  fn bb@(BBox (P2 x0 y0) _) = mkGrid attr x0 y0 width height xstep ystep 
    where
      width   = boundaryWidth  bb
      height  = boundaryHeight bb  
                                      

-- | Draw a background grid.
--
-- The grid is drawn to the picture area so the top and right 
-- edges of the grid may be trucated and not show complete
-- squares.
--
backgroundGrid :: (Fractional u, Ord u, Stroke t) 
               => t -> u -> Picture u -> Picture u
backgroundGrid attr step = rectBackgroundGrid attr step step


-- | Grid (no truncation). ...
rectGridPicture :: (Fractional u, Ord u, Stroke t) 
                 => t -> Int -> Int -> u -> u -> Picture u
rectGridPicture attr row_count col_count xstep ystep = 
    mkGrid attr 0 0 width height xstep ystep 
  where
    width  = xstep * fromIntegral col_count
    height = ystep * fromIntegral row_count

-- | Grid (no truncation). ...
gridPicture :: (Fractional u, Ord u, Stroke t) 
                 => t -> Int -> Int -> u -> Picture u
gridPicture attr row_count col_count step = 
    rectGridPicture attr row_count col_count step step 


    



