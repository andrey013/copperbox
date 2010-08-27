{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Fresh.Picture
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Fresh picture.
--
--------------------------------------------------------------------------------


module Wumpus.Fresh.Picture
  ( 
    frameMulti
  , ellipse_
  , printPicture

  ) where

import Wumpus.Fresh.BoundingBox
import Wumpus.Fresh.Colour
import Wumpus.Fresh.FormatCombinators
import Wumpus.Fresh.Geometry
import Wumpus.Fresh.OneList
import Wumpus.Fresh.PictureInternal
import Wumpus.Fresh.PtSize
import Wumpus.Fresh.Utils

import Data.Semigroup                           -- package: algebra

import qualified Data.Sequence                  as S


-- This function throws an error when supplied the empty list.
--
frameMulti :: (Real u, Floating u, FromPtSize u) 
           => [Primitive u] -> Picture u
frameMulti []     = error "Wumpus.Core.Picture.frameMulti - empty list"
frameMulti (p:ps) = let (bb,ones) = step p ps 
                    in Leaf (bb,S.empty) ones 
  where
    step a []     = (boundary a, one a)
    step a (x:xs) = let (bb',rest) = step x xs
                    in (boundary a `append` bb', cons a rest)


ellipse_ :: Num u => u -> u -> Point2 u -> Primitive u
ellipse_ hw hh pt = PEllipse (EFill (RGB255 127 0 0)) NoLink body
  where
    body = PrimEllipse { ellipse_center        = pt
                       , ellipse_half_width    = hw
                       , ellipse_half_height   = hh
                       , ellipse_ctm           = identityCTM
                       } 


printPicture :: (Num u, PSUnit u) => Picture u -> IO ()
printPicture pic = putStrLn (show $ format pic) >> putStrLn []
