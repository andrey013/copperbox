{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Extra.Text
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  experimental
-- Portability :  GHC only
--
-- Text handling
-- 
--------------------------------------------------------------------------------

module Wumpus.Extra.Text 
  ( 

  -- * Picture constructors
    textline
  , colouredTextline

  ) where

import Wumpus.Core

import Data.Aviary

-- | Draw a line of text at the given point. The bounding box is
-- calculated with Courier metrics - as Courier is a monospaced
-- font the bounding bow will be strictly wider than necessary 
-- for variable width fonts.
--
-- The supplied point represents the bottom-left corner of the 
-- bounding box accounting for descenders, this is different to
-- PostScript\'s behaviour where the the start point is the 
-- bottom-left of a stem (e.g. H,I) and does not account for
-- descenders.
--
textline :: (Fractional u, Ord u) 
         => FontAttr -> Point2 u -> String -> Picture u
textline = frame `ooo` textlabel 



-- | Coloured version of 'textline'. Same conditions vis bounding
-- box metrics and start point apply. 
colouredTextline :: (Fractional u, Ord u, PSColour c)
                 => c -> FontAttr -> Point2 u -> String -> Picture u
colouredTextline  c attr = frame `oo` textlabel (psColour c,attr)