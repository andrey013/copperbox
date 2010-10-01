{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Microprint.Datatypes
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Base datatypes.
--
--------------------------------------------------------------------------------

module Wumpus.Microprint.Datatypes
  (

  -- * Datatypes  
 
    Tile(..)
  , Height
  , GreekText

  ) where

import Wumpus.Core




data Tile = Space Int | Word RGBi Int
  deriving (Eq,Ord,Show)

type Height = Int


-- Note probably better if used a list of lines instead
--
-- > [[Title]] 
--
-- and did not have line break in the Tile datatype.
--
type GreekText = (Height,[[Tile]])


 

