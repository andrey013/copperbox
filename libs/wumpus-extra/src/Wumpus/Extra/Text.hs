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
  -- * Courier metrics at 48 point
    courier48_width
  , courier48_height
  , courier48_descender_height
  , courier48_spacer_width


  ) where


-- | 
courier48_width :: Double
courier48_width = 26.0

courier48_height :: Double
courier48_height = 39.0

courier48_descender_height :: Double
courier48_descender_height = 9.0

courier48_spacer_width :: Double
courier48_spacer_width = 3.0
