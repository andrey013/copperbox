{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.MicroPrint
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- MicroPrints
--
--------------------------------------------------------------------------------

module Wumpus.MicroPrint
  (
    
    module Wumpus.MicroPrint.Render

  , renderMicroPrint

  -- * Re-exports from DrawMonad
  , MicroPrint
  , Tile(..)
  , Height
  , linebreak
  , setRGB
  , char
  , space

  ) where

import Wumpus.Core

import Wumpus.MicroPrint.DrawMonad
import Wumpus.MicroPrint.Render
-- Note is is permissible to change colour mid-word...

-- | Build a picture from a 
renderMicroPrint :: MP_config -> MicroPrint a -> Maybe DPicture
renderMicroPrint cfg mf = drawMicroPrint cfg $ execMicroPrint mf
