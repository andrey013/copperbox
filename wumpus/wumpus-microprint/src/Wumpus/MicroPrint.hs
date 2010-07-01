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

  -- * Re-export all MicroPrint.Render        
    module Wumpus.MicroPrint.Render

  -- * Top level rendering function
  , renderMicroPrint

  -- * Re-export some from MicroPrint.DrawMonad
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



-- | Build a picture from a MicroPrint.
--
renderMicroPrint :: MP_config -> MicroPrint a -> Maybe DPicture
renderMicroPrint cfg mf = drawMicroPrint cfg $ execMicroPrint mf
