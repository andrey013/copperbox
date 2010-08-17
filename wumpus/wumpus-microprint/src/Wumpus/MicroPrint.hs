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

  -- * Top level rendering functions
  , renderMicroPrint
  , renderMicroPrintU

  -- * Re-export some from MicroPrint.DrawMonad
  , MicroPrint
  , Tile(..)
  , Height
  , linebreak
  , setRGB
  , char
  , space

  ) where


import Wumpus.MicroPrint.DrawMonad
import Wumpus.MicroPrint.Render

import Wumpus.Core                              -- package: wumpus-core

import Data.Maybe


-- | Build a picture from a MicroPrint.
--
-- This function returns Nothing if the picture is empty.
-- 
renderMicroPrint :: MicroPrintConfig -> MicroPrint a -> Maybe DPicture
renderMicroPrint cfg mf = drawMicroPrint cfg $ execMicroPrint mf


-- | Build a picture from a MicroPrint - /unsafe/ version.
--
-- This function throws a runtime error if the picture is empty.
-- 
renderMicroPrintU :: MicroPrintConfig -> MicroPrint a -> DPicture
renderMicroPrintU cfg mf = fromMaybe errK $ renderMicroPrint cfg mf
  where
    errK = error "renderMicroPrintU - empty picture."
