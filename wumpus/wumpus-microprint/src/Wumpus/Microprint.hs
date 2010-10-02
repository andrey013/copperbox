{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Microprint
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- MicroPrints
--
-- \*\* WARNING \*\* - This module is out-of-date and is due a 
-- rethink. Teletype is no longer the recommended drawing style.
--
--------------------------------------------------------------------------------

module Wumpus.Microprint
  (

  -- * Re-export all Microprint.Render        
    module Wumpus.Microprint.Render

  -- * Top level rendering functions
  , renderTeletype
  , renderTeletypeU

  -- * Re-export some from MicroPrint.DrawMonad
  , Teletype
  , Tile(..)
  , Height
  , linebreak
  , setRGB
  , char
  , space

  ) where


import Wumpus.Microprint.Teletype
import Wumpus.Microprint.Render

import Wumpus.Core                              -- package: wumpus-core
import Wumpus.Basic.Graphic			-- package: wumpus-basic

import Data.Maybe



-- | Build a picture from a Teletype drawing.
--
-- This function returns Nothing if the picture is empty.
-- 
renderTeletype :: RenderScalingCtx -> DrawWordF -> Teletype a -> Maybe DPicture
renderTeletype ctx fn mf = 
    liftToPictureMb $ execDrawing (standardContext 14) 
                    $ render ctx fn $ execTeletype mf


-- | Build a picture from a Teletype - /unsafe/ version.
--
-- This function throws a runtime error if the picture is empty.
-- 
renderTeletypeU :: RenderScalingCtx -> DrawWordF -> Teletype a -> DPicture
renderTeletypeU ctx fn mf = fromMaybe errK $ renderTeletype ctx fn mf
  where
    errK = error "renderTeletypeU - empty picture."

 