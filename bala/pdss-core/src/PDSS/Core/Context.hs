{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  PDSS.Core.Context
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Equivalent to DrawingContext in Wumpus-Basic.
--
--------------------------------------------------------------------------------


module PDSS.Core.Context
  ( 
    PdContext
  , PdContextF

  , standard_context

  ) where 

import PDSS.Core.Colour
import PDSS.Core.InternalTypes

data PdContext = PdContext
    { ctx_font          :: Font
    , ctx_font_size     :: Int
    , ctx_bg_colour     :: RGBi
    , ctx_fg_colour     :: RGBi
    , ctx_lbl_colour    :: RGBi
    }


type PdContextF = PdContext -> PdContext

standard_context :: PdContext
standard_context = PdContext
    { ctx_font          = COURIER
    , ctx_font_size     = 12
    , ctx_bg_colour     = white
    , ctx_fg_colour     = black
    , ctx_lbl_colour    = black
    }