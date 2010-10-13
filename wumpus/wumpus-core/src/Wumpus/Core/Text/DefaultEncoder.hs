{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Core.Text.DefaultEncoder
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- This is the default @TextEncoder@ with lookups for both Latin1
-- character codes / names and Symbol font.
--
--------------------------------------------------------------------------------

module Wumpus.Core.Text.DefaultEncoder
  ( 
    defaultEncoder

  ) where

import Wumpus.Core.Text.Encoder
import Wumpus.Core.Text.Latin1
import Wumpus.Core.Text.SymbolFont

import Data.Map


-- | This is the default encoder supporting names and codes for 
-- both Latin1 and the Symbol font.
--
defaultEncoder :: TextEncoder
defaultEncoder = TextEncoder
      { svg_encoding_name       = "ISO-8859-1"
      , font_encoder_map        = fem
      }
  where
    fem = insert symbol_font_encoder symbolFontEncoder 
        $ insert latin1_font_encoder latin1FontEncoder
        $ empty 


