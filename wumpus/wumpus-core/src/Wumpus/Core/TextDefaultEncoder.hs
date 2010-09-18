{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Core.TextDefaultEncoder
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- This is the default Text encoder with lookups for both Latin1
-- and Symbol font.
--
--------------------------------------------------------------------------------

module Wumpus.Core.TextDefaultEncoder
  ( 
    defaultEncoder

  ) where

import Wumpus.Core.TextEncoder
import Wumpus.Core.TextLatin1
import Wumpus.Core.TextSymbolFont

import Data.Map


-- | This is the default encoder supporting names and codes for 
-- both Latin1 and the Symbol font.
--
defaultEncoder :: TextEncoder
defaultEncoder = TextEncoder
      { svg_encoding_name       = "ISO-8859-1"
      , default_encoder_name    = latin1_font_encoder
      , font_encoder_map        = fem
      }
  where
    fem = insert symbol_font_encoder symbolFontEncoder 
        $ insert latin1_font_encoder latin1FontEncoder
        $ empty 


