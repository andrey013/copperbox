{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Drawing.Text.SafeFonts
-- Copyright   :  (c) Stephen Tetley 2009-2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Safe to use \"Core 13\" fonts that are expected to be present
-- for any PostScript interpreter.
--
-- Note - regrettably Symbol is not safe to use for SVG.
--
-- \*\* WARNING \*\* - this module is in flux due to changes to 
-- Text encoding in Wumpus-Core and adding font metrics to 
-- Wumpus-Basic. The code here is likely to be revised.
--
--------------------------------------------------------------------------------

module Wumpus.Drawing.Text.SafeFonts
  ( 
  -- * Times Roman
    times_roman
  , times_italic
  , times_bold
  , times_bold_italic

  -- * Helvetica
  , helvetica
  , helvetica_oblique
  , helvetica_bold
  , helvetica_bold_oblique

  -- * Courier
  , courier
  , courier_oblique
  , courier_bold
  , courier_bold_oblique

  -- * Symbol
  , symbol

  ) where



import Wumpus.Core
import Wumpus.Core.Text.StandardEncoding
import Wumpus.Core.Text.Symbol

-- Supported fonts are:
--
-- Times-Roman  Times-Italic       Times-Bold      Times-BoldItalic
-- Helvetica    Helvetica-Oblique  Helvetica-Bold  Helvetica-Bold-Oblique
-- Courier      Courier-Oblique    Courier-Bold    Courier-Bold-Oblique
-- Symbol

--------------------------------------------------------------------------------
-- 

-- | Times-Roman
-- 
times_roman :: FontFace
times_roman = 
    FontFace "Times-Roman" "Times New Roman" SVG_REGULAR standard_encoding

-- | Times Italic
--
times_italic :: FontFace
times_italic = 
    FontFace "Times-Italic" "Times New Roman" SVG_ITALIC standard_encoding
                       
-- | Times Bold
--
times_bold :: FontFace
times_bold = 
    FontFace "Times-Bold" "Times New Roman" SVG_BOLD standard_encoding

-- | Times Bold Italic
--
times_bold_italic :: FontFace
times_bold_italic = FontFace "Times-BoldItalic" 
                             "Times New Roman" 
                             SVG_BOLD_ITALIC 
                             standard_encoding


--------------------------------------------------------------------------------
-- Helvetica

-- | Helvetica 
--
helvetica :: FontFace
helvetica = FontFace "Helvetica" "Helvetica" SVG_REGULAR standard_encoding


-- | Helvetica Oblique
--
helvetica_oblique :: FontFace
helvetica_oblique = 
    FontFace "Helvetica-Oblique" "Helvetica" SVG_OBLIQUE standard_encoding

-- | Helvetica Bold
-- 
helvetica_bold :: FontFace
helvetica_bold = 
    FontFace "Helvetica-Bold" "Helvetica" SVG_BOLD standard_encoding


-- | Helvetica Bold Oblique
--
helvetica_bold_oblique :: FontFace
helvetica_bold_oblique = FontFace "Helvetica-Bold-Oblique" 
                                  "Helvetica" 
                                  SVG_BOLD_OBLIQUE 
                                  standard_encoding



--------------------------------------------------------------------------------

-- | Courier
-- 
courier :: FontFace
courier = FontFace "Courier" "Courier New" SVG_REGULAR standard_encoding

-- | Courier Oblique
-- 
courier_oblique :: FontFace
courier_oblique = 
    FontFace "Courier-Oblique" "Courier New" SVG_OBLIQUE standard_encoding

-- | Courier Bold
-- 
courier_bold :: FontFace
courier_bold = 
    FontFace "Courier-Bold" "Courier New" SVG_BOLD standard_encoding


-- | Courier Bold Oblique
-- 
courier_bold_oblique :: FontFace
courier_bold_oblique = FontFace "Courier-Bold-Oblique" 
                                "Courier New" 
                                SVG_BOLD_OBLIQUE 
                                standard_encoding

--------------------------------------------------------------------------------
-- Symbol

-- | Symbol
--
-- Note - Symbol is intentionally not supported for SVG by some 
-- renderers (Firefox). Chrome is fine, but the use of symbol 
-- should be still be avoided for web graphics.
-- 
symbol :: FontFace
symbol = FontFace "Symbol" "Symbol" SVG_REGULAR symbol_encoding





