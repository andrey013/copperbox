{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.SafeFonts
-- Copyright   :  (c) Stephen Tetley 2009-2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Safe to use fonts.
--
--
--------------------------------------------------------------------------------

module Wumpus.Basic.SafeFonts
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
    FontFace "Times-Roman" "Times New Roman" SVG_REGULAR latin1_font_encoder

-- | Times Italic
--
times_italic :: FontFace
times_italic = 
    FontFace "Times-Italic" "Times New Roman" SVG_ITALIC latin1_font_encoder
                       
-- | Times Bold
--
times_bold :: FontFace
times_bold = 
    FontFace "Times-Bold" "Times New Roman" SVG_BOLD latin1_font_encoder

-- | Times Bold Italic
--
times_bold_italic :: FontFace
times_bold_italic = FontFace "Times-BoldItalic" 
                             "Times New Roman" 
                             SVG_BOLD_ITALIC 
                             latin1_font_encoder


--------------------------------------------------------------------------------
-- Helvetica

-- | Helvetica 
--
helvetica :: FontFace
helvetica = FontFace "Helvetica" "Helvetica" SVG_REGULAR latin1_font_encoder


-- | Helvetica Oblique
--
helvetica_oblique :: FontFace
helvetica_oblique = 
    FontFace "Helvetica-Oblique" "Helvetica" SVG_OBLIQUE latin1_font_encoder

-- | Helvetica Bold
-- 
helvetica_bold :: FontFace
helvetica_bold = 
    FontFace "Helvetica-Bold" "Helvetica" SVG_BOLD latin1_font_encoder


-- | Helvetica Bold Oblique
--
helvetica_bold_oblique :: FontFace
helvetica_bold_oblique = FontFace "Helvetica-Bold-Oblique" 
                                  "Helvetica" 
                                  SVG_BOLD_OBLIQUE 
                                  latin1_font_encoder



--------------------------------------------------------------------------------

-- | Courier
-- 
courier :: FontFace
courier = FontFace "Courier" "Courier New" SVG_REGULAR latin1_font_encoder

-- | Courier Oblique
-- 
courier_oblique :: FontFace
courier_oblique = 
    FontFace "Courier-Oblique" "Courier New" SVG_OBLIQUE latin1_font_encoder

-- | Courier Bold
-- 
courier_bold :: FontFace
courier_bold = 
    FontFace "Courier-Bold" "Courier New" SVG_BOLD latin1_font_encoder


-- | Courier Bold Oblique
-- 
courier_bold_oblique :: FontFace
courier_bold_oblique = FontFace "Courier-Bold-Oblique" 
                                "Courier New" 
                                SVG_BOLD_OBLIQUE 
                                latin1_font_encoder

--------------------------------------------------------------------------------
-- Symbol

-- | Symbol
--
-- Note - Symbol does not appear to be well supported by some SVG
-- renders. Seemingly Chrome is fine but Firefox defaults to some
-- serif font.
-- 
symbol :: FontFace
symbol = FontFace "Symbol" "Symbol" SVG_REGULAR symbol_font_encoder





