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
    timesRoman
  , timesItalic
  , timesBold
  , timesBoldItalic

  -- * Helvetica
  , helvetica
  , helveticaOblique
  , helveticaBold
  , helveticaBoldOblique

  -- * Courier
  , courier
  , courierOblique
  , courierBold
  , courierBoldOblique

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
timesRoman :: FontFace
timesRoman = 
    FontFace "Times-Roman" "Times New Roman" SVG_REGULAR latin1_font_encoder

-- | Times Italic
--
timesItalic :: FontFace
timesItalic = 
    FontFace "Times-Italic" "Times New Roman" SVG_ITALIC latin1_font_encoder
                       
-- | Times Bold
--
timesBold :: FontFace
timesBold = 
    FontFace "Times-Bold" "Times New Roman" SVG_BOLD latin1_font_encoder

-- | Times Bold Italic
--
timesBoldItalic :: FontFace
timesBoldItalic = FontFace "Times-BoldItalic" 
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
helveticaOblique :: FontFace
helveticaOblique = 
    FontFace "Helvetica-Oblique" "Helvetica" SVG_OBLIQUE latin1_font_encoder

-- | Helvetica Bold
-- 
helveticaBold :: FontFace
helveticaBold = 
    FontFace "Helvetica-Bold" "Helvetica" SVG_BOLD latin1_font_encoder


-- | Helvetica Bold Oblique
--
helveticaBoldOblique :: FontFace
helveticaBoldOblique = FontFace "Helvetica-Bold-Oblique" 
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
courierOblique :: FontFace
courierOblique = 
    FontFace "Courier-Oblique" "Courier New" SVG_OBLIQUE latin1_font_encoder

-- | Courier Bold
-- 
courierBold :: FontFace
courierBold = 
    FontFace "Courier-Bold" "Courier New" SVG_BOLD latin1_font_encoder


-- | Courier Bold Oblique
-- 
courierBoldOblique :: FontFace
courierBoldOblique = FontFace "Courier-Bold-Oblique" 
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





