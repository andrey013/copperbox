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
  -- $symboldoc
  , symbol

  ) where



import Wumpus.Core

{-

 Times-Roman  Times-Italic  Times-Bold  Times-BoldItalic
 Helvetica  Helvetica-Oblique  Helvetica-Bold  Helvetica-Bold-Oblique
 Courier  Courier-Oblique  Courier-Bold  Courier-Bold-Oblique
 Symbol

-}
--------------------------------------------------------------------------------
-- Times-Roman

timesRoman :: FontFace
timesRoman = FontFace "Times-Roman" "Times New Roman" SVG_REGULAR

-- Times Italic

timesItalic :: FontFace
timesItalic = FontFace "Times-Italic" "Times New Roman" SVG_ITALIC

-- Times Bold

timesBold :: FontFace
timesBold = FontFace "Times-Bold" "Times New Roman" SVG_BOLD

-- Times Bold Italic

timesBoldItalic :: FontFace
timesBoldItalic = 
    FontFace "Times-BoldItalic" "Times New Roman" SVG_BOLD_ITALIC


--------------------------------------------------------------------------------
-- Helvetica

helvetica :: FontFace
helvetica = FontFace "Helvetica" "Helvetica" SVG_REGULAR


-- Helvetica Oblique

helveticaOblique :: FontFace
helveticaOblique = FontFace "Helvetica-Oblique" "Helvetica" SVG_OBLIQUE

-- Helvetica Bold

helveticaBold :: FontFace
helveticaBold = FontFace "Helvetica-Bold" "Helvetica" SVG_BOLD


-- Helvetica Bold Oblique

helveticaBoldOblique :: FontFace
helveticaBoldOblique = 
    FontFace "Helvetica-Bold-Oblique" "Helvetica" SVG_BOLD_OBLIQUE



--------------------------------------------------------------------------------
-- Courier

courier :: FontFace
courier = FontFace "Courier" "Courier New" SVG_REGULAR

-- Courier Oblique

courierOblique :: FontFace
courierOblique = FontFace "Courier-Oblique" "Courier New" SVG_OBLIQUE

-- Courier Bold

courierBold :: FontFace
courierBold = FontFace "Courier-Bold" "Courier New" SVG_BOLD


-- Courier Bold Oblique

courierBoldOblique :: FontFace
courierBoldOblique = 
    FontFace "Courier-Bold-Oblique" "Courier New" SVG_BOLD_OBLIQUE

--------------------------------------------------------------------------------
-- Symbol

-- $symboldoc
-- Symbol does not appear to be well supported by SVG.
-- It renders in Chrome but not in Firefox.

symbol :: FontFace
symbol = FontFace "Symbol" "Symbol" SVG_REGULAR





