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

timesRoman :: Int -> FontAttr
timesRoman = FontAttr "Times-Roman" "Times New Roman" SVG_REGULAR

-- Times Italic

timesItalic :: Int -> FontAttr
timesItalic = FontAttr "Times-Italic" "Times New Roman" SVG_ITALIC

-- Times Bold

timesBold :: Int -> FontAttr
timesBold = FontAttr "Times-Bold" "Times New Roman" SVG_BOLD

-- Times Bold Italic

timesBoldItalic :: Int -> FontAttr
timesBoldItalic = 
    FontAttr "Times-BoldItalic" "Times New Roman" SVG_BOLD_ITALIC


--------------------------------------------------------------------------------
-- Helvetica

helvetica :: Int -> FontAttr
helvetica = FontAttr "Helvetica" "Helvetica" SVG_REGULAR


-- Helvetica Oblique

helveticaOblique :: Int -> FontAttr
helveticaOblique = FontAttr "Helvetica-Oblique" "Helvetica" SVG_OBLIQUE

-- Helvetica Bold

helveticaBold :: Int -> FontAttr
helveticaBold = FontAttr "Helvetica-Bold" "Helvetica" SVG_BOLD


-- Helvetica Bold Oblique

helveticaBoldOblique :: Int -> FontAttr
helveticaBoldOblique = 
    FontAttr "Helvetica-Bold-Oblique" "Helvetica" SVG_BOLD_OBLIQUE



--------------------------------------------------------------------------------
-- Courier

courier :: Int -> FontAttr
courier = FontAttr "Courier" "Courier New" SVG_REGULAR

-- Courier Oblique

courierOblique :: Int -> FontAttr
courierOblique = FontAttr "Courier-Oblique" "Courier New" SVG_OBLIQUE

-- Courier Bold

courierBold :: Int -> FontAttr
courierBold = FontAttr "Courier-Bold" "Courier New" SVG_BOLD


-- Courier Bold Oblique

courierBoldOblique :: Int -> FontAttr
courierBoldOblique = 
    FontAttr "Courier-Bold-Oblique" "Courier New" SVG_BOLD_OBLIQUE

--------------------------------------------------------------------------------
-- Symbol

-- $symboldoc
-- Symbol does not appear to be well supported by SVG.
-- It renders in Chrome but not in Firefox.

symbol :: Int -> FontAttr
symbol = FontAttr "Symbol" "Symbol" SVG_REGULAR





