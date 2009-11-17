{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Extra.SafeFonts
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  experimental
-- Portability :  GHC only
--
-- Safe to use font / size combinations.
--
-- Consider drawing then uniform-scaling for other sizes in 
-- PostScript.
--
--------------------------------------------------------------------------------

module Wumpus.Extra.SafeFonts
  ( 
  -- * Times Roman
    timesRoman10
  , timesRoman12
  , timesRoman18
  , timesRoman24
  , timesRoman48

  , timesItalic10
  , timesItalic12
  , timesItalic18
  , timesItalic24
  , timesItalic48

  , timesBold10
  , timesBold12
  , timesBold18
  , timesBold24
  , timesBold48

  , timesBoldItalic10
  , timesBoldItalic12
  , timesBoldItalic18
  , timesBoldItalic24
  , timesBoldItalic48

  -- * Helvetica
  , helvetica10
  , helvetica12
  , helvetica18
  , helvetica24
  , helvetica48

  , helveticaOblique10
  , helveticaOblique12
  , helveticaOblique18
  , helveticaOblique24
  , helveticaOblique48

  , helveticaBold10
  , helveticaBold12
  , helveticaBold18
  , helveticaBold24
  , helveticaBold48

  , helveticaBoldOblique10
  , helveticaBoldOblique12
  , helveticaBoldOblique18
  , helveticaBoldOblique24
  , helveticaBoldOblique48

  -- * Courier
  , courier10
  , courier12
  , courier18
  , courier24
  , courier48

  , courierOblique10
  , courierOblique12
  , courierOblique18
  , courierOblique24
  , courierOblique48

  , courierBold10
  , courierBold12
  , courierBold18
  , courierBold24
  , courierBold48

  , courierBoldOblique10
  , courierBoldOblique12
  , courierBoldOblique18
  , courierBoldOblique24
  , courierBoldOblique48

  -- * Symbol
  -- $symboldoc
  , symbol10
  , symbol12
  , symbol18
  , symbol24
  , symbol48

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

mkTimesRoman :: Int -> FontAttr
mkTimesRoman = FontAttr "Times-Roman" "Times New Roman" SVG_REGULAR

timesRoman10 :: FontAttr
timesRoman10 = mkTimesRoman 10

timesRoman12 :: FontAttr
timesRoman12 = mkTimesRoman 12

timesRoman18 :: FontAttr
timesRoman18 = mkTimesRoman 18

timesRoman24 :: FontAttr
timesRoman24 = mkTimesRoman 24

timesRoman48 :: FontAttr
timesRoman48 = mkTimesRoman 48

-- Times Italic

mkTimesItalic :: Int -> FontAttr
mkTimesItalic = FontAttr "Times-Italic" "Times New Roman" SVG_ITALIC

timesItalic10 :: FontAttr
timesItalic10 = mkTimesItalic 10

timesItalic12 :: FontAttr
timesItalic12 = mkTimesItalic 12

timesItalic18 :: FontAttr
timesItalic18 = mkTimesItalic 18

timesItalic24 :: FontAttr
timesItalic24 = mkTimesItalic 24

timesItalic48 :: FontAttr
timesItalic48 = mkTimesItalic 48

-- Times Bold

mkTimesBold :: Int -> FontAttr
mkTimesBold = FontAttr "Times-Bold" "Times New Roman" SVG_BOLD

timesBold10 :: FontAttr
timesBold10 = mkTimesBold 10

timesBold12 :: FontAttr
timesBold12 = mkTimesBold 12

timesBold18 :: FontAttr
timesBold18 = mkTimesBold 18

timesBold24 :: FontAttr
timesBold24 = mkTimesBold 24

timesBold48 :: FontAttr
timesBold48 = mkTimesBold 48

-- Times Bold Italic

mkTimesBoldItalic :: Int -> FontAttr
mkTimesBoldItalic = 
    FontAttr "Times-BoldItalic" "Times New Roman" SVG_BOLD_ITALIC

timesBoldItalic10 :: FontAttr
timesBoldItalic10 = mkTimesBoldItalic 10

timesBoldItalic12 :: FontAttr
timesBoldItalic12 = mkTimesBoldItalic 12

timesBoldItalic18 :: FontAttr
timesBoldItalic18 = mkTimesBoldItalic 18

timesBoldItalic24 :: FontAttr
timesBoldItalic24 = mkTimesBoldItalic 24

timesBoldItalic48 :: FontAttr
timesBoldItalic48 = mkTimesBoldItalic 48


--------------------------------------------------------------------------------
-- Helvetica

mkHelvetica :: Int -> FontAttr
mkHelvetica = FontAttr "Helvetica" "Helvetica" SVG_REGULAR


helvetica10 :: FontAttr
helvetica10 = mkHelvetica 10

helvetica12 :: FontAttr
helvetica12 = mkHelvetica 12

helvetica18 :: FontAttr
helvetica18 = mkHelvetica 18

helvetica24 :: FontAttr
helvetica24 = mkHelvetica 24

helvetica48 :: FontAttr
helvetica48 = mkHelvetica 48

-- Helvetica Oblique

mkHelveticaOblique :: Int -> FontAttr
mkHelveticaOblique = FontAttr "Helvetica-Oblique" "Helvetica" SVG_OBLIQUE


helveticaOblique10 :: FontAttr
helveticaOblique10 = mkHelveticaOblique 10

helveticaOblique12 :: FontAttr
helveticaOblique12 = mkHelveticaOblique 12

helveticaOblique18 :: FontAttr
helveticaOblique18 = mkHelveticaOblique 18

helveticaOblique24 :: FontAttr
helveticaOblique24 = mkHelveticaOblique 24

helveticaOblique48 :: FontAttr
helveticaOblique48 = mkHelveticaOblique 48


-- Helvetica Bold

mkHelveticaBold :: Int -> FontAttr
mkHelveticaBold = FontAttr "Helvetica-Bold" "Helvetica" SVG_BOLD


helveticaBold10 :: FontAttr
helveticaBold10 = mkHelveticaBold 10

helveticaBold12 :: FontAttr
helveticaBold12 = mkHelveticaBold 12

helveticaBold18 :: FontAttr
helveticaBold18 = mkHelveticaBold 18

helveticaBold24 :: FontAttr
helveticaBold24 = mkHelveticaBold 24

helveticaBold48 :: FontAttr
helveticaBold48 = mkHelveticaBold 48

-- Helvetica Bold Oblique

mkHelveticaBoldOblique :: Int -> FontAttr
mkHelveticaBoldOblique = 
    FontAttr "Helvetica-Bold-Oblique" "Helvetica" SVG_BOLD_OBLIQUE


helveticaBoldOblique10 :: FontAttr
helveticaBoldOblique10 = mkHelveticaBoldOblique 10

helveticaBoldOblique12 :: FontAttr
helveticaBoldOblique12 = mkHelveticaBoldOblique 12

helveticaBoldOblique18 :: FontAttr
helveticaBoldOblique18 = mkHelveticaBoldOblique 18

helveticaBoldOblique24 :: FontAttr
helveticaBoldOblique24 = mkHelveticaBoldOblique 24

helveticaBoldOblique48 :: FontAttr
helveticaBoldOblique48 = mkHelveticaBoldOblique 48




--------------------------------------------------------------------------------
-- Courier

mkCourier :: Int -> FontAttr
mkCourier = FontAttr "Courier" "Courier New" SVG_REGULAR

courier10 :: FontAttr
courier10 = mkCourier 10

courier12 :: FontAttr
courier12 = mkCourier 12

courier18 :: FontAttr
courier18 = mkCourier 18

courier24 :: FontAttr
courier24 = mkCourier 24

courier48 :: FontAttr
courier48 = mkCourier 48

-- Courier Oblique

mkCourierOblique :: Int -> FontAttr
mkCourierOblique = FontAttr "Courier-Oblique" "Courier New" SVG_OBLIQUE


courierOblique10 :: FontAttr
courierOblique10 = mkCourierOblique 10

courierOblique12 :: FontAttr
courierOblique12 = mkCourierOblique 12

courierOblique18 :: FontAttr
courierOblique18 = mkCourierOblique 18

courierOblique24 :: FontAttr
courierOblique24 = mkCourierOblique 24

courierOblique48 :: FontAttr
courierOblique48 = mkCourierOblique 48

-- Courier Bold

mkCourierBold :: Int -> FontAttr
mkCourierBold = FontAttr "Courier-Bold" "Courier New" SVG_BOLD


courierBold10 :: FontAttr
courierBold10 = mkCourierBold 10

courierBold12 :: FontAttr
courierBold12 = mkCourierBold 12

courierBold18 :: FontAttr
courierBold18 = mkCourierBold 18

courierBold24 :: FontAttr
courierBold24 = mkCourierBold 24

courierBold48 :: FontAttr
courierBold48 = mkCourierBold 48

-- Courier Bold Oblique

mkCourierBoldOblique :: Int -> FontAttr
mkCourierBoldOblique = 
    FontAttr "Courier-Bold-Oblique" "Courier New" SVG_BOLD_OBLIQUE


courierBoldOblique10 :: FontAttr
courierBoldOblique10 = mkCourierBoldOblique 10

courierBoldOblique12 :: FontAttr
courierBoldOblique12 = mkCourierBoldOblique 12

courierBoldOblique18 :: FontAttr
courierBoldOblique18 = mkCourierBoldOblique 18

courierBoldOblique24 :: FontAttr
courierBoldOblique24 = mkCourierBoldOblique 24

courierBoldOblique48 :: FontAttr
courierBoldOblique48 = mkCourierBoldOblique 48

--------------------------------------------------------------------------------
-- Symbol

-- $symboldoc
-- Symbol does not appear to be well supported by SVG.
-- It renders in Chrome but not in Firefox.

mkSymbol :: Int -> FontAttr
mkSymbol = FontAttr "Symbol" "Symbol" SVG_REGULAR


symbol10 :: FontAttr
symbol10 = mkSymbol 10

symbol12 :: FontAttr
symbol12 = mkSymbol 12

symbol18 :: FontAttr
symbol18 = mkSymbol 18

symbol24 :: FontAttr
symbol24 = mkSymbol 24

symbol48 :: FontAttr
symbol48 = mkSymbol 48




