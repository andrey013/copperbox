{-# OPTIONS -Wall #-}

> ------------------------------------------------------------------------------
> -- | 
> -- Module       :   Wumpus.Core.Text.StandardEncoding
> -- Copyright    :   (c) Stephen Tetley 2010
> -- License      :   BSD3
> -- 
> -- Maintainer   :   Stephen Tetley <stephen.tetley@gmail.com>
> -- Stability    :   unstable
> -- Portability  :   GHC
> -- 
> -- Encoding vector for the Standard Encoding.
> -- 
> -- \*\* This file is auto-generated. \*\*
> -- 
> -- Generated - 2010-11-13 18:20
> -- 
> ------------------------------------------------------------------------------

> module Wumpus.Core.Text.StandardEncoding
>   (
>      standard_encoding
>   ) where

> import Wumpus.Core.Text.Base

> import qualified Data.IntMap as IntMap


> -- | Table mapping character numbers to Glyph names for the
> -- Standard Encoding.

> standard_encoding :: IntMap.IntMap String
> standard_encoding = IntMap.fromAscList $
>     [ ( 0x0020, "space" )
>     , ( 0x0021, "exclam" )
>     , ( 0x0022, "quotedbl" )
>     , ( 0x0023, "numbersign" )
>     , ( 0x0024, "dollar" )
>     , ( 0x0025, "percent" )
>     , ( 0x0026, "ampersand" )
>     , ( 0x0027, "quoteright" )
>     , ( 0x0028, "parenleft" )
>     , ( 0x0029, "parenright" )
>     , ( 0x002a, "asterisk" )
>     , ( 0x002b, "plus" )
>     , ( 0x002c, "comma" )
>     , ( 0x002d, "hyphen" )
>     , ( 0x002e, "period" )
>     , ( 0x002f, "slash" )
>     , ( 0x0030, "zero" )
>     , ( 0x0031, "one" )
>     , ( 0x0032, "two" )
>     , ( 0x0033, "three" )
>     , ( 0x0034, "four" )
>     , ( 0x0035, "five" )
>     , ( 0x0036, "six" )
>     , ( 0x0037, "seven" )
>     , ( 0x0038, "eight" )
>     , ( 0x0039, "nine" )
>     , ( 0x003a, "colon" )
>     , ( 0x003b, "semicolon" )
>     , ( 0x003c, "less" )
>     , ( 0x003d, "equal" )
>     , ( 0x003e, "greater" )
>     , ( 0x003f, "question" )
>     , ( 0x0040, "at" )
>     , ( 0x0041, "A" )
>     , ( 0x0042, "B" )
>     , ( 0x0043, "C" )
>     , ( 0x0044, "D" )
>     , ( 0x0045, "E" )
>     , ( 0x0046, "F" )
>     , ( 0x0047, "G" )
>     , ( 0x0048, "H" )
>     , ( 0x0049, "I" )
>     , ( 0x004a, "J" )
>     , ( 0x004b, "K" )
>     , ( 0x004c, "L" )
>     , ( 0x004d, "M" )
>     , ( 0x004e, "N" )
>     , ( 0x004f, "O" )
>     , ( 0x0050, "P" )
>     , ( 0x0051, "Q" )
>     , ( 0x0052, "R" )
>     , ( 0x0053, "S" )
>     , ( 0x0054, "T" )
>     , ( 0x0055, "U" )
>     , ( 0x0056, "V" )
>     , ( 0x0057, "W" )
>     , ( 0x0058, "X" )
>     , ( 0x0059, "Y" )
>     , ( 0x005a, "Z" )
>     , ( 0x005b, "bracketleft" )
>     , ( 0x005c, "backslash" )
>     , ( 0x005d, "bracketright" )
>     , ( 0x005e, "asciicircum" )
>     , ( 0x005f, "underscore" )
>     , ( 0x0060, "quoteleft" )
>     , ( 0x0061, "a" )
>     , ( 0x0062, "b" )
>     , ( 0x0063, "c" )
>     , ( 0x0064, "d" )
>     , ( 0x0065, "e" )
>     , ( 0x0066, "f" )
>     , ( 0x0067, "g" )
>     , ( 0x0068, "h" )
>     , ( 0x0069, "i" )
>     , ( 0x006a, "j" )
>     , ( 0x006b, "k" )
>     , ( 0x006c, "l" )
>     , ( 0x006d, "m" )
>     , ( 0x006e, "n" )
>     , ( 0x006f, "o" )
>     , ( 0x0070, "p" )
>     , ( 0x0071, "q" )
>     , ( 0x0072, "r" )
>     , ( 0x0073, "s" )
>     , ( 0x0074, "t" )
>     , ( 0x0075, "u" )
>     , ( 0x0076, "v" )
>     , ( 0x0077, "w" )
>     , ( 0x0078, "x" )
>     , ( 0x0079, "y" )
>     , ( 0x007a, "z" )
>     , ( 0x007b, "braceleft" )
>     , ( 0x007c, "bar" )
>     , ( 0x007d, "braceright" )
>     , ( 0x007e, "asciitilde" )
>     , ( 0x00a1, "exclamdown" )
>     , ( 0x00a2, "cent" )
>     , ( 0x00a3, "sterling" )
>     , ( 0x00a4, "fraction" )
>     , ( 0x00a5, "yen" )
>     , ( 0x00a6, "florin" )
>     , ( 0x00a7, "section" )
>     , ( 0x00a8, "currency" )
>     , ( 0x00a9, "quotesingle" )
>     , ( 0x00aa, "quotedblleft" )
>     , ( 0x00ab, "guillemotleft" )
>     , ( 0x00ac, "guilsinglleft" )
>     , ( 0x00ad, "guilsinglright" )
>     , ( 0x00ae, "fi" )
>     , ( 0x00af, "fl" )
>     , ( 0x00b1, "endash" )
>     , ( 0x00b2, "dagger" )
>     , ( 0x00b3, "daggerdbl" )
>     , ( 0x00b4, "periodcentered" )
>     , ( 0x00b6, "paragraph" )
>     , ( 0x00b7, "bullet" )
>     , ( 0x00b8, "quotesinglbase" )
>     , ( 0x00b9, "quotedblbase" )
>     , ( 0x00ba, "quotedblright" )
>     , ( 0x00bb, "guillemotright" )
>     , ( 0x00bc, "ellipsis" )
>     , ( 0x00bd, "perthousand" )
>     , ( 0x00bf, "questiondown" )
>     , ( 0x00c1, "grave" )
>     , ( 0x00c2, "acute" )
>     , ( 0x00c3, "circumflex" )
>     , ( 0x00c4, "tilde" )
>     , ( 0x00c5, "macron" )
>     , ( 0x00c6, "breve" )
>     , ( 0x00c7, "dotaccent" )
>     , ( 0x00c8, "dieresis" )
>     , ( 0x00ca, "ring" )
>     , ( 0x00cb, "cedilla" )
>     , ( 0x00cd, "hungarumlaut" )
>     , ( 0x00ce, "ogonek" )
>     , ( 0x00cf, "caron" )
>     , ( 0x00d0, "emdash" )
>     , ( 0x00e1, "AE" )
>     , ( 0x00e3, "ordfeminine" )
>     , ( 0x00e8, "Lslash" )
>     , ( 0x00e9, "Oslash" )
>     , ( 0x00ea, "OE" )
>     , ( 0x00eb, "ordmasculine" )
>     , ( 0x00f1, "ae" )
>     , ( 0x00f5, "dotlessi" )
>     , ( 0x00f8, "lslash" )
>     , ( 0x00f9, "oslash" )
>     , ( 0x00fa, "oe" )
>     , ( 0x00fb, "germandbls" )
>     ]
