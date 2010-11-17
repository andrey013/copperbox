{-# OPTIONS -Wall #-}

> ------------------------------------------------------------------------------
> -- | 
> -- Module       :   Wumpus.Core.Text.Latin1Encoding
> -- Copyright    :   (c) Stephen Tetley 2010
> -- License      :   BSD3
> -- 
> -- Maintainer   :   Stephen Tetley <stephen.tetley@gmail.com>
> -- Stability    :   unstable
> -- Portability  :   GHC
> -- 
> -- Encoding vector for the Latin1 Encoding.
> -- 
> -- \*\* This file is auto-generated. \*\*
> -- 
> -- Generated - 2010-11-17 15:24
> -- 
> ------------------------------------------------------------------------------

> module Wumpus.Core.Text.Latin1Encoding
>   (
>      latin1_encoding
>   ) where

> import Wumpus.Core.Text.Base

> import qualified Data.IntMap as IntMap


> -- | Table mapping character numbers to glyph names for the
> -- Latin1 Encoding.

> latin1_encoding :: EncodingVector
> latin1_encoding = IntMap.fromAscList $
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
>     , ( 0x002d, "minus" )
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
>     , ( 0x0090, "dotlessi" )
>     , ( 0x0091, "grave" )
>     , ( 0x0093, "circumflex" )
>     , ( 0x0094, "tilde" )
>     , ( 0x0096, "breve" )
>     , ( 0x0097, "dotaccent" )
>     , ( 0x009a, "ring" )
>     , ( 0x009d, "hungarumlaut" )
>     , ( 0x009e, "ogonek" )
>     , ( 0x009f, "caron" )
>     , ( 0x00a1, "exclamdown" )
>     , ( 0x00a2, "cent" )
>     , ( 0x00a3, "sterling" )
>     , ( 0x00a4, "currency" )
>     , ( 0x00a5, "yen" )
>     , ( 0x00a6, "brokenbar" )
>     , ( 0x00a7, "section" )
>     , ( 0x00a8, "dieresis" )
>     , ( 0x00a9, "copyright" )
>     , ( 0x00aa, "ordfeminine" )
>     , ( 0x00ab, "guillemotleft" )
>     , ( 0x00ac, "logicalnot" )
>     , ( 0x00ad, "hyphen" )
>     , ( 0x00ae, "registered" )
>     , ( 0x00af, "macron" )
>     , ( 0x00b0, "degree" )
>     , ( 0x00b1, "plusminus" )
>     , ( 0x00b2, "twosuperior" )
>     , ( 0x00b3, "threesuperior" )
>     , ( 0x00b4, "acute2" )
>     , ( 0x00b5, "mu" )
>     , ( 0x00b6, "paragraph" )
>     , ( 0x00b7, "periodcentered" )
>     , ( 0x00b8, "cedilla" )
>     , ( 0x00b9, "onesuperior" )
>     , ( 0x00ba, "ordmasculine" )
>     , ( 0x00bb, "guillemotright" )
>     , ( 0x00bc, "onequarter" )
>     , ( 0x00bd, "onehalf" )
>     , ( 0x00be, "threequarters" )
>     , ( 0x00bf, "questiondown" )
>     , ( 0x00c0, "Agrave" )
>     , ( 0x00c1, "Aacute" )
>     , ( 0x00c2, "Acircumflex" )
>     , ( 0x00c3, "Atilde" )
>     , ( 0x00c4, "Adieresis" )
>     , ( 0x00c5, "Aring" )
>     , ( 0x00c6, "AE" )
>     , ( 0x00c7, "Ccedilla" )
>     , ( 0x00c8, "Egrave" )
>     , ( 0x00c9, "Eacute" )
>     , ( 0x00ca, "Ecircumflex" )
>     , ( 0x00cb, "Edieresis" )
>     , ( 0x00cc, "Igrave" )
>     , ( 0x00cd, "Iacute" )
>     , ( 0x00ce, "Icircumflex" )
>     , ( 0x00cf, "Idieresis" )
>     , ( 0x00d0, "Eth" )
>     , ( 0x00d1, "Ntilde" )
>     , ( 0x00d2, "Ograve" )
>     , ( 0x00d3, "Oacute" )
>     , ( 0x00d4, "Ocircumflex" )
>     , ( 0x00d5, "Otilde" )
>     , ( 0x00d6, "Odieresis" )
>     , ( 0x00d7, "multiply" )
>     , ( 0x00d9, "Ugrave" )
>     , ( 0x00da, "Uacute" )
>     , ( 0x00db, "Ucircumflex" )
>     , ( 0x00dc, "Udieresis" )
>     , ( 0x00dd, "Yacute" )
>     , ( 0x00de, "Thorn" )
>     , ( 0x00df, "germandbls" )
>     , ( 0x00e0, "agrave" )
>     , ( 0x00e1, "aacute" )
>     , ( 0x00e2, "acircumflex" )
>     , ( 0x00e3, "atilde" )
>     , ( 0x00e4, "adieresis" )
>     , ( 0x00e5, "aring" )
>     , ( 0x00e6, "ae" )
>     , ( 0x00e7, "ccedilla" )
>     , ( 0x00e8, "egrave" )
>     , ( 0x00e9, "Oslash" )
>     , ( 0x00ea, "ecircumflex" )
>     , ( 0x00eb, "edieresis" )
>     , ( 0x00ec, "igrave" )
>     , ( 0x00ed, "iacute" )
>     , ( 0x00ee, "icircumflex" )
>     , ( 0x00ef, "idieresis" )
>     , ( 0x00f0, "eth" )
>     , ( 0x00f1, "ntilde" )
>     , ( 0x00f2, "ograve" )
>     , ( 0x00f3, "oacute" )
>     , ( 0x00f4, "ocircumflex" )
>     , ( 0x00f5, "otilde" )
>     , ( 0x00f6, "odieresis" )
>     , ( 0x00f7, "divide" )
>     , ( 0x00f8, "oslash" )
>     , ( 0x00f9, "ugrave" )
>     , ( 0x00fa, "uacute" )
>     , ( 0x00fb, "ucircumflex" )
>     , ( 0x00fc, "udieresis" )
>     , ( 0x00fd, "yacute" )
>     , ( 0x00fe, "thorn" )
>     , ( 0x00ff, "ydieresis" )
>     ]
