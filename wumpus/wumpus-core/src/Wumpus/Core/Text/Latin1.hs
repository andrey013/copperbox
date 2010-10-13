{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Core.Text.Latin1
-- Copyright   :  (c) Stephen Tetley 2009-2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- A @TextEncoder@ record instance for Latin1 characters.
-- 
--------------------------------------------------------------------------------

module Wumpus.Core.Text.Latin1
  ( 
  
    latin1_font_encoder
  , latin1FontEncoder
  , latin1FontAll

  ) where

import Wumpus.Core.Text.Encoder

import qualified Data.Map as Map


-- | Latin1 Font Encoder name 
--  
-- (Number 0) 
--
latin1_font_encoder :: FontEncoderName
latin1_font_encoder = FontEncoderName 0

-- | Latin1 FontEncoder instance.
latin1FontEncoder :: FontEncoder
latin1FontEncoder = FontEncoder 
    { ps_lookup         = Map.lookup `flip` codeToName
    , svg_lookup        = Map.lookup `flip` nameToCode
    , ps_fallback       = "space"
    , svg_fallback      = 0o040
    }

nameToCode :: Map.Map GlyphName CharCode
nameToCode = Map.fromList latin1FontAll

codeToName :: Map.Map CharCode GlyphName
codeToName = foldr fn Map.empty latin1FontAll where
  fn (s,i) a = Map.insert i s a 

-- | A lookup list of Latin 1 names to their octal code.
latin1FontAll :: [(GlyphName, CharCode)]
latin1FontAll = 
    [ ("A",                     0o101)
    , ("AE",                    0o306)
    , ("Aacute",                0o301)
    , ("Acircumflex",           0o302)
    , ("Adieresis",             0o304)
    , ("Agrave",                0o300)
    , ("Aring",                 0o305)
    , ("Atilde",                0o303)
    , ("B",                     0o102)
    , ("C",                     0o103)
    , ("Ccedilla",              0o307)
    , ("D",                     0o104)
    , ("E",                     0o105)
    , ("Eacute",                0o311)
    , ("Ecircumflex",           0o312)
    , ("Edieresis",             0o313)
    , ("Egrave",                0o310)
    , ("Eth",                   0o320)
    , ("F",                     0o106)
    , ("G",                     0o107)
    , ("H",                     0o110)
    , ("I",                     0o111)
    , ("Iacute",                0o315)
    , ("Icircumflex",           0o316)
    , ("Idieresis",             0o317)
    , ("Igrave",                0o314)
    , ("J",                     0o112)
    , ("K",                     0o113)
    , ("L",                     0o114)
    , ("M",                     0o115)
    , ("N",                     0o116)
    , ("Ntilde",                0o321)
    , ("O",                     0o117)
    , ("Oacute",                0o323)
    , ("Ocircumflex",           0o324)
    , ("Odieresis",             0o326)
    , ("Ograve",                0o322)
    , ("Oslash",                0o351)
    , ("Otilde",                0o325)
    , ("P",                     0o120)
    , ("Q",                     0o121)
    , ("R",                     0o122)
    , ("S",                     0o123)
    , ("T",                     0o124)
    , ("Thorn",                 0o336)
    , ("U",                     0o125)
    , ("Uacute",                0o332)
    , ("Ucircumflex",           0o333)
    , ("Udieresis",             0o334)
    , ("Ugrave",                0o331)
    , ("V",                     0o126)
    , ("W",                     0o127)
    , ("X",                     0o130)
    , ("Y",                     0o131)
    , ("Yacute",                0o335)
    , ("Z",                     0o132)
    , ("a",                     0o141)
    , ("aacute",                0o341)
    , ("acircumflex",           0o342)
    , ("acute2",                0o264)
    , ("adieresis",             0o344)
    , ("ae",                    0o346)
    , ("agrave",                0o340)
    , ("ampersand",             0o046)
    , ("aring",                 0o345)
    , ("asciicircum",           0o136)
    , ("asciitilde",            0o176)
    , ("asterisk",              0o052)
    , ("at",                    0o100)
    , ("atilde",                0o343)
    , ("b",                     0o142)
    , ("backslash",             0o134)
    , ("bar",                   0o174)
    , ("braceleft",             0o173)
    , ("braceright",            0o175)
    , ("bracketleft",           0o133)
    , ("bracketright",          0o135)
    , ("breve",                 0o226)
    , ("brokenbar",             0o246)
    , ("c",                     0o143)
    , ("caron",                 0o237)
    , ("ccedilla",              0o347)
    , ("cedilla",               0o270)
    , ("cent",                  0o242)
    , ("circumflex",            0o223)
    , ("colon",                 0o072)
    , ("comma",                 0o054)
    , ("copyright",             0o251)
    , ("currency",              0o244)
    , ("d",                     0o144)
    , ("degree",                0o260)
    , ("dieresis",              0o250)
    , ("divide",                0o367)
    , ("dollar",                0o044)
    , ("dotaccent",             0o227)
    , ("dotlessi",              0o220)
    , ("e",                     0o145)
    , ("eacute",                0o351)
    , ("ecircumflex",           0o352)
    , ("edieresis",             0o353)
    , ("egrave",                0o350)
    , ("eight",                 0o070)
    , ("equal",                 0o075)
    , ("eth",                   0o360)
    , ("exclam",                0o041)
    , ("exclamdown",            0o241)
    , ("f",                     0o146)
    , ("five",                  0o065)
    , ("four",                  0o064)
    , ("g",                     0o147)
    , ("germandbls",            0o337)
    , ("grave",                 0o221)
    , ("greater",               0o076)
    , ("guillemotleft",         0o253)
    , ("guillemotright",        0o273)
    , ("h",                     0o150)
    , ("hungarumlaut",          0o235)
    , ("hyphen",                0o255)
    , ("i",                     0o151)
    , ("iacute",                0o355)
    , ("icircumflex",           0o356)
    , ("idieresis",             0o357)
    , ("igrave",                0o354)
    , ("j",                     0o152)
    , ("k",                     0o153)
    , ("l",                     0o154)
    , ("less",                  0o074)
    , ("logicalnot",            0o254)
    , ("m",                     0o155)
    , ("macron",                0o257)
    , ("minus",                 0o055)
    , ("mu",                    0o265)
    , ("multiply",              0o327)
    , ("n",                     0o156)
    , ("nine",                  0o071)
    , ("ntilde",                0o361)
    , ("numbersign",            0o043)
    , ("o",                     0o157)
    , ("oacute",                0o363)
    , ("ocircumflex",           0o364)
    , ("odieresis",             0o366)
    , ("ogonek",                0o236)
    , ("ograve",                0o362)
    , ("one",                   0o061)
    , ("onehalf",               0o275)
    , ("onequarter",            0o274)
    , ("onesuperior",           0o271)
    , ("ordfeminine",           0o252)
    , ("ordmasculine",          0o272)
    , ("oslash",                0o370)
    , ("otilde",                0o365)
    , ("p",                     0o160)
    , ("paragraph",             0o266)
    , ("parenleft",             0o050)
    , ("parenright",            0o051)
    , ("percent",               0o045)
    , ("period",                0o056)
    , ("periodcentered",        0o267)
    , ("plus",                  0o053)
    , ("plusminus",             0o261)
    , ("q",                     0o161)
    , ("question",              0o077)
    , ("questiondown",          0o277)
    , ("quotedbl",              0o042)
    , ("quoteleft",             0o140)
    , ("quoteright",            0o047)
    , ("r",                     0o162)
    , ("registered",            0o256)
    , ("ring",                  0o232)
    , ("s",                     0o163)
    , ("section",               0o247)
    , ("semicolon",             0o073)
    , ("seven",                 0o067)
    , ("six",                   0o066)
    , ("slash",                 0o057)
    , ("space",                 0o040)
    , ("sterling",              0o243)
    , ("t",                     0o164)
    , ("thorn",                 0o376)
    , ("three",                 0o063)
    , ("threequarters",         0o276)
    , ("threesuperior",         0o263)
    , ("tilde",                 0o224)
    , ("two",                   0o062)
    , ("twosuperior",           0o262)
    , ("u",                     0o165)
    , ("uacute",                0o372)
    , ("ucircumflex",           0o373)
    , ("udieresis",             0o374)
    , ("ugrave",                0o371)
    , ("underscore",            0o137)
    , ("v",                     0o166)
    , ("w",                     0o167)
    , ("x",                     0o170)
    , ("y",                     0o171)
    , ("yacute",                0o375)
    , ("ydieresis",             0o377)
    , ("yen",                   0o245)
    , ("z",                     0o172)
    , ("zero",                  0o060)
    ]