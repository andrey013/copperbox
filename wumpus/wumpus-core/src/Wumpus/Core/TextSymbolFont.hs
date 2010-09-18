{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Core.TextSymbolFont
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- A TextEncoder record instance for Symbol font characters.
-- 
--------------------------------------------------------------------------------

module Wumpus.Core.TextSymbolFont
  ( 
  
    symbol_font_encoder
  , symbolFontEncoder
  , symbolFontAll

  ) where

import Wumpus.Core.TextEncoder

import qualified Data.Map as Map



-- | Symbol Font Encoder name 
--
symbol_font_encoder :: FontEncoderName
symbol_font_encoder = FontEncoderName "Symbol_font_encoder"



-- | Latin1 TextEncoder instance.
symbolFontEncoder :: FontEncoder
symbolFontEncoder = FontEncoder 
    { ps_lookup         = Map.lookup `flip` codeToName
    , svg_lookup        = Map.lookup `flip` nameToCode
    , ps_fallback       = "space"
    , svg_fallback      = 0o040
    }

nameToCode :: Map.Map GlyphName CharCode
nameToCode = Map.fromList symbolFontAll

codeToName :: Map.Map CharCode GlyphName
codeToName = foldr fn Map.empty symbolFontAll where
  fn (s,i) a = Map.insert i s a 

-- | A lookup list of Latin 1 names to their octal code.
symbolFontAll :: [(GlyphName, CharCode)]
symbolFontAll = 
    [ ("Alpha",                 0o101)
    , ("Beta",                  0o102)
    , ("Chi",                   0o103)
    , ("Delta",                 0o104)
    , ("Epsilon",               0o105)
    , ("Eta",                   0o110)
    , ("Euro",                  0o240)
    , ("Gamma",                 0o107)
    , ("Ifraktur",              0o301)
    , ("Iota",                  0o111)
    , ("Kappa",                 0o113)
    , ("Lambda",                0o114)
    , ("Mu",                    0o115)
    , ("Nu",                    0o116)
    , ("Omega",                 0o127)
    , ("Omicron",               0o117)
    , ("Phi",                   0o106)
    , ("Pi",                    0o120)
    , ("Psi",                   0o131)
    , ("Rfraktur",              0o302)
    , ("Rho",                   0o122)
    , ("Sigma",                 0o123)
    , ("Tau",                   0o124)
    , ("Theta",                 0o121)
    , ("Upsilon",               0o125)
    , ("Upsilon1",              0o241)
    , ("Xi",                    0o130)
    , ("Zeta",                  0o132)
    , ("aleph",                 0o300)
    , ("alpha",                 0o141)
    , ("ampersand",             0o046)
    , ("angle",                 0o320)
    , ("angleleft",             0o341)
    , ("angleright",            0o361)
    , ("approxequal",           0o273)
    , ("arrowboth",             0o253)
    , ("arrowdblboth",          0o333)
    , ("arrowdbldown",          0o337)
    , ("arrowdblleft",          0o334)
    , ("arrowdblright",         0o336)
    , ("arrowdblup",            0o335)
    , ("arrowdown",             0o257)
    , ("arrowhorizex",          0o276)
    , ("arrowleft",             0o254)
    , ("arrowright",            0o256)
    , ("arrowup",               0o255)
    , ("arrowvertex",           0o275)
    , ("asteriskmath",          0o052)
    , ("bar",                   0o174)
    , ("beta",                  0o142)
    , ("braceleft",             0o173)
    , ("braceright",            0o175)
    , ("bracelefttp",           0o354)
    , ("braceleftmid",          0o355)
    , ("braceleftbt",           0o356)
    , ("bracerighttp",          0o374)
    , ("bracerightmid",         0o375)
    , ("bracerightbt",          0o376)
    , ("braceex",               0o357)
    , ("bracketleft",           0o133)
    , ("bracketright",          0o135)
    , ("bracketlefttp",         0o351)
    , ("bracketleftex",         0o352)
    , ("bracketleftbt",         0o353)
    , ("bracketrighttp",        0o371)
    , ("bracketrightex",        0o372)
    , ("bracketrightbt",        0o373)
    , ("bullet",                0o267)
    , ("carriagereturn",        0o277)
    , ("chi",                   0o143)
    , ("circlemultiply",        0o304)
    , ("circleplus",            0o305)
    , ("club",                  0o247)
    , ("colon",                 0o072)
    , ("comma",                 0o054)
    , ("congruent",             0o100)
    , ("copyrightsans",         0o343)
    , ("copyrightserif",        0o323)
    , ("degree",                0o260)
    , ("delta",                 0o144)
    , ("diamond",               0o250)
    , ("divide",                0o270)
    , ("dotmath",               0o327)
    , ("eight",                 0o070)
    , ("element",               0o316)
    , ("ellipsis",              0o274)
    , ("emptyset",              0o306)
    , ("epsilon",               0o145)
    , ("equal",                 0o075)
    , ("equivalence",           0o272)
    , ("eta",                   0o150)
    , ("exclam",                0o041)
    , ("existential",           0o044)
    , ("five",                  0o065)
    , ("florin",                0o246)
    , ("four",                  0o064)
    , ("fraction",              0o244)
    , ("gamma",                 0o147)
    , ("gradient",              0o321)
    , ("greater",               0o076)
    , ("greaterequal",          0o263)
    , ("heart",                 0o251)
    , ("infinity",              0o245)
    , ("integral",              0o362)
    , ("integraltp",            0o363)
    , ("integralex",            0o364)
    , ("integralbt",            0o365)
    , ("intersection",          0o307)
    , ("iota",                  0o151)
    , ("kappa",                 0o153)
    , ("lambda",                0o154)
    , ("less",                  0o074)
    , ("lessequal",             0o243)
    , ("logicaland",            0o331)
    , ("logicalnot",            0o330)
    , ("logicalor",             0o332)
    , ("lozenge",               0o340)
    , ("minus",                 0o055)
    , ("minute",                0o242)
    , ("mu",                    0o155)
    , ("multiply",              0o264)
    , ("nine",                  0o071)
    , ("notelement",            0o317)
    , ("notequal",              0o271)
    , ("notsubset",             0o313)
    , ("nu",                    0o156)
    , ("numbersign",            0o043)
    , ("omega",                 0o167)
    , ("omega1",                0o166)
    , ("omicron",               0o157)
    , ("one",                   0o061)
    , ("parenleft",             0o050)
    , ("parenright",            0o051)
    , ("parenlefttp",           0o346)
    , ("parenleftex",           0o347)
    , ("parenleftbt",           0o350)
    , ("parenrighttp",          0o366)
    , ("parenrightex",          0o367)
    , ("parenrightbt",          0o370)
    , ("partialdiff",           0o266)
    , ("percent",               0o045)
    , ("period",                0o056)
    , ("perpendicular",         0o136)
    , ("phi",                   0o146)
    , ("phi1",                  0o152)
    , ("pi",                    0o160)
    , ("plus",                  0o053)
    , ("plusminus",             0o261)
    , ("product",               0o325)
    , ("propersubset",          0o314)
    , ("propersuperset",        0o311)
    , ("proportional",          0o265)
    , ("psi",                   0o171)
    , ("question",              0o077)
    , ("radical",               0o326)
    , ("radicalex",             0o140)
    , ("reflexsubset",          0o315)
    , ("reflexsuperset",        0o312)
    , ("registersans",          0o342)
    , ("registerserif",         0o322)
    , ("rho",                   0o162)
    , ("second",                0o262)
    , ("semicolon",             0o073)
    , ("seven",                 0o067)
    , ("sigma",                 0o163)
    , ("sigma1",                0o126)
    , ("similar",               0o176)
    , ("six",                   0o066)
    , ("slash",                 0o057)
    , ("space",                 0o040)
    , ("spade",                 0o252)
    , ("suchthat",              0o047)
    , ("summation",             0o345)
    , ("tau",                   0o164)
    , ("therefore",             0o134)
    , ("theta",                 0o161)
    , ("theta1",                0o112)
    , ("three",                 0o063)
    , ("trademarksans",         0o344)
    , ("trademarkserif",        0o324)
    , ("two",                   0o062)
    , ("underscore",            0o137)
    , ("union",                 0o310)
    , ("universal",             0o042)
    , ("upsilon",               0o165)
    , ("weierstrass",           0o303)
    , ("xi",                    0o170)
    , ("zero",                  0o060)
    , ("zeta",                  0o172)
    ]
