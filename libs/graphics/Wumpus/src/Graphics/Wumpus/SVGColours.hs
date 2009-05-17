{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Wumpus.SVGColours
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- The SVG \'named colours\', but as rgb [0,1] values 
--
--------------------------------------------------------------------------------

module Graphics.Wumpus.SVGColours (
    -- * Named colours
    aliceblue, 
    antiquewhite,
    aqua,
    aquamarine,
    azure,
    beige,
    bisque,
    black,
    blanchedalmond,
    blue,
    blueviolet,
    brown,
    burlywood,
    cadetblue,
    chartreuse,
    chocolate,
    coral,
    cornflowerblue,
    cornsilk,
    crimson,
    cyan,
    darkblue,
    darkcyan,
    darkgoldenrod,
    darkgray,
    darkgreen,
    darkgrey,
    darkkhaki,
    darkmagenta,
    darkolivegreen,
    darkorange,
    darkorchid,
    darkred,
    darksalmon,
    darkseagreen,
    darkslateblue,
    darkslategray,
    darkslategrey,
    darkturquoise,
    darkviolet,
    deeppink,
    deepskyblue,
    dimgray,
    dimgrey,
    dodgerblue,
    firebrick,
    floralwhite,
    forestgreen,
    fuchsia,
    gainsboro,
    ghostwhite,
    gold,
    goldenrod,
    gray,
    grey,
    green,
    greenyellow,
    honeydew,
    hotpink,
    indianred,
    indigo,
    ivory,
    khaki,
    lavender,
    lavenderblush,
    lawngreen,
    lemonchiffon,
    lightblue,
    lightcoral,
    lightcyan,
    lightgoldenrodyellow,
    lightgray,
    lightgreen,
    lightgrey,
    lightpink,
    lightsalmon,
    lightseagreen,
    lightskyblue,
    lightslategray,
    lightslategrey,
    lightsteelblue,
    lightyellow,
    lime,
    limegreen,
    linen,
    magenta,
    maroon,
    mediumaquamarine,
    mediumblue,
    mediumorchid,
    mediumpurple,
    mediumseagreen,
    mediumslateblue,
    mediumspringgreen,
    mediumturquoise,
    mediumvioletred,
    midnightblue,
    mintcream,
    mistyrose,
    moccasin,
    navajowhite,
    navy,
    oldlace,
    olive,
    olivedrab,
    orange,
    orangered,
    orchid,
    palegoldenrod,
    palegreen,
    paleturquoise,
    palevioletred,
    papayawhip,
    peachpuff,
    peru,
    pink,
    plum,
    powderblue,
    purple,
    red,
    rosybrown,
    royalblue,
    saddlebrown,
    salmon,
    sandybrown,
    seagreen,
    seashell,
    sienna,
    silver,
    skyblue,
    slateblue,
    slategray,
    slategrey,
    snow,
    springgreen,
    steelblue,
    tan,
    teal,
    thistle,
    tomato,
    turquoise,
    violet,
    wheat,
    white,
    whitesmoke,
    yellow,
    yellowgreen
) where

import Graphics.Wumpus.Colour ( Colour3 )

import Data.SG ( Triple(..) )

import Prelude hiding ( tan )

rgb :: Double -> Double -> Double -> Colour3
rgb r g b = Triple (r / 255.0, g / 255.0, b / 255.0)
  
  
aliceblue           :: Colour3
aliceblue           = rgb 240 248 255

antiquewhite        :: Colour3
antiquewhite        = rgb 250 235 215

aqua                :: Colour3
aqua                = rgb 0 255 255

aquamarine          :: Colour3
aquamarine          = rgb 127 255 212

azure               :: Colour3
azure               = rgb 240 255 255

beige               :: Colour3  
beige               = rgb 245 245 220

bisque              :: Colour3
bisque              = rgb 255 228 196

black               :: Colour3
black               = rgb  0 0 0

blanchedalmond      :: Colour3
blanchedalmond      = rgb 255 235 205

blue                :: Colour3
blue                = rgb  0 0 255

blueviolet          :: Colour3
blueviolet          = rgb 138 43 226

brown               :: Colour3
brown               = rgb 165 42 42

burlywood           :: Colour3
burlywood           = rgb 222 184 135

cadetblue           :: Colour3
cadetblue           = rgb  95 158 160

chartreuse          :: Colour3
chartreuse          = rgb 127 255 0

chocolate           :: Colour3
chocolate           = rgb 210 105 30

coral               :: Colour3
coral               = rgb 255 127 80

cornflowerblue      :: Colour3
cornflowerblue      = rgb 100 149 237

cornsilk            :: Colour3
cornsilk            = rgb 255 248 220

crimson             :: Colour3
crimson             = rgb 220 20 60

cyan                :: Colour3
cyan                = rgb  0 255 255

darkblue            :: Colour3
darkblue            = rgb  0 0 139

darkcyan            :: Colour3
darkcyan            = rgb  0 139 139

darkgoldenrod       :: Colour3
darkgoldenrod       = rgb 184 134 11

darkgray            :: Colour3
darkgray            = rgb 169 169 169

darkgreen           :: Colour3
darkgreen           = rgb  0 100 0

darkgrey            :: Colour3
darkgrey            = rgb 169 169 169

darkkhaki           :: Colour3
darkkhaki           = rgb 189 183 107

darkmagenta         :: Colour3
darkmagenta         = rgb 139 0 139

darkolivegreen      :: Colour3
darkolivegreen      = rgb  85 107 47

darkorange          :: Colour3
darkorange          = rgb 255 140 0

darkorchid          :: Colour3
darkorchid          = rgb 153 50 204

darkred             :: Colour3
darkred             = rgb 139 0 0

darksalmon          :: Colour3
darksalmon          = rgb 233 150 122

darkseagreen        :: Colour3
darkseagreen        = rgb 143 188 143

darkslateblue       :: Colour3
darkslateblue       = rgb  72 61 139

darkslategray       :: Colour3
darkslategray       = rgb  47 79 79

darkslategrey       :: Colour3
darkslategrey       = rgb  47 79 79

darkturquoise       :: Colour3
darkturquoise       = rgb  0 206 209

darkviolet          :: Colour3
darkviolet          = rgb 148 0 211

deeppink            :: Colour3
deeppink            = rgb 255 20 147

deepskyblue         :: Colour3
deepskyblue         = rgb  0 191 255

dimgray             :: Colour3
dimgray             = rgb 105 105 105

dimgrey             :: Colour3
dimgrey             = rgb 105 105 105

dodgerblue          :: Colour3
dodgerblue          = rgb  30 144 255

firebrick           :: Colour3
firebrick           = rgb 178 34 34

floralwhite         :: Colour3
floralwhite         = rgb 255 250 240

forestgreen         :: Colour3
forestgreen         = rgb  34 139 34

fuchsia             :: Colour3
fuchsia             = rgb 255 0 255

gainsboro           :: Colour3
gainsboro           = rgb 220 220 220

ghostwhite          :: Colour3
ghostwhite          = rgb 248 248 255

gold                :: Colour3
gold                = rgb 255 215 0

goldenrod           :: Colour3
goldenrod           = rgb 218 165 32

gray                :: Colour3
gray                = rgb 128 128 128

grey                :: Colour3
grey                = rgb 128 128 128

green               :: Colour3
green               = rgb  0 128 0

greenyellow         :: Colour3
greenyellow         = rgb 173 255 47

honeydew            :: Colour3
honeydew            = rgb 240 255 240

hotpink             :: Colour3
hotpink             = rgb 255 105 180

indianred           :: Colour3
indianred           = rgb 205 92 92

indigo              :: Colour3
indigo              = rgb  75 0 130

ivory               :: Colour3
ivory               = rgb 255 255 240

khaki               :: Colour3
khaki               = rgb 240 230 140

lavender            :: Colour3
lavender            = rgb 230 230 250

lavenderblush       :: Colour3
lavenderblush       = rgb 255 240 245

lawngreen           :: Colour3
lawngreen           = rgb 124 252 0

lemonchiffon        :: Colour3
lemonchiffon        = rgb 255 250 205

lightblue           :: Colour3
lightblue           = rgb 173 216 230

lightcoral          :: Colour3
lightcoral          = rgb 240 128 128

lightcyan           :: Colour3
lightcyan           = rgb 224 255 255

lightgoldenrodyellow  :: Colour3
lightgoldenrodyellow  = rgb 250 250 210

lightgray           :: Colour3
lightgray           = rgb 211 211 211

lightgreen          :: Colour3
lightgreen          = rgb 144 238 144

lightgrey           :: Colour3
lightgrey           = rgb 211 211 211

lightpink           :: Colour3
lightpink           = rgb 255 182 193

lightsalmon         :: Colour3
lightsalmon         = rgb 255 160 122

lightseagreen       :: Colour3
lightseagreen       = rgb  32 178 170

lightskyblue        :: Colour3
lightskyblue        = rgb 135 206 250

lightslategray      :: Colour3
lightslategray      = rgb 119 136 153

lightslategrey      :: Colour3
lightslategrey      = rgb 119 136 153

lightsteelblue      :: Colour3
lightsteelblue      = rgb 176 196 222

lightyellow         :: Colour3
lightyellow         = rgb 255 255 224

lime                :: Colour3
lime                = rgb  0 255 0

limegreen           :: Colour3
limegreen           = rgb  50 205 50

linen               :: Colour3
linen               = rgb 250 240 230

magenta             :: Colour3
magenta             = rgb 255 0 255

maroon              :: Colour3
maroon              = rgb 128 0 0

mediumaquamarine    :: Colour3
mediumaquamarine    = rgb 102 205 170

mediumblue          :: Colour3
mediumblue          = rgb 0 0 205

mediumorchid        :: Colour3
mediumorchid        = rgb 186 85 211

mediumpurple        :: Colour3
mediumpurple        = rgb 147 112 219

mediumseagreen      :: Colour3
mediumseagreen      = rgb 60 179 113

mediumslateblue     :: Colour3
mediumslateblue     = rgb 123 104 238

mediumspringgreen   :: Colour3
mediumspringgreen   = rgb 0 250 154

mediumturquoise     :: Colour3
mediumturquoise     = rgb 72 209 204

mediumvioletred     :: Colour3
mediumvioletred     = rgb 199 21 133

midnightblue        :: Colour3
midnightblue        = rgb  25 25 112

mintcream           :: Colour3
mintcream           = rgb 245 255 250

mistyrose           :: Colour3
mistyrose           = rgb 255 228 225

moccasin            :: Colour3
moccasin            = rgb 255 228 181

navajowhite         :: Colour3
navajowhite         = rgb 255 222 173

navy                :: Colour3
navy                = rgb  0 0 128

oldlace             :: Colour3
oldlace             = rgb 253 245 230

olive               :: Colour3
olive               = rgb 128 128 0

olivedrab           :: Colour3
olivedrab           = rgb 107 142 35

orange              :: Colour3
orange              = rgb 255 165 0

orangered           :: Colour3
orangered           = rgb 255 69 0

orchid              :: Colour3
orchid              = rgb 218 112 214

palegoldenrod       :: Colour3
palegoldenrod       = rgb 238 232 170

palegreen           :: Colour3
palegreen           = rgb 152 251 152

paleturquoise       :: Colour3
paleturquoise       = rgb 175 238 238

palevioletred       :: Colour3
palevioletred       = rgb 219 112 147

papayawhip          :: Colour3
papayawhip          = rgb 255 239 213

peachpuff           :: Colour3
peachpuff           = rgb 255 218 185

peru                :: Colour3
peru                = rgb 205 133 63

pink                :: Colour3
pink                = rgb 255 192 203

plum                :: Colour3
plum                = rgb 221 160 221

powderblue          :: Colour3
powderblue          = rgb 176 224 230

purple              :: Colour3
purple              = rgb 128 0 128

red                 :: Colour3
red                 = rgb 255 0 0

rosybrown           :: Colour3
rosybrown           = rgb 188 143 143

royalblue           :: Colour3
royalblue           = rgb  65 105 225

saddlebrown         :: Colour3
saddlebrown         = rgb 139 69 19

salmon              :: Colour3
salmon              = rgb 250 128 114

sandybrown          :: Colour3
sandybrown          = rgb 244 164 96

seagreen            :: Colour3
seagreen            = rgb  46 139 87

seashell            :: Colour3
seashell            = rgb 255 245 238

sienna              :: Colour3
sienna              = rgb 160 82 45

silver              :: Colour3
silver              = rgb 192 192 192

skyblue             :: Colour3
skyblue             = rgb 135 206 235

slateblue           :: Colour3
slateblue           = rgb 106 90 205

slategray           :: Colour3
slategray           = rgb 112 128 144

slategrey           :: Colour3
slategrey           = rgb 112 128 144

snow                :: Colour3
snow                = rgb 255 250 250

springgreen         :: Colour3
springgreen         = rgb  0 255 127

steelblue           :: Colour3
steelblue           = rgb  70 130 180

tan                 :: Colour3
tan                 = rgb 210 180 140

teal                :: Colour3
teal                = rgb  0 128 128

thistle             :: Colour3
thistle             = rgb 216 191 216

tomato              :: Colour3
tomato              = rgb 255 99 71

turquoise           :: Colour3
turquoise           = rgb  64 224 208

violet              :: Colour3
violet              = rgb 238 130 238

wheat               :: Colour3
wheat               = rgb 245 222 179

white               :: Colour3
white               = rgb 255 255 255

whitesmoke          :: Colour3
whitesmoke          = rgb 245 245 245

yellow              :: Colour3
yellow              = rgb 255 255 0

yellowgreen         :: Colour3
yellowgreen         = rgb 154 205 50





