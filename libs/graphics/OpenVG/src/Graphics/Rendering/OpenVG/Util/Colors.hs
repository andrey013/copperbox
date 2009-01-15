{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenVG.Util.Colors
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Colour definitions named with the SVG \'named colours\', but
-- defining OpenGL colours. 
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenVG.Util.Colors (
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

import Graphics.Rendering.OpenGL.GL ( 
    Color4(..), Color3(..), GLfloat, GLubyte )

import Prelude hiding ( tan )

class RGB t where rgb :: GLfloat -> GLfloat -> GLfloat -> t GLfloat

instance RGB Color4 where
  rgb r g b = Color4 (r / 255.0) (g / 255.0) (b / 255.0) 1.0

instance RGB Color3 where
  rgb r g b = Color3 (r / 255.0) (g / 255.0) (b / 255.0)
  
  
aliceblue           :: RGB c => c GLfloat
aliceblue           = rgb 240 248 255

antiquewhite        :: RGB c => c GLfloat
antiquewhite        = rgb 250 235 215

aqua                :: RGB c => c GLfloat
aqua                = rgb 0 255 255

aquamarine          :: RGB c => c GLfloat
aquamarine          = rgb 127 255 212

azure               :: RGB c => c GLfloat
azure               = rgb 240 255 255

beige               :: RGB c => c GLfloat  
beige               = rgb 245 245 220

bisque              :: RGB c => c GLfloat
bisque              = rgb 255 228 196

black               :: RGB c => c GLfloat
black               = rgb  0 0 0

blanchedalmond      :: RGB c => c GLfloat
blanchedalmond      = rgb 255 235 205

blue                :: RGB c => c GLfloat
blue                = rgb  0 0 255

blueviolet          :: RGB c => c GLfloat
blueviolet          = rgb 138 43 226

brown               :: RGB c => c GLfloat
brown               = rgb 165 42 42

burlywood           :: RGB c => c GLfloat
burlywood           = rgb 222 184 135

cadetblue           :: RGB c => c GLfloat
cadetblue           = rgb  95 158 160

chartreuse          :: RGB c => c GLfloat
chartreuse          = rgb 127 255 0

chocolate           :: RGB c => c GLfloat
chocolate           = rgb 210 105 30

coral               :: RGB c => c GLfloat
coral               = rgb 255 127 80

cornflowerblue      :: RGB c => c GLfloat
cornflowerblue      = rgb 100 149 237

cornsilk            :: RGB c => c GLfloat
cornsilk            = rgb 255 248 220

crimson             :: RGB c => c GLfloat
crimson             = rgb 220 20 60

cyan                :: RGB c => c GLfloat
cyan                = rgb  0 255 255

darkblue            :: RGB c => c GLfloat
darkblue            = rgb  0 0 139

darkcyan            :: RGB c => c GLfloat
darkcyan            = rgb  0 139 139

darkgoldenrod       :: RGB c => c GLfloat
darkgoldenrod       = rgb 184 134 11

darkgray            :: RGB c => c GLfloat
darkgray            = rgb 169 169 169

darkgreen           :: RGB c => c GLfloat
darkgreen           = rgb  0 100 0

darkgrey            :: RGB c => c GLfloat
darkgrey            = rgb 169 169 169

darkkhaki           :: RGB c => c GLfloat
darkkhaki           = rgb 189 183 107

darkmagenta         :: RGB c => c GLfloat
darkmagenta         = rgb 139 0 139

darkolivegreen      :: RGB c => c GLfloat
darkolivegreen      = rgb  85 107 47

darkorange          :: RGB c => c GLfloat
darkorange          = rgb 255 140 0

darkorchid          :: RGB c => c GLfloat
darkorchid          = rgb 153 50 204

darkred             :: RGB c => c GLfloat
darkred             = rgb 139 0 0

darksalmon          :: RGB c => c GLfloat
darksalmon          = rgb 233 150 122

darkseagreen        :: RGB c => c GLfloat
darkseagreen        = rgb 143 188 143

darkslateblue       :: RGB c => c GLfloat
darkslateblue       = rgb  72 61 139

darkslategray       :: RGB c => c GLfloat
darkslategray       = rgb  47 79 79

darkslategrey       :: RGB c => c GLfloat
darkslategrey       = rgb  47 79 79

darkturquoise       :: RGB c => c GLfloat
darkturquoise       = rgb  0 206 209

darkviolet          :: RGB c => c GLfloat
darkviolet          = rgb 148 0 211

deeppink            :: RGB c => c GLfloat
deeppink            = rgb 255 20 147

deepskyblue         :: RGB c => c GLfloat
deepskyblue         = rgb  0 191 255

dimgray             :: RGB c => c GLfloat
dimgray             = rgb 105 105 105

dimgrey             :: RGB c => c GLfloat
dimgrey             = rgb 105 105 105

dodgerblue          :: RGB c => c GLfloat
dodgerblue          = rgb  30 144 255

firebrick           :: RGB c => c GLfloat
firebrick           = rgb 178 34 34

floralwhite         :: RGB c => c GLfloat
floralwhite         = rgb 255 250 240

forestgreen         :: RGB c => c GLfloat
forestgreen         = rgb  34 139 34

fuchsia             :: RGB c => c GLfloat
fuchsia             = rgb 255 0 255

gainsboro           :: RGB c => c GLfloat
gainsboro           = rgb 220 220 220

ghostwhite          :: RGB c => c GLfloat
ghostwhite          = rgb 248 248 255

gold                :: RGB c => c GLfloat
gold                = rgb 255 215 0

goldenrod           :: RGB c => c GLfloat
goldenrod           = rgb 218 165 32

gray                :: RGB c => c GLfloat
gray                = rgb 128 128 128

grey                :: RGB c => c GLfloat
grey                = rgb 128 128 128

green               :: RGB c => c GLfloat
green               = rgb  0 128 0

greenyellow         :: RGB c => c GLfloat
greenyellow         = rgb 173 255 47

honeydew            :: RGB c => c GLfloat
honeydew            = rgb 240 255 240

hotpink             :: RGB c => c GLfloat
hotpink             = rgb 255 105 180

indianred           :: RGB c => c GLfloat
indianred           = rgb 205 92 92

indigo              :: RGB c => c GLfloat
indigo              = rgb  75 0 130

ivory               :: RGB c => c GLfloat
ivory               = rgb 255 255 240

khaki               :: RGB c => c GLfloat
khaki               = rgb 240 230 140

lavender            :: RGB c => c GLfloat
lavender            = rgb 230 230 250

lavenderblush       :: RGB c => c GLfloat
lavenderblush       = rgb 255 240 245

lawngreen           :: RGB c => c GLfloat
lawngreen           = rgb 124 252 0

lemonchiffon        :: RGB c => c GLfloat
lemonchiffon        = rgb 255 250 205

lightblue           :: RGB c => c GLfloat
lightblue           = rgb 173 216 230

lightcoral          :: RGB c => c GLfloat
lightcoral          = rgb 240 128 128

lightcyan           :: RGB c => c GLfloat
lightcyan           = rgb 224 255 255

lightgoldenrodyellow  :: RGB c => c GLfloat
lightgoldenrodyellow  = rgb 250 250 210

lightgray           :: RGB c => c GLfloat
lightgray           = rgb 211 211 211

lightgreen          :: RGB c => c GLfloat
lightgreen          = rgb 144 238 144

lightgrey           :: RGB c => c GLfloat
lightgrey           = rgb 211 211 211

lightpink           :: RGB c => c GLfloat
lightpink           = rgb 255 182 193

lightsalmon         :: RGB c => c GLfloat
lightsalmon         = rgb 255 160 122

lightseagreen       :: RGB c => c GLfloat
lightseagreen       = rgb  32 178 170

lightskyblue        :: RGB c => c GLfloat
lightskyblue        = rgb 135 206 250

lightslategray      :: RGB c => c GLfloat
lightslategray      = rgb 119 136 153

lightslategrey      :: RGB c => c GLfloat
lightslategrey      = rgb 119 136 153

lightsteelblue      :: RGB c => c GLfloat
lightsteelblue      = rgb 176 196 222

lightyellow         :: RGB c => c GLfloat
lightyellow         = rgb 255 255 224

lime                :: RGB c => c GLfloat
lime                = rgb  0 255 0

limegreen           :: RGB c => c GLfloat
limegreen           = rgb  50 205 50

linen               :: RGB c => c GLfloat
linen               = rgb 250 240 230

magenta             :: RGB c => c GLfloat
magenta             = rgb 255 0 255

maroon              :: RGB c => c GLfloat
maroon              = rgb 128 0 0

mediumaquamarine    :: RGB c => c GLfloat
mediumaquamarine    = rgb 102 205 170

mediumblue          :: RGB c => c GLfloat
mediumblue          = rgb 0 0 205

mediumorchid        :: RGB c => c GLfloat
mediumorchid        = rgb 186 85 211

mediumpurple        :: RGB c => c GLfloat
mediumpurple        = rgb 147 112 219

mediumseagreen      :: RGB c => c GLfloat
mediumseagreen      = rgb 60 179 113

mediumslateblue     :: RGB c => c GLfloat
mediumslateblue     = rgb 123 104 238

mediumspringgreen   :: RGB c => c GLfloat
mediumspringgreen   = rgb 0 250 154

mediumturquoise     :: RGB c => c GLfloat
mediumturquoise     = rgb 72 209 204

mediumvioletred     :: RGB c => c GLfloat
mediumvioletred     = rgb 199 21 133

midnightblue        :: RGB c => c GLfloat
midnightblue        = rgb  25 25 112

mintcream           :: RGB c => c GLfloat
mintcream           = rgb 245 255 250

mistyrose           :: RGB c => c GLfloat
mistyrose           = rgb 255 228 225

moccasin            :: RGB c => c GLfloat
moccasin            = rgb 255 228 181

navajowhite         :: RGB c => c GLfloat
navajowhite         = rgb 255 222 173

navy                :: RGB c => c GLfloat
navy                = rgb  0 0 128

oldlace             :: RGB c => c GLfloat
oldlace             = rgb 253 245 230

olive               :: RGB c => c GLfloat
olive               = rgb 128 128 0

olivedrab           :: RGB c => c GLfloat
olivedrab           = rgb 107 142 35

orange              :: RGB c => c GLfloat
orange              = rgb 255 165 0

orangered           :: RGB c => c GLfloat
orangered           = rgb 255 69 0

orchid              :: RGB c => c GLfloat
orchid              = rgb 218 112 214

palegoldenrod       :: RGB c => c GLfloat
palegoldenrod       = rgb 238 232 170

palegreen           :: RGB c => c GLfloat
palegreen           = rgb 152 251 152

paleturquoise       :: RGB c => c GLfloat
paleturquoise       = rgb 175 238 238

palevioletred       :: RGB c => c GLfloat
palevioletred       = rgb 219 112 147

papayawhip          :: RGB c => c GLfloat
papayawhip          = rgb 255 239 213

peachpuff           :: RGB c => c GLfloat
peachpuff           = rgb 255 218 185

peru                :: RGB c => c GLfloat
peru                = rgb 205 133 63

pink                :: RGB c => c GLfloat
pink                = rgb 255 192 203

plum                :: RGB c => c GLfloat
plum                = rgb 221 160 221

powderblue          :: RGB c => c GLfloat
powderblue          = rgb 176 224 230

purple              :: RGB c => c GLfloat
purple              = rgb 128 0 128

red                 :: RGB c => c GLfloat
red                 = rgb 255 0 0

rosybrown           :: RGB c => c GLfloat
rosybrown           = rgb 188 143 143

royalblue           :: RGB c => c GLfloat
royalblue           = rgb  65 105 225

saddlebrown         :: RGB c => c GLfloat
saddlebrown         = rgb 139 69 19

salmon              :: RGB c => c GLfloat
salmon              = rgb 250 128 114

sandybrown          :: RGB c => c GLfloat
sandybrown          = rgb 244 164 96

seagreen            :: RGB c => c GLfloat
seagreen            = rgb  46 139 87

seashell            :: RGB c => c GLfloat
seashell            = rgb 255 245 238

sienna              :: RGB c => c GLfloat
sienna              = rgb 160 82 45

silver              :: RGB c => c GLfloat
silver              = rgb 192 192 192

skyblue             :: RGB c => c GLfloat
skyblue             = rgb 135 206 235

slateblue           :: RGB c => c GLfloat
slateblue           = rgb 106 90 205

slategray           :: RGB c => c GLfloat
slategray           = rgb 112 128 144

slategrey           :: RGB c => c GLfloat
slategrey           = rgb 112 128 144

snow                :: RGB c => c GLfloat
snow                = rgb 255 250 250

springgreen         :: RGB c => c GLfloat
springgreen         = rgb  0 255 127

steelblue           :: RGB c => c GLfloat
steelblue           = rgb  70 130 180

tan                 :: RGB c => c GLfloat
tan                 = rgb 210 180 140

teal                :: RGB c => c GLfloat
teal                = rgb  0 128 128

thistle             :: RGB c => c GLfloat
thistle             = rgb 216 191 216

tomato              :: RGB c => c GLfloat
tomato              = rgb 255 99 71

turquoise           :: RGB c => c GLfloat
turquoise           = rgb  64 224 208

violet              :: RGB c => c GLfloat
violet              = rgb 238 130 238

wheat               :: RGB c => c GLfloat
wheat               = rgb 245 222 179

white               :: RGB c => c GLfloat
white               = rgb 255 255 255

whitesmoke          :: RGB c => c GLfloat
whitesmoke          = rgb 245 245 245

yellow              :: RGB c => c GLfloat
yellow              = rgb 255 255 0

yellowgreen         :: RGB c => c GLfloat
yellowgreen         = rgb 154 205 50





