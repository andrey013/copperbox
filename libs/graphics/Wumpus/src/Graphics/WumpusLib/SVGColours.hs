{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.WumpusLib.SVGColours
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

module Graphics.WumpusLib.SVGColours (
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
  
  
aliceblue           :: Colour3
aliceblue           = Triple (0.941, 0.973, 1.0)

antiquewhite        :: Colour3
antiquewhite        = Triple (0.980, 0.922, 0.843)

aqua                :: Colour3
aqua                = Triple (0.0, 1.0, 1.0)

aquamarine          :: Colour3
aquamarine          = Triple (0.498, 1.0, 0.831)

azure               :: Colour3
azure               = Triple (0.941, 1.0, 1.0)

beige               :: Colour3  
beige               = Triple (0.961, 0.961, 0.863)

bisque              :: Colour3
bisque              = Triple (1.0, 0.894, 0.769)

black               :: Colour3
black               = Triple (0.0, 0.0, 0.0)

blanchedalmond      :: Colour3
blanchedalmond      = Triple (1.0, 0.922, 0.804)

blue                :: Colour3
blue                = Triple (0.0, 0.0, 1.0)

blueviolet          :: Colour3
blueviolet          = Triple (0.541, 0.169, 0.886)

brown               :: Colour3
brown               = Triple (0.647, 0.165, 0.165)

burlywood           :: Colour3
burlywood           = Triple (0.871, 0.722, 0.529)

cadetblue           :: Colour3
cadetblue           = Triple (0.373, 0.620, 0.627)

chartreuse          :: Colour3
chartreuse          = Triple (0.498, 1.0, 0.0)

chocolate           :: Colour3
chocolate           = Triple (0.824, 0.412, 0.118)

coral               :: Colour3
coral               = Triple (1.0, 0.498, 0.314)

cornflowerblue      :: Colour3
cornflowerblue      = Triple (0.392, 0.584, 0.929)

cornsilk            :: Colour3
cornsilk            = Triple (1.0, 0.973, 0.863)

crimson             :: Colour3
crimson             = Triple (0.863, 0.078, 0.235)

cyan                :: Colour3
cyan                = Triple (0.0, 1.0, 1.0)

darkblue            :: Colour3
darkblue            = Triple (0.0, 0.0, 0.545)

darkcyan            :: Colour3
darkcyan            = Triple (0.0, 0.545, 0.545)

darkgoldenrod       :: Colour3
darkgoldenrod       = Triple (0.722, 0.545, 0.043)

darkgray            :: Colour3
darkgray            = Triple (0.663, 0.663, 0.663)

darkgreen           :: Colour3
darkgreen           = Triple (0.0, 0.392, 0.0)

darkgrey            :: Colour3
darkgrey            = Triple (0.663, 0.663, 0.663)

darkkhaki           :: Colour3
darkkhaki           = Triple (0.741, 0.718, 0.420)

darkmagenta         :: Colour3
darkmagenta         = Triple (0.545, 0.0, 0.545)

darkolivegreen      :: Colour3
darkolivegreen      = Triple (0.333, 0.420, 0.184)

darkorange          :: Colour3
darkorange          = Triple (1.0, 0.549, 0.0)

darkorchid          :: Colour3
darkorchid          = Triple (0.600, 0.196, 0.800)

darkred             :: Colour3
darkred             = Triple (0.545, 0.0, 0.0)

darksalmon          :: Colour3
darksalmon          = Triple (0.914, 0.588, 0.478)

darkseagreen        :: Colour3
darkseagreen        = Triple (0.561, 0.737, 0.561)

darkslateblue       :: Colour3
darkslateblue       = Triple (0.282, 0.239, 0.545)

darkslategray       :: Colour3
darkslategray       = Triple (0.184, 0.310, 0.310)

darkslategrey       :: Colour3
darkslategrey       = Triple (0.184, 0.310, 0.310)

darkturquoise       :: Colour3
darkturquoise       = Triple (0.0, 0.808, 0.820)

darkviolet          :: Colour3
darkviolet          = Triple (0.580, 0.0, 0.827)

deeppink            :: Colour3
deeppink            = Triple (1.0, 0.078, 0.576)

deepskyblue         :: Colour3
deepskyblue         = Triple (0.0, 0.750, 1.0)

dimgray             :: Colour3
dimgray             = Triple (0.412, 0.412, 0.412)

dimgrey             :: Colour3
dimgrey             = Triple (0.412, 0.412, 0.412)

dodgerblue          :: Colour3
dodgerblue          = Triple (0.118, 0.565, 1.0)

firebrick           :: Colour3
firebrick           = Triple (0.698, 0.133, 0.133)

floralwhite         :: Colour3
floralwhite         = Triple (1.0, 0.980, 0.941)

forestgreen         :: Colour3
forestgreen         = Triple (0.133, 0.545, 0.133)

fuchsia             :: Colour3
fuchsia             = Triple (1.0, 0.0, 1.0)

gainsboro           :: Colour3
gainsboro           = Triple (0.863, 0.863, 0.863)

ghostwhite          :: Colour3
ghostwhite          = Triple (0.973, 0.973, 1.0)

gold                :: Colour3
gold                = Triple (1.0, 0.843, 0.0)

goldenrod           :: Colour3
goldenrod           = Triple (0.855, 0.647, 0.125)

gray                :: Colour3
gray                = Triple (0.5, 0.5, 0.5)

grey                :: Colour3
grey                = Triple (0.5, 0.5, 0.5)

green               :: Colour3
green               = Triple (0.0, 0.5, 0.0)

greenyellow         :: Colour3
greenyellow         = Triple (0.678, 1.0, 0.184)

honeydew            :: Colour3
honeydew            = Triple (0.941, 1.0, 0.941)

hotpink             :: Colour3
hotpink             = Triple (1.0, 0.412, 0.706)

indianred           :: Colour3
indianred           = Triple (0.804, 0.361, 0.361)

indigo              :: Colour3
indigo              = Triple (0.294, 0.0, 0.510)

ivory               :: Colour3
ivory               = Triple (1.0, 1.0, 0.941)

khaki               :: Colour3
khaki               = Triple (0.941, 0.902, 0.549)

lavender            :: Colour3
lavender            = Triple (0.902, 0.902, 0.980)

lavenderblush       :: Colour3
lavenderblush       = Triple (1.0, 0.941, 0.961)

lawngreen           :: Colour3
lawngreen           = Triple (0.486, 0.988, 0.0)

lemonchiffon        :: Colour3
lemonchiffon        = Triple (1.0, 0.980, 0.804)

lightblue           :: Colour3
lightblue           = Triple (0.678, 0.847, 0.902)

lightcoral          :: Colour3
lightcoral          = Triple (0.941, 0.5, 0.5)

lightcyan           :: Colour3
lightcyan           = Triple (0.878, 1.0, 1.0)

lightgoldenrodyellow  :: Colour3
lightgoldenrodyellow  = Triple (0.980, 0.980, 0.824)

lightgray           :: Colour3
lightgray           = Triple (0.827, 0.827, 0.827)

lightgreen          :: Colour3
lightgreen          = Triple (0.565, 0.933, 0.565)

lightgrey           :: Colour3
lightgrey           = Triple (0.827, 0.827, 0.827)

lightpink           :: Colour3
lightpink           = Triple (1.0, 0.714, 0.757)

lightsalmon         :: Colour3
lightsalmon         = Triple (1.0, 0.627, 0.478)

lightseagreen       :: Colour3
lightseagreen       = Triple (0.125, 0.698, 0.666)

lightskyblue        :: Colour3
lightskyblue        = Triple (0.529, 0.808, 0.980)

lightslategray      :: Colour3
lightslategray      = Triple (0.466, 0.533, 0.600)

lightslategrey      :: Colour3
lightslategrey      = Triple (0.466, 0.533, 0.600)

lightsteelblue      :: Colour3
lightsteelblue      = Triple (0.690, 0.769, 0.871)

lightyellow         :: Colour3
lightyellow         = Triple (1.0, 1.0, 0.878)

lime                :: Colour3
lime                = Triple (0.0, 1.0, 0.0)

limegreen           :: Colour3
limegreen           = Triple (0.196, 0.804, 0.196)

linen               :: Colour3
linen               = Triple (0.980, 0.941, 0.902)

magenta             :: Colour3
magenta             = Triple (1.0, 0.0, 1.0)

maroon              :: Colour3
maroon              = Triple (0.5, 0.0, 0.0)

mediumaquamarine    :: Colour3
mediumaquamarine    = Triple (0.4, 0.804, 0.666)

mediumblue          :: Colour3
mediumblue          = Triple (0.0, 0.0, 0.804)

mediumorchid        :: Colour3
mediumorchid        = Triple (0.729, 0.333, 0.827)

mediumpurple        :: Colour3
mediumpurple        = Triple (0.576, 0.439, 0.859)

mediumseagreen      :: Colour3
mediumseagreen      = Triple (0.235, 0.702, 0.443)

mediumslateblue     :: Colour3
mediumslateblue     = Triple (0.482, 0.408, 0.933)

mediumspringgreen   :: Colour3
mediumspringgreen   = Triple (0.0, 0.980, 0.604)

mediumturquoise     :: Colour3
mediumturquoise     = Triple (0.282, 0.820, 0.800)

mediumvioletred     :: Colour3
mediumvioletred     = Triple (0.780, 0.082, 0.522)

midnightblue        :: Colour3
midnightblue        = Triple (0.098, 0.098, 0.439)

mintcream           :: Colour3
mintcream           = Triple (0.961, 1.0, 0.980)

mistyrose           :: Colour3
mistyrose           = Triple (1.0, 0.894, 0.882)

moccasin            :: Colour3
moccasin            = Triple (1.0, 0.894, 0.710)

navajowhite         :: Colour3
navajowhite         = Triple (1.0, 0.871, 0.678)

navy                :: Colour3
navy                = Triple (0.0, 0.0, 0.5)

oldlace             :: Colour3
oldlace             = Triple (0.992, 0.961, 0.902)

olive               :: Colour3
olive               = Triple (0.5, 0.5, 0.0)

olivedrab           :: Colour3
olivedrab           = Triple (0.420, 0.557, 0.137)

orange              :: Colour3
orange              = Triple (1.0, 0.647, 0.0)

orangered           :: Colour3
orangered           = Triple (1.0, 0.271, 0.0)

orchid              :: Colour3
orchid              = Triple (0.855, 0.439, 0.839)

palegoldenrod       :: Colour3
palegoldenrod       = Triple (0.933, 0.910, 0.666)

palegreen           :: Colour3
palegreen           = Triple (0.596, 0.984, 0.596)

paleturquoise       :: Colour3
paleturquoise       = Triple (0.686, 0.933, 0.933)

palevioletred       :: Colour3
palevioletred       = Triple (0.859, 0.439, 0.576)

papayawhip          :: Colour3
papayawhip          = Triple (1.0, 0.937, 0.835)

peachpuff           :: Colour3
peachpuff           = Triple (1.0, 0.855, 0.725)

peru                :: Colour3
peru                = Triple (0.804, 0.522, 0.247)

pink                :: Colour3
pink                = Triple (1.0, 0.753, 0.796)

plum                :: Colour3
plum                = Triple (0.867, 0.627, 0.867)

powderblue          :: Colour3
powderblue          = Triple (0.690, 0.878, 0.902)

purple              :: Colour3
purple              = Triple (0.5, 0.0, 0.5)

red                 :: Colour3
red                 = Triple (1.0, 0.0, 0.0)

rosybrown           :: Colour3
rosybrown           = Triple (0.737, 0.561, 0.561)

royalblue           :: Colour3
royalblue           = Triple (0.255, 0.412, 0.882)

saddlebrown         :: Colour3
saddlebrown         = Triple (0.545, 0.271, 0.075)

salmon              :: Colour3
salmon              = Triple (0.980, 0.5, 0.447)

sandybrown          :: Colour3
sandybrown          = Triple (0.957, 0.643, 0.376)

seagreen            :: Colour3
seagreen            = Triple (0.180, 0.545, 0.341)

seashell            :: Colour3
seashell            = Triple (1.0, 0.961, 0.933)

sienna              :: Colour3
sienna              = Triple (0.627, 0.322, 0.176)

silver              :: Colour3
silver              = Triple (0.753, 0.753, 0.753)

skyblue             :: Colour3
skyblue             = Triple (0.529, 0.808, 0.922)

slateblue           :: Colour3
slateblue           = Triple (0.416, 0.353, 0.804)

slategray           :: Colour3
slategray           = Triple (0.439, 0.5, 0.565)

slategrey           :: Colour3
slategrey           = Triple (0.439, 0.5, 0.565)

snow                :: Colour3
snow                = Triple (1.0, 0.980, 0.980)

springgreen         :: Colour3
springgreen         = Triple (0.0, 1.0, 0.498)

steelblue           :: Colour3
steelblue           = Triple (0.275, 0.510, 0.706)

tan                 :: Colour3
tan                 = Triple (0.824, 0.706, 0.549)

teal                :: Colour3
teal                = Triple (0.0, 0.5, 0.5)

thistle             :: Colour3
thistle             = Triple (0.847, 0.750, 0.847)

tomato              :: Colour3
tomato              = Triple (1.0, 0.388, 0.278)

turquoise           :: Colour3
turquoise           = Triple (0.251, 0.878, 0.816)

violet              :: Colour3
violet              = Triple (0.933, 0.510, 0.933)

wheat               :: Colour3
wheat               = Triple (0.961, 0.871, 0.702)

white               :: Colour3
white               = Triple (1.0, 1.0, 1.0)

whitesmoke          :: Colour3
whitesmoke          = Triple (0.961, 0.961, 0.961)

yellow              :: Colour3
yellow              = Triple (1.0, 1.0, 0.0)

yellowgreen         :: Colour3
yellowgreen         = Triple (0.604, 0.804, 0.196)





