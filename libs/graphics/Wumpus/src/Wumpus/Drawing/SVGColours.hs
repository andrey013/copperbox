{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Drawing.SVGColours
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

module Wumpus.Drawing.SVGColours (
    -- * Named colours
    aliceBlue, 
    antiqueWhite,
    aqua,
    aquamarine,
    azure,
    beige,
    bisque,
    black,
    blanchedAlmond,
    blue,
    blueViolet,
    brown,
    burlywood,
    cadetBlue,
    chartreuse,
    chocolate,
    coral,
    cornflowerBlue,
    cornsilk,
    crimson,
    cyan,
    darkBlue,
    darkCyan,
    darkGoldenrod,
    darkGray,
    darkGreen,
    darkGrey,
    darkKhaki,
    darkMagenta,
    darkOliveGreen,
    darkOrange,
    darkOrchid,
    darkRed,
    darkSalmon,
    darkSeaGreen,
    darkSlateBlue,
    darkSlateGray,
    darkSlateGrey,
    darkTurquoise,
    darkViolet,
    deepPink,
    deepSkyBlue,
    dimGray,
    dimGrey,
    dodgerBlue,
    firebrick,
    floralWhite,
    forestGreen,
    fuchsia,
    gainsboro,
    ghostWhite,
    gold,
    goldenrod,
    gray,
    grey,
    green,
    greenYellow,
    honeydew,
    hotPink,
    indianRed,
    indigo,
    ivory,
    khaki,
    lavender,
    lavenderBlush,
    lawnGreen,
    lemonChiffon,
    lightBlue,
    lightCoral,
    lightCyan,
    lightGoldenrodYellow,
    lightGray,
    lightGreen,
    lightGrey,
    lightPink,
    lightSalmon,
    lightSeaGreen,
    lightSkyBlue,
    lightSlateGray,
    lightSlateGrey,
    lightSteelBlue,
    lightYellow,
    lime,
    limeGreen,
    linen,
    magenta,
    maroon,
    mediumAquamarine,
    mediumBlue,
    mediumOrchid,
    mediumPurple,
    mediumSeaGreen,
    mediumSlateBlue,
    mediumSpringGreen,
    mediumTurquoise,
    mediumVioletRed,
    midnightBlue,
    mintcream,
    mistyrose,
    moccasin,
    navajoWhite,
    navy,
    oldlace,
    olive,
    oliveDrab,
    orange,
    orangeRed,
    orchid,
    paleGoldenrod,
    paleGreen,
    paleTurquoise,
    paleVioletRed,
    papayawhip,
    peachpuff,
    peru,
    pink,
    plum,
    powderBlue,
    purple,
    red,
    rosyBrown,
    royalBlue,
    saddleBrown,
    salmon,
    sandyBrown,
    seaGreen,
    seashell,
    sienna,
    silver,
    skyBlue,
    slateBlue,
    slateGray,
    slateGrey,
    snow,
    springGreen,
    steelBlue,
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
    yellowGreen
) where


import Wumpus.Core.Colour ( Colour3(..), DColour3 )


import Prelude hiding ( tan )
  
  
aliceBlue           :: DColour3
aliceBlue           = C3 0.941  0.973  1.0

antiqueWhite        :: DColour3
antiqueWhite        = C3 0.980  0.922  0.843

aqua                :: DColour3
aqua                = C3 0.0  1.0  1.0

aquamarine          :: DColour3
aquamarine          = C3 0.498  1.0  0.831

azure               :: DColour3
azure               = C3 0.941  1.0  1.0

beige               :: DColour3  
beige               = C3 0.961  0.961  0.863

bisque              :: DColour3
bisque              = C3 1.0  0.894  0.769

black               :: DColour3
black               = C3 0.0  0.0  0.0

blanchedAlmond      :: DColour3
blanchedAlmond      = C3 1.0  0.922  0.804

blue                :: DColour3
blue                = C3 0.0  0.0  1.0

blueViolet          :: DColour3
blueViolet          = C3 0.541  0.169  0.886

brown               :: DColour3
brown               = C3 0.647  0.165  0.165

burlywood           :: DColour3
burlywood           = C3 0.871  0.722  0.529

cadetBlue           :: DColour3
cadetBlue           = C3 0.373  0.620  0.627

chartreuse          :: DColour3
chartreuse          = C3 0.498  1.0  0.0

chocolate           :: DColour3
chocolate           = C3 0.824  0.412  0.118

coral               :: DColour3
coral               = C3 1.0  0.498  0.314

cornflowerBlue      :: DColour3
cornflowerBlue      = C3 0.392  0.584  0.929

cornsilk            :: DColour3
cornsilk            = C3 1.0  0.973  0.863

crimson             :: DColour3
crimson             = C3 0.863  0.078  0.235

cyan                :: DColour3
cyan                = C3 0.0  1.0  1.0

darkBlue            :: DColour3
darkBlue            = C3 0.0  0.0  0.545

darkCyan            :: DColour3
darkCyan            = C3 0.0  0.545  0.545

darkGoldenrod       :: DColour3
darkGoldenrod       = C3 0.722  0.545  0.043

darkGray            :: DColour3
darkGray            = C3 0.663  0.663  0.663

darkGreen           :: DColour3
darkGreen           = C3 0.0  0.392  0.0

darkGrey            :: DColour3
darkGrey            = C3 0.663  0.663  0.663

darkKhaki           :: DColour3
darkKhaki           = C3 0.741  0.718  0.420

darkMagenta         :: DColour3
darkMagenta         = C3 0.545  0.0  0.545

darkOliveGreen      :: DColour3
darkOliveGreen      = C3 0.333  0.420  0.184

darkOrange          :: DColour3
darkOrange          = C3 1.0  0.549  0.0

darkOrchid          :: DColour3
darkOrchid          = C3 0.600  0.196  0.800

darkRed             :: DColour3
darkRed             = C3 0.545  0.0  0.0

darkSalmon          :: DColour3
darkSalmon          = C3 0.914  0.588  0.478

darkSeaGreen        :: DColour3
darkSeaGreen        = C3 0.561  0.737  0.561

darkSlateBlue       :: DColour3
darkSlateBlue       = C3 0.282  0.239  0.545

darkSlateGray       :: DColour3
darkSlateGray       = C3 0.184  0.310  0.310

darkSlateGrey       :: DColour3
darkSlateGrey       = C3 0.184  0.310  0.310

darkTurquoise       :: DColour3
darkTurquoise       = C3 0.0  0.808  0.820

darkViolet          :: DColour3
darkViolet          = C3 0.580  0.0  0.827

deepPink            :: DColour3
deepPink            = C3 1.0  0.078  0.576

deepSkyBlue         :: DColour3
deepSkyBlue         = C3 0.0  0.750  1.0

dimGray             :: DColour3
dimGray             = C3 0.412  0.412  0.412

dimGrey             :: DColour3
dimGrey             = C3 0.412  0.412  0.412

dodgerBlue          :: DColour3
dodgerBlue          = C3 0.118  0.565  1.0

firebrick           :: DColour3
firebrick           = C3 0.698  0.133  0.133

floralWhite         :: DColour3
floralWhite         = C3 1.0  0.980  0.941

forestGreen         :: DColour3
forestGreen         = C3 0.133  0.545  0.133

fuchsia             :: DColour3
fuchsia             = C3 1.0  0.0  1.0

gainsboro           :: DColour3
gainsboro           = C3 0.863  0.863  0.863

ghostWhite          :: DColour3
ghostWhite          = C3 0.973  0.973  1.0

gold                :: DColour3
gold                = C3 1.0  0.843  0.0

goldenrod           :: DColour3
goldenrod           = C3 0.855  0.647  0.125

gray                :: DColour3
gray                = C3 0.5  0.5  0.5

grey                :: DColour3
grey                = C3 0.5  0.5  0.5

green               :: DColour3
green               = C3 0.0  0.5  0.0

greenYellow         :: DColour3
greenYellow         = C3 0.678  1.0  0.184

honeydew            :: DColour3
honeydew            = C3 0.941  1.0  0.941

hotPink             :: DColour3
hotPink             = C3 1.0  0.412  0.706

indianRed           :: DColour3
indianRed           = C3 0.804  0.361  0.361

indigo              :: DColour3
indigo              = C3 0.294  0.0  0.510

ivory               :: DColour3
ivory               = C3 1.0  1.0  0.941

khaki               :: DColour3
khaki               = C3 0.941  0.902  0.549

lavender            :: DColour3
lavender            = C3 0.902  0.902  0.980

lavenderBlush       :: DColour3
lavenderBlush       = C3 1.0  0.941  0.961

lawnGreen           :: DColour3
lawnGreen           = C3 0.486  0.988  0.0

lemonChiffon        :: DColour3
lemonChiffon        = C3 1.0  0.980  0.804

lightBlue           :: DColour3
lightBlue           = C3 0.678  0.847  0.902

lightCoral          :: DColour3
lightCoral          = C3 0.941  0.5  0.5

lightCyan           :: DColour3
lightCyan           = C3 0.878  1.0  1.0

lightGoldenrodYellow  :: DColour3
lightGoldenrodYellow  = C3 0.980  0.980  0.824

lightGray           :: DColour3
lightGray           = C3 0.827  0.827  0.827

lightGreen          :: DColour3
lightGreen          = C3 0.565  0.933  0.565

lightGrey           :: DColour3
lightGrey           = C3 0.827  0.827  0.827

lightPink           :: DColour3
lightPink           = C3 1.0  0.714  0.757

lightSalmon         :: DColour3
lightSalmon         = C3 1.0  0.627  0.478

lightSeaGreen       :: DColour3
lightSeaGreen       = C3 0.125  0.698  0.666

lightSkyBlue        :: DColour3
lightSkyBlue        = C3 0.529  0.808  0.980

lightSlateGray      :: DColour3
lightSlateGray      = C3 0.466  0.533  0.600

lightSlateGrey      :: DColour3
lightSlateGrey      = C3 0.466  0.533  0.600

lightSteelBlue      :: DColour3
lightSteelBlue      = C3 0.690  0.769  0.871

lightYellow         :: DColour3
lightYellow         = C3 1.0  1.0  0.878

lime                :: DColour3
lime                = C3 0.0  1.0  0.0

limeGreen           :: DColour3
limeGreen           = C3 0.196  0.804  0.196

linen               :: DColour3
linen               = C3 0.980  0.941  0.902

magenta             :: DColour3
magenta             = C3 1.0  0.0  1.0

maroon              :: DColour3
maroon              = C3 0.5  0.0  0.0

mediumAquamarine    :: DColour3
mediumAquamarine    = C3 0.4  0.804  0.666

mediumBlue          :: DColour3
mediumBlue          = C3 0.0  0.0  0.804

mediumOrchid        :: DColour3
mediumOrchid        = C3 0.729  0.333  0.827

mediumPurple        :: DColour3
mediumPurple        = C3 0.576  0.439  0.859

mediumSeaGreen      :: DColour3
mediumSeaGreen      = C3 0.235  0.702  0.443

mediumSlateBlue     :: DColour3
mediumSlateBlue     = C3 0.482  0.408  0.933

mediumSpringGreen   :: DColour3
mediumSpringGreen   = C3 0.0  0.980  0.604

mediumTurquoise     :: DColour3
mediumTurquoise     = C3 0.282  0.820  0.800

mediumVioletRed     :: DColour3
mediumVioletRed     = C3 0.780  0.082  0.522

midnightBlue        :: DColour3
midnightBlue        = C3 0.098  0.098  0.439

mintcream           :: DColour3
mintcream           = C3 0.961  1.0  0.980

mistyrose           :: DColour3
mistyrose           = C3 1.0  0.894  0.882

moccasin            :: DColour3
moccasin            = C3 1.0  0.894  0.710

navajoWhite         :: DColour3
navajoWhite         = C3 1.0  0.871  0.678

navy                :: DColour3
navy                = C3 0.0  0.0  0.5

oldlace             :: DColour3
oldlace             = C3 0.992  0.961  0.902

olive               :: DColour3
olive               = C3 0.5  0.5  0.0

oliveDrab           :: DColour3
oliveDrab           = C3 0.420  0.557  0.137

orange              :: DColour3
orange              = C3 1.0  0.647  0.0

orangeRed           :: DColour3
orangeRed           = C3 1.0  0.271  0.0

orchid              :: DColour3
orchid              = C3 0.855  0.439  0.839

paleGoldenrod       :: DColour3
paleGoldenrod       = C3 0.933  0.910  0.666

paleGreen           :: DColour3
paleGreen           = C3 0.596  0.984  0.596

paleTurquoise       :: DColour3
paleTurquoise       = C3 0.686  0.933  0.933

paleVioletRed       :: DColour3
paleVioletRed       = C3 0.859  0.439  0.576

papayawhip          :: DColour3
papayawhip          = C3 1.0  0.937  0.835

peachpuff           :: DColour3
peachpuff           = C3 1.0  0.855  0.725

peru                :: DColour3
peru                = C3 0.804  0.522  0.247

pink                :: DColour3
pink                = C3 1.0  0.753  0.796

plum                :: DColour3
plum                = C3 0.867  0.627  0.867

powderBlue          :: DColour3
powderBlue          = C3 0.690  0.878  0.902

purple              :: DColour3
purple              = C3 0.5  0.0  0.5

red                 :: DColour3
red                 = C3 1.0  0.0  0.0

rosyBrown           :: DColour3
rosyBrown           = C3 0.737  0.561  0.561

royalBlue           :: DColour3
royalBlue           = C3 0.255  0.412  0.882

saddleBrown         :: DColour3
saddleBrown         = C3 0.545  0.271  0.075

salmon              :: DColour3
salmon              = C3 0.980  0.5  0.447

sandyBrown          :: DColour3
sandyBrown          = C3 0.957  0.643  0.376

seaGreen            :: DColour3
seaGreen            = C3 0.180  0.545  0.341

seashell            :: DColour3
seashell            = C3 1.0  0.961  0.933

sienna              :: DColour3
sienna              = C3 0.627  0.322  0.176

silver              :: DColour3
silver              = C3 0.753  0.753  0.753

skyBlue             :: DColour3
skyBlue             = C3 0.529  0.808  0.922

slateBlue           :: DColour3
slateBlue           = C3 0.416  0.353  0.804

slateGray           :: DColour3
slateGray           = C3 0.439  0.5  0.565

slateGrey           :: DColour3
slateGrey           = C3 0.439  0.5  0.565

snow                :: DColour3
snow                = C3 1.0  0.980  0.980

springGreen         :: DColour3
springGreen         = C3 0.0  1.0  0.498

steelBlue           :: DColour3
steelBlue           = C3 0.275  0.510  0.706

tan                 :: DColour3
tan                 = C3 0.824  0.706  0.549

teal                :: DColour3
teal                = C3 0.0  0.5  0.5

thistle             :: DColour3
thistle             = C3 0.847  0.750  0.847

tomato              :: DColour3
tomato              = C3 1.0  0.388  0.278

turquoise           :: DColour3
turquoise           = C3 0.251  0.878  0.816

violet              :: DColour3
violet              = C3 0.933  0.510  0.933

wheat               :: DColour3
wheat               = C3 0.961  0.871  0.702

white               :: DColour3
white               = C3 1.0  1.0  1.0

whitesmoke          :: DColour3
whitesmoke          = C3 0.961  0.961  0.961

yellow              :: DColour3
yellow              = C3 1.0  1.0  0.0

yellowGreen         :: DColour3
yellowGreen         = C3 0.604  0.804  0.196





