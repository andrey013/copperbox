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


import Wumpus.Core.Colour ( Colour3 )
import Wumpus.Core.Vector ( Vec3(..) )


import Prelude hiding ( tan )
  
  
aliceBlue           :: Colour3
aliceBlue           = V3 0.941  0.973  1.0

antiqueWhite        :: Colour3
antiqueWhite        = V3 0.980  0.922  0.843

aqua                :: Colour3
aqua                = V3 0.0  1.0  1.0

aquamarine          :: Colour3
aquamarine          = V3 0.498  1.0  0.831

azure               :: Colour3
azure               = V3 0.941  1.0  1.0

beige               :: Colour3  
beige               = V3 0.961  0.961  0.863

bisque              :: Colour3
bisque              = V3 1.0  0.894  0.769

black               :: Colour3
black               = V3 0.0  0.0  0.0

blanchedAlmond      :: Colour3
blanchedAlmond      = V3 1.0  0.922  0.804

blue                :: Colour3
blue                = V3 0.0  0.0  1.0

blueViolet          :: Colour3
blueViolet          = V3 0.541  0.169  0.886

brown               :: Colour3
brown               = V3 0.647  0.165  0.165

burlywood           :: Colour3
burlywood           = V3 0.871  0.722  0.529

cadetBlue           :: Colour3
cadetBlue           = V3 0.373  0.620  0.627

chartreuse          :: Colour3
chartreuse          = V3 0.498  1.0  0.0

chocolate           :: Colour3
chocolate           = V3 0.824  0.412  0.118

coral               :: Colour3
coral               = V3 1.0  0.498  0.314

cornflowerBlue      :: Colour3
cornflowerBlue      = V3 0.392  0.584  0.929

cornsilk            :: Colour3
cornsilk            = V3 1.0  0.973  0.863

crimson             :: Colour3
crimson             = V3 0.863  0.078  0.235

cyan                :: Colour3
cyan                = V3 0.0  1.0  1.0

darkBlue            :: Colour3
darkBlue            = V3 0.0  0.0  0.545

darkCyan            :: Colour3
darkCyan            = V3 0.0  0.545  0.545

darkGoldenrod       :: Colour3
darkGoldenrod       = V3 0.722  0.545  0.043

darkGray            :: Colour3
darkGray            = V3 0.663  0.663  0.663

darkGreen           :: Colour3
darkGreen           = V3 0.0  0.392  0.0

darkGrey            :: Colour3
darkGrey            = V3 0.663  0.663  0.663

darkKhaki           :: Colour3
darkKhaki           = V3 0.741  0.718  0.420

darkMagenta         :: Colour3
darkMagenta         = V3 0.545  0.0  0.545

darkOliveGreen      :: Colour3
darkOliveGreen      = V3 0.333  0.420  0.184

darkOrange          :: Colour3
darkOrange          = V3 1.0  0.549  0.0

darkOrchid          :: Colour3
darkOrchid          = V3 0.600  0.196  0.800

darkRed             :: Colour3
darkRed             = V3 0.545  0.0  0.0

darkSalmon          :: Colour3
darkSalmon          = V3 0.914  0.588  0.478

darkSeaGreen        :: Colour3
darkSeaGreen        = V3 0.561  0.737  0.561

darkSlateBlue       :: Colour3
darkSlateBlue       = V3 0.282  0.239  0.545

darkSlateGray       :: Colour3
darkSlateGray       = V3 0.184  0.310  0.310

darkSlateGrey       :: Colour3
darkSlateGrey       = V3 0.184  0.310  0.310

darkTurquoise       :: Colour3
darkTurquoise       = V3 0.0  0.808  0.820

darkViolet          :: Colour3
darkViolet          = V3 0.580  0.0  0.827

deepPink            :: Colour3
deepPink            = V3 1.0  0.078  0.576

deepSkyBlue         :: Colour3
deepSkyBlue         = V3 0.0  0.750  1.0

dimGray             :: Colour3
dimGray             = V3 0.412  0.412  0.412

dimGrey             :: Colour3
dimGrey             = V3 0.412  0.412  0.412

dodgerBlue          :: Colour3
dodgerBlue          = V3 0.118  0.565  1.0

firebrick           :: Colour3
firebrick           = V3 0.698  0.133  0.133

floralWhite         :: Colour3
floralWhite         = V3 1.0  0.980  0.941

forestGreen         :: Colour3
forestGreen         = V3 0.133  0.545  0.133

fuchsia             :: Colour3
fuchsia             = V3 1.0  0.0  1.0

gainsboro           :: Colour3
gainsboro           = V3 0.863  0.863  0.863

ghostWhite          :: Colour3
ghostWhite          = V3 0.973  0.973  1.0

gold                :: Colour3
gold                = V3 1.0  0.843  0.0

goldenrod           :: Colour3
goldenrod           = V3 0.855  0.647  0.125

gray                :: Colour3
gray                = V3 0.5  0.5  0.5

grey                :: Colour3
grey                = V3 0.5  0.5  0.5

green               :: Colour3
green               = V3 0.0  0.5  0.0

greenYellow         :: Colour3
greenYellow         = V3 0.678  1.0  0.184

honeydew            :: Colour3
honeydew            = V3 0.941  1.0  0.941

hotPink             :: Colour3
hotPink             = V3 1.0  0.412  0.706

indianRed           :: Colour3
indianRed           = V3 0.804  0.361  0.361

indigo              :: Colour3
indigo              = V3 0.294  0.0  0.510

ivory               :: Colour3
ivory               = V3 1.0  1.0  0.941

khaki               :: Colour3
khaki               = V3 0.941  0.902  0.549

lavender            :: Colour3
lavender            = V3 0.902  0.902  0.980

lavenderBlush       :: Colour3
lavenderBlush       = V3 1.0  0.941  0.961

lawnGreen           :: Colour3
lawnGreen           = V3 0.486  0.988  0.0

lemonChiffon        :: Colour3
lemonChiffon        = V3 1.0  0.980  0.804

lightBlue           :: Colour3
lightBlue           = V3 0.678  0.847  0.902

lightCoral          :: Colour3
lightCoral          = V3 0.941  0.5  0.5

lightCyan           :: Colour3
lightCyan           = V3 0.878  1.0  1.0

lightGoldenrodYellow  :: Colour3
lightGoldenrodYellow  = V3 0.980  0.980  0.824

lightGray           :: Colour3
lightGray           = V3 0.827  0.827  0.827

lightGreen          :: Colour3
lightGreen          = V3 0.565  0.933  0.565

lightGrey           :: Colour3
lightGrey           = V3 0.827  0.827  0.827

lightPink           :: Colour3
lightPink           = V3 1.0  0.714  0.757

lightSalmon         :: Colour3
lightSalmon         = V3 1.0  0.627  0.478

lightSeaGreen       :: Colour3
lightSeaGreen       = V3 0.125  0.698  0.666

lightSkyBlue        :: Colour3
lightSkyBlue        = V3 0.529  0.808  0.980

lightSlateGray      :: Colour3
lightSlateGray      = V3 0.466  0.533  0.600

lightSlateGrey      :: Colour3
lightSlateGrey      = V3 0.466  0.533  0.600

lightSteelBlue      :: Colour3
lightSteelBlue      = V3 0.690  0.769  0.871

lightYellow         :: Colour3
lightYellow         = V3 1.0  1.0  0.878

lime                :: Colour3
lime                = V3 0.0  1.0  0.0

limeGreen           :: Colour3
limeGreen           = V3 0.196  0.804  0.196

linen               :: Colour3
linen               = V3 0.980  0.941  0.902

magenta             :: Colour3
magenta             = V3 1.0  0.0  1.0

maroon              :: Colour3
maroon              = V3 0.5  0.0  0.0

mediumAquamarine    :: Colour3
mediumAquamarine    = V3 0.4  0.804  0.666

mediumBlue          :: Colour3
mediumBlue          = V3 0.0  0.0  0.804

mediumOrchid        :: Colour3
mediumOrchid        = V3 0.729  0.333  0.827

mediumPurple        :: Colour3
mediumPurple        = V3 0.576  0.439  0.859

mediumSeaGreen      :: Colour3
mediumSeaGreen      = V3 0.235  0.702  0.443

mediumSlateBlue     :: Colour3
mediumSlateBlue     = V3 0.482  0.408  0.933

mediumSpringGreen   :: Colour3
mediumSpringGreen   = V3 0.0  0.980  0.604

mediumTurquoise     :: Colour3
mediumTurquoise     = V3 0.282  0.820  0.800

mediumVioletRed     :: Colour3
mediumVioletRed     = V3 0.780  0.082  0.522

midnightBlue        :: Colour3
midnightBlue        = V3 0.098  0.098  0.439

mintcream           :: Colour3
mintcream           = V3 0.961  1.0  0.980

mistyrose           :: Colour3
mistyrose           = V3 1.0  0.894  0.882

moccasin            :: Colour3
moccasin            = V3 1.0  0.894  0.710

navajoWhite         :: Colour3
navajoWhite         = V3 1.0  0.871  0.678

navy                :: Colour3
navy                = V3 0.0  0.0  0.5

oldlace             :: Colour3
oldlace             = V3 0.992  0.961  0.902

olive               :: Colour3
olive               = V3 0.5  0.5  0.0

oliveDrab           :: Colour3
oliveDrab           = V3 0.420  0.557  0.137

orange              :: Colour3
orange              = V3 1.0  0.647  0.0

orangeRed           :: Colour3
orangeRed           = V3 1.0  0.271  0.0

orchid              :: Colour3
orchid              = V3 0.855  0.439  0.839

paleGoldenrod       :: Colour3
paleGoldenrod       = V3 0.933  0.910  0.666

paleGreen           :: Colour3
paleGreen           = V3 0.596  0.984  0.596

paleTurquoise       :: Colour3
paleTurquoise       = V3 0.686  0.933  0.933

paleVioletRed       :: Colour3
paleVioletRed       = V3 0.859  0.439  0.576

papayawhip          :: Colour3
papayawhip          = V3 1.0  0.937  0.835

peachpuff           :: Colour3
peachpuff           = V3 1.0  0.855  0.725

peru                :: Colour3
peru                = V3 0.804  0.522  0.247

pink                :: Colour3
pink                = V3 1.0  0.753  0.796

plum                :: Colour3
plum                = V3 0.867  0.627  0.867

powderBlue          :: Colour3
powderBlue          = V3 0.690  0.878  0.902

purple              :: Colour3
purple              = V3 0.5  0.0  0.5

red                 :: Colour3
red                 = V3 1.0  0.0  0.0

rosyBrown           :: Colour3
rosyBrown           = V3 0.737  0.561  0.561

royalBlue           :: Colour3
royalBlue           = V3 0.255  0.412  0.882

saddleBrown         :: Colour3
saddleBrown         = V3 0.545  0.271  0.075

salmon              :: Colour3
salmon              = V3 0.980  0.5  0.447

sandyBrown          :: Colour3
sandyBrown          = V3 0.957  0.643  0.376

seaGreen            :: Colour3
seaGreen            = V3 0.180  0.545  0.341

seashell            :: Colour3
seashell            = V3 1.0  0.961  0.933

sienna              :: Colour3
sienna              = V3 0.627  0.322  0.176

silver              :: Colour3
silver              = V3 0.753  0.753  0.753

skyBlue             :: Colour3
skyBlue             = V3 0.529  0.808  0.922

slateBlue           :: Colour3
slateBlue           = V3 0.416  0.353  0.804

slateGray           :: Colour3
slateGray           = V3 0.439  0.5  0.565

slateGrey           :: Colour3
slateGrey           = V3 0.439  0.5  0.565

snow                :: Colour3
snow                = V3 1.0  0.980  0.980

springGreen         :: Colour3
springGreen         = V3 0.0  1.0  0.498

steelBlue           :: Colour3
steelBlue           = V3 0.275  0.510  0.706

tan                 :: Colour3
tan                 = V3 0.824  0.706  0.549

teal                :: Colour3
teal                = V3 0.0  0.5  0.5

thistle             :: Colour3
thistle             = V3 0.847  0.750  0.847

tomato              :: Colour3
tomato              = V3 1.0  0.388  0.278

turquoise           :: Colour3
turquoise           = V3 0.251  0.878  0.816

violet              :: Colour3
violet              = V3 0.933  0.510  0.933

wheat               :: Colour3
wheat               = V3 0.961  0.871  0.702

white               :: Colour3
white               = V3 1.0  1.0  1.0

whitesmoke          :: Colour3
whitesmoke          = V3 0.961  0.961  0.961

yellow              :: Colour3
yellow              = V3 1.0  1.0  0.0

yellowGreen         :: Colour3
yellowGreen         = V3 0.604  0.804  0.196





