{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Drawing.X11Colours
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- The X11 \'named colours\', but as rgb [0,1] values 
--
--------------------------------------------------------------------------------

module Wumpus.Drawing.X11Colours (
    -- * Named X11 colours
    antiqueWhite1,
    antiqueWhite2,
    antiqueWhite3,
    antiqueWhite4,
    aquamarine1,
    aquamarine2,
    aquamarine3,
    aquamarine4,
    azure1,
    azure2,
    azure3,
    azure4,
    bisque1,
    bisque2,
    bisque3,
    bisque4,
    blue1,
    blue2,
    blue3,
    blue4,
    brown1,
    brown2,
    brown3,
    brown4,
    burlywood1,
    burlywood2,
    burlywood3,
    burlywood4,
    cadetBlue1,
    cadetBlue2,
    cadetBlue3,
    cadetBlue4,
    chartreuse1,
    chartreuse2,
    chartreuse3,
    chartreuse4,
    chocolate1,
    chocolate2,
    chocolate3,
    chocolate4,
    coral1,
    coral2,
    coral3,
    coral4,
    cornsilk1,
    cornsilk2,
    cornsilk3,
    cornsilk4,
    cyan1,
    cyan2,
    cyan3,
    cyan4,
    darkGoldenrod1,
    darkGoldenrod2,
    darkGoldenrod3,
    darkGoldenrod4,
    darkOliveGreen1,
    darkOliveGreen2,
    darkOliveGreen3,
    darkOliveGreen4,
    darkOrange1,
    darkOrange2,
    darkOrange3,
    darkOrange4,
    darkOrchid1,
    darkOrchid2,
    darkOrchid3,
    darkOrchid4,
    darkSeaGreen1,
    darkSeaGreen2,
    darkSeaGreen3,
    darkSeaGreen4,
    darkSlateGray1,
    darkSlateGray2,
    darkSlateGray3,
    darkSlateGray4,
    deepPink1,
    deepPink2,
    deepPink3,
    deepPink4,
    deepSkyBlue1,
    deepSkyBlue2,
    deepSkyBlue3,
    deepSkyBlue4,
    dodgerBlue1,
    dodgerBlue2,
    dodgerBlue3,
    dodgerBlue4,
    firebrick1,
    firebrick2,
    firebrick3,
    firebrick4,
    gold1,
    gold2,
    gold3,
    gold4,
    goldenrod1,
    goldenrod2,
    goldenrod3,
    goldenrod4,
    green1,
    green2,
    green3,
    green4,
    honeydew1,
    honeydew2,
    honeydew3,
    honeydew4,
    hotPink1,
    hotPink2,
    hotPink3,
    hotPink4,
    indianRed1,
    indianRed2,
    indianRed3,
    indianRed4,
    ivory1,
    ivory2,
    ivory3,
    ivory4,
    khaki1,
    khaki2,
    khaki3,
    khaki4,
    lavenderBlush1,
    lavenderBlush2,
    lavenderBlush3,
    lavenderBlush4,
    lemonChiffon1,
    lemonChiffon2,
    lemonChiffon3,
    lemonChiffon4,
    lightBlue1,
    lightBlue2,
    lightBlue3,
    lightBlue4,
    lightCyan1,
    lightCyan2,
    lightCyan3,
    lightCyan4,
    lightGoldenrod1,
    lightGoldenrod2,
    lightGoldenrod3,
    lightGoldenrod4,
    lightPink1,
    lightPink2,
    lightPink3,
    lightPink4,
    lightSalmon1,
    lightSalmon2,
    lightSalmon3,
    lightSalmon4,
    lightSkyBlue1,
    lightSkyBlue2,
    lightSkyBlue3,
    lightSkyBlue4,
    lightSteelBlue1,
    lightSteelBlue2,
    lightSteelBlue3,
    lightSteelBlue4,
    lightYellow1,
    lightYellow2,
    lightYellow3,
    lightYellow4,
    magenta1,
    magenta2,
    magenta3,
    magenta4,
    maroon1,
    maroon2,
    maroon3,
    maroon4,
    mediumOrchid1,
    mediumOrchid2,
    mediumOrchid3,
    mediumOrchid4,
    mediumPurple1,
    mediumPurple2,
    mediumPurple3,
    mediumPurple4,
    mistyRose1,
    mistyRose2,
    mistyRose3,
    mistyRose4,
    navajoWhite1,
    navajoWhite2,
    navajoWhite3,
    navajoWhite4,
    oliveDrab1,
    oliveDrab2,
    oliveDrab3,
    oliveDrab4,
    orange1,
    orange2,
    orange3,
    orange4,
    orangeRed1,
    orangeRed2,
    orangeRed3,
    orangeRed4,
    orchid1,
    orchid2,
    orchid3,
    orchid4,
    paleGreen1,
    paleGreen2,
    paleGreen3,
    paleGreen4,
    paleTurquoise1,
    paleTurquoise2,
    paleTurquoise3,
    paleTurquoise4,
    paleVioletRed1,
    paleVioletRed2,
    paleVioletRed3,
    paleVioletRed4,
    peachPuff1,
    peachPuff2,
    peachPuff3,
    peachPuff4,
    pink1,
    pink2,
    pink3,
    pink4,
    plum1,
    plum2,
    plum3,
    plum4,
    purple1,
    purple2,
    purple3,
    purple4,
    red1,
    red2,
    red3,
    red4,
    rosyBrown1,
    rosyBrown2,
    rosyBrown3,
    rosyBrown4,
    royalBlue1,
    royalBlue2,
    royalBlue3,
    royalBlue4,
    salmon1,
    salmon2,
    salmon3,
    salmon4,
    seaGreen1,
    seaGreen2,
    seaGreen3,
    seaGreen4,
    seashell1,
    seashell2,
    seashell3,
    seashell4,
    sienna1,
    sienna2,
    sienna3,
    sienna4,
    skyBlue1,
    skyBlue2,
    skyBlue3,
    skyBlue4,
    slateBlue1,
    slateBlue2,
    slateBlue3,
    slateBlue4,
    slateGray1,
    slateGray2,
    slateGray3,
    slateGray4,
    snow1,
    snow2,
    snow3,
    snow4,
    springGreen1,
    springGreen2,
    springGreen3,
    springGreen4,
    steelBlue1,
    steelBlue2,
    steelBlue3,
    steelBlue4,
    tan1,
    tan2,
    tan3,
    tan4,
    thistle1,
    thistle2,
    thistle3,
    thistle4,
    tomato1,
    tomato2,
    tomato3,
    tomato4,
    turquoise1,
    turquoise2,
    turquoise3,
    turquoise4,
    violetRed1,
    violetRed2,
    violetRed3,
    violetRed4,
    wheat1,
    wheat2,
    wheat3,
    wheat4,
    yellow1,
    yellow2,
    yellow3,
    yellow4,
    gray0,
    green0,
    grey0,
    maroon0,
    purple0,

  ) where

import Wumpus.Core.Colour ( Colour3(..), DColour3 )


antiqueWhite1           :: DColour3
antiqueWhite2           :: DColour3
antiqueWhite3           :: DColour3
antiqueWhite4           :: DColour3
antiqueWhite1           = C3 1   0.936   0.86
antiqueWhite2           = C3 0.932   0.875   0.8
antiqueWhite3           = C3 0.804   0.752   0.69
antiqueWhite4           = C3 0.545   0.512   0.47

aquamarine1             :: DColour3
aquamarine2             :: DColour3
aquamarine3             :: DColour3
aquamarine4             :: DColour3
aquamarine1             = C3 0.498  1   0.83
aquamarine2             = C3 0.464   0.932   0.776
aquamarine3             = C3 0.4   0.804   0.668
aquamarine4             = C3 0.27   0.545   0.455

azure1                  :: DColour3
azure2                  :: DColour3
azure3                  :: DColour3
azure4                  :: DColour3
azure1                  = C3 0.94  1  1
azure2                  = C3 0.88   0.932   0.932
azure3                  = C3 0.756   0.804   0.804
azure4                  = C3 0.512   0.545   0.545

bisque1                 :: DColour3
bisque2                 :: DColour3
bisque3                 :: DColour3
bisque4                 :: DColour3
bisque1                 = C3 1   0.894   0.77
bisque2                 = C3 0.932   0.835   0.716
bisque3                 = C3 0.804   0.716   0.62
bisque4                 = C3 0.545   0.49   0.42

blue1                   :: DColour3
blue2                   :: DColour3
blue3                   :: DColour3
blue4                   :: DColour3
blue1                   = C3 0  0  1
blue2                   = C3 0  0   0.932
blue3                   = C3 0  0   0.804
blue4                   = C3 0  0   0.545

brown1                  :: DColour3
brown2                  :: DColour3
brown3                  :: DColour3
brown4                  :: DColour3
brown1                  = C3 1   0.25   0.25
brown2                  = C3 0.932   0.23   0.23
brown3                  = C3 0.804   0.2   0.2
brown4                  = C3 0.545   0.136   0.136

burlywood1              :: DColour3
burlywood2              :: DColour3
burlywood3              :: DColour3
burlywood4              :: DColour3
burlywood1              = C3 1   0.828   0.608
burlywood2              = C3 0.932   0.772   0.57
burlywood3              = C3 0.804   0.668   0.49
burlywood4              = C3 0.545   0.45   0.332

cadetBlue1              :: DColour3
cadetBlue2              :: DColour3
cadetBlue3              :: DColour3
cadetBlue4              :: DColour3
cadetBlue1              = C3 0.596   0.96  1
cadetBlue2              = C3 0.556   0.898   0.932
cadetBlue3              = C3 0.48   0.772   0.804
cadetBlue4              = C3 0.325   0.525   0.545

chartreuse1             :: DColour3
chartreuse2             :: DColour3
chartreuse3             :: DColour3
chartreuse4             :: DColour3
chartreuse1             = C3 0.498  1  0
chartreuse2             = C3 0.464   0.932  0
chartreuse3             = C3 0.4   0.804  0
chartreuse4             = C3 0.27   0.545  0

chocolate1              :: DColour3
chocolate2              :: DColour3
chocolate3              :: DColour3
chocolate4              :: DColour3
chocolate1              = C3 1   0.498   0.14
chocolate2              = C3 0.932   0.464   0.13
chocolate3              = C3 0.804   0.4   0.112
chocolate4              = C3 0.545   0.27   0.075

coral1                  :: DColour3
coral2                  :: DColour3
coral3                  :: DColour3
coral4                  :: DColour3
coral1                  = C3 1   0.448   0.336
coral2                  = C3 0.932   0.415   0.312
coral3                  = C3 0.804   0.356   0.27
coral4                  = C3 0.545   0.244   0.185

cornsilk1               :: DColour3
cornsilk2               :: DColour3
cornsilk3               :: DColour3
cornsilk4               :: DColour3
cornsilk1               = C3 1   0.972   0.864
cornsilk2               = C3 0.932   0.91   0.804
cornsilk3               = C3 0.804   0.785   0.694
cornsilk4               = C3 0.545   0.532   0.47

cyan1                   :: DColour3
cyan2                   :: DColour3
cyan3                   :: DColour3
cyan4                   :: DColour3
cyan1                   = C3 0   1   1
cyan2                   = C3 0   0.932   0.932
cyan3                   = C3 0   0.804   0.804
cyan4                   = C3 0   0.545   0.545

darkGoldenrod1          :: DColour3
darkGoldenrod2          :: DColour3
darkGoldenrod3          :: DColour3
darkGoldenrod4          :: DColour3
darkGoldenrod1          = C3 1   0.725   0.06
darkGoldenrod2          = C3 0.932   0.68   0.055
darkGoldenrod3          = C3 0.804   0.585   0.048
darkGoldenrod4          = C3 0.545   0.396   0.03

darkOliveGreen1         :: DColour3
darkOliveGreen2         :: DColour3
darkOliveGreen3         :: DColour3
darkOliveGreen4         :: DColour3
darkOliveGreen1         = C3 0.792  1   0.44
darkOliveGreen2         = C3 0.736   0.932   0.408
darkOliveGreen3         = C3 0.635   0.804   0.352
darkOliveGreen4         = C3 0.43   0.545   0.24

darkOrange1             :: DColour3
darkOrange2             :: DColour3
darkOrange3             :: DColour3
darkOrange4             :: DColour3
darkOrange1             = C3 1   0.498  0
darkOrange2             = C3 0.932   0.464  0
darkOrange3             = C3 0.804   0.4  0
darkOrange4             = C3 0.545   0.27  0

darkOrchid1             :: DColour3
darkOrchid2             :: DColour3
darkOrchid3             :: DColour3
darkOrchid4             :: DColour3
darkOrchid1             = C3 0.75   0.244  1
darkOrchid2             = C3 0.698   0.228   0.932
darkOrchid3             = C3 0.604   0.196   0.804
darkOrchid4             = C3 0.408   0.132   0.545

darkSeaGreen1           :: DColour3
darkSeaGreen2           :: DColour3
darkSeaGreen3           :: DColour3
darkSeaGreen4           :: DColour3
darkSeaGreen1           = C3 0.756  1   0.756
darkSeaGreen2           = C3 0.705   0.932   0.705
darkSeaGreen3           = C3 0.608   0.804   0.608
darkSeaGreen4           = C3 0.41   0.545   0.41

darkSlateGray1          :: DColour3
darkSlateGray2          :: DColour3
darkSlateGray3          :: DColour3
darkSlateGray4          :: DColour3
darkSlateGray1          = C3 0.592  1  1
darkSlateGray2          = C3 0.552   0.932   0.932
darkSlateGray3          = C3 0.475   0.804   0.804
darkSlateGray4          = C3 0.32   0.545   0.545

deepPink1               :: DColour3
deepPink2               :: DColour3
deepPink3               :: DColour3
deepPink4               :: DColour3
deepPink1               = C3 1   0.08   0.576
deepPink2               = C3 0.932   0.07   0.536
deepPink3               = C3 0.804   0.064   0.464
deepPink4               = C3 0.545   0.04   0.312

deepSkyBlue1            :: DColour3
deepSkyBlue2            :: DColour3
deepSkyBlue3            :: DColour3
deepSkyBlue4            :: DColour3
deepSkyBlue1            = C3 0   0.75  1
deepSkyBlue2            = C3 0   0.698   0.932
deepSkyBlue3            = C3 0   0.604   0.804
deepSkyBlue4            = C3 0   0.408   0.545

dodgerBlue1             :: DColour3
dodgerBlue2             :: DColour3
dodgerBlue3             :: DColour3
dodgerBlue4             :: DColour3
dodgerBlue1             = C3 0.116   0.565  1
dodgerBlue2             = C3 0.11   0.525   0.932
dodgerBlue3             = C3 0.094   0.455   0.804
dodgerBlue4             = C3 0.064   0.305   0.545

firebrick1              :: DColour3
firebrick2              :: DColour3
firebrick3              :: DColour3
firebrick4              :: DColour3
firebrick1              = C3 1   0.19   0.19
firebrick2              = C3 0.932   0.172   0.172
firebrick3              = C3 0.804   0.15   0.15
firebrick4              = C3 0.545   0.1   0.1

gold1                   :: DColour3
gold2                   :: DColour3
gold3                   :: DColour3
gold4                   :: DColour3
gold1                   = C3 1   0.844  0
gold2                   = C3 0.932   0.79  0
gold3                   = C3 0.804   0.68  0
gold4                   = C3 0.545   0.46  0

goldenrod1              :: DColour3
goldenrod2              :: DColour3
goldenrod3              :: DColour3
goldenrod4              :: DColour3
goldenrod1              = C3 1   0.756   0.145
goldenrod2              = C3 0.932   0.705   0.132
goldenrod3              = C3 0.804   0.608   0.112
goldenrod4              = C3 0.545   0.41   0.08

green1                  :: DColour3
green2                  :: DColour3
green3                  :: DColour3
green4                  :: DColour3
green1                  = C3 0  1  0
green2                  = C3 0   0.932  0
green3                  = C3 0   0.804  0
green4                  = C3 0   0.545  0

honeydew1               :: DColour3
honeydew2               :: DColour3
honeydew3               :: DColour3
honeydew4               :: DColour3
honeydew1               = C3 0.94  1   0.94
honeydew2               = C3 0.88   0.932   0.88
honeydew3               = C3 0.756   0.804   0.756
honeydew4               = C3 0.512   0.545   0.512

hotPink1                :: DColour3
hotPink2                :: DColour3
hotPink3                :: DColour3
hotPink4                :: DColour3
hotPink1                = C3 1   0.43   0.705
hotPink2                = C3 0.932   0.415   0.655
hotPink3                = C3 0.804   0.376   0.565
hotPink4                = C3 0.545   0.228   0.385

indianRed1              :: DColour3
indianRed2              :: DColour3
indianRed3              :: DColour3
indianRed4              :: DColour3
indianRed1              = C3 1   0.415   0.415
indianRed2              = C3 0.932   0.39   0.39
indianRed3              = C3 0.804   0.332   0.332
indianRed4              = C3 0.545   0.228   0.228

ivory1                  :: DColour3
ivory2                  :: DColour3
ivory3                  :: DColour3
ivory4                  :: DColour3
ivory1                  = C3 1   1   0.94
ivory2                  = C3 0.932   0.932   0.88
ivory3                  = C3 0.804   0.804   0.756
ivory4                  = C3 0.545   0.545   0.512

khaki1                  :: DColour3
khaki2                  :: DColour3
khaki3                  :: DColour3
khaki4                  :: DColour3
khaki1                  = C3 1   0.965   0.56
khaki2                  = C3 0.932   0.9   0.52
khaki3                  = C3 0.804   0.776   0.45
khaki4                  = C3 0.545   0.525   0.305

lavenderBlush1          :: DColour3
lavenderBlush2          :: DColour3
lavenderBlush3          :: DColour3
lavenderBlush4          :: DColour3
lavenderBlush1          = C3 1   0.94   0.96
lavenderBlush2          = C3 0.932   0.88   0.898
lavenderBlush3          = C3 0.804   0.756   0.772
lavenderBlush4          = C3 0.545   0.512   0.525

lemonChiffon1           :: DColour3
lemonChiffon2           :: DColour3
lemonChiffon3           :: DColour3
lemonChiffon4           :: DColour3
lemonChiffon1           = C3 1   0.98   0.804
lemonChiffon2           = C3 0.932   0.912   0.75
lemonChiffon3           = C3 0.804   0.79   0.648
lemonChiffon4           = C3 0.545   0.536   0.44

lightBlue1              :: DColour3
lightBlue2              :: DColour3
lightBlue3              :: DColour3
lightBlue4              :: DColour3
lightBlue1              = C3 0.75   0.936  1
lightBlue2              = C3 0.698   0.875   0.932
lightBlue3              = C3 0.604   0.752   0.804
lightBlue4              = C3 0.408   0.512   0.545

lightCyan1              :: DColour3
lightCyan2              :: DColour3
lightCyan3              :: DColour3
lightCyan4              :: DColour3
lightCyan1              = C3 0.88  1  1
lightCyan2              = C3 0.82   0.932   0.932
lightCyan3              = C3 0.705   0.804   0.804
lightCyan4              = C3 0.48   0.545   0.545

lightGoldenrod1         :: DColour3
lightGoldenrod2         :: DColour3
lightGoldenrod3         :: DColour3
lightGoldenrod4         :: DColour3
lightGoldenrod1         = C3 1   0.925   0.545
lightGoldenrod2         = C3 0.932   0.864   0.51
lightGoldenrod3         = C3 0.804   0.745   0.44
lightGoldenrod4         = C3 0.545   0.505   0.298

lightPink1              :: DColour3
lightPink2              :: DColour3
lightPink3              :: DColour3
lightPink4              :: DColour3
lightPink1              = C3 1   0.684   0.725
lightPink2              = C3 0.932   0.635   0.68
lightPink3              = C3 0.804   0.55   0.585
lightPink4              = C3 0.545   0.372   0.396

lightSalmon1            :: DColour3
lightSalmon2            :: DColour3
lightSalmon3            :: DColour3
lightSalmon4            :: DColour3
lightSalmon1            = C3 1   0.628   0.48
lightSalmon2            = C3 0.932   0.585   0.448
lightSalmon3            = C3 0.804   0.505   0.385
lightSalmon4            = C3 0.545   0.34   0.26

lightSkyBlue1           :: DColour3
lightSkyBlue2           :: DColour3
lightSkyBlue3           :: DColour3
lightSkyBlue4           :: DColour3
lightSkyBlue1           = C3 0.69   0.888  1
lightSkyBlue2           = C3 0.644   0.828   0.932
lightSkyBlue3           = C3 0.552   0.712   0.804
lightSkyBlue4           = C3 0.376   0.484   0.545

lightSteelBlue1         :: DColour3
lightSteelBlue2         :: DColour3
lightSteelBlue3         :: DColour3
lightSteelBlue4         :: DColour3
lightSteelBlue1         = C3 0.792   0.884  1
lightSteelBlue2         = C3 0.736   0.824   0.932
lightSteelBlue3         = C3 0.635   0.71   0.804
lightSteelBlue4         = C3 0.43   0.484   0.545

lightYellow1            :: DColour3
lightYellow2            :: DColour3
lightYellow3            :: DColour3
lightYellow4            :: DColour3
lightYellow1            = C3 1  1   0.88
lightYellow2            = C3 0.932   0.932   0.82
lightYellow3            = C3 0.804   0.804   0.705
lightYellow4            = C3 0.545   0.545   0.48

magenta1                :: DColour3
magenta2                :: DColour3
magenta3                :: DColour3
magenta4                :: DColour3
magenta1                = C3 1  0  1
magenta2                = C3 0.932  0   0.932
magenta3                = C3 0.804  0   0.804
magenta4                = C3 0.545  0   0.545

maroon1                 :: DColour3
maroon2                 :: DColour3
maroon3                 :: DColour3
maroon4                 :: DColour3
maroon1                 = C3 1   0.204   0.7
maroon2                 = C3 0.932   0.19   0.655
maroon3                 = C3 0.804   0.16   0.565
maroon4                 = C3 0.545   0.11   0.385

mediumOrchid1           :: DColour3
mediumOrchid2           :: DColour3
mediumOrchid3           :: DColour3
mediumOrchid4           :: DColour3
mediumOrchid1           = C3 0.88   0.4  1
mediumOrchid2           = C3 0.82   0.372   0.932
mediumOrchid3           = C3 0.705   0.32   0.804
mediumOrchid4           = C3 0.48   0.215   0.545

mediumPurple1           :: DColour3
mediumPurple2           :: DColour3
mediumPurple3           :: DColour3
mediumPurple4           :: DColour3
mediumPurple1           = C3 0.67   0.51  1
mediumPurple2           = C3 0.624   0.475   0.932
mediumPurple3           = C3 0.536   0.408   0.804
mediumPurple4           = C3 0.365   0.28   0.545

mistyRose1              :: DColour3
mistyRose2              :: DColour3
mistyRose3              :: DColour3
mistyRose4              :: DColour3
mistyRose1              = C3 1   0.894   0.884
mistyRose2              = C3 0.932   0.835   0.824
mistyRose3              = C3 0.804   0.716   0.71
mistyRose4              = C3 0.545   0.49   0.484

navajoWhite1            :: DColour3
navajoWhite2            :: DColour3
navajoWhite3            :: DColour3
navajoWhite4            :: DColour3
navajoWhite1            = C3 1   0.87   0.68
navajoWhite2            = C3 0.932   0.81   0.63
navajoWhite3            = C3 0.804   0.7   0.545
navajoWhite4            = C3 0.545   0.475   0.37

oliveDrab1              :: DColour3
oliveDrab2              :: DColour3
oliveDrab3              :: DColour3
oliveDrab4              :: DColour3
oliveDrab1              = C3 0.752  1   0.244
oliveDrab2              = C3 0.7   0.932   0.228
oliveDrab3              = C3 0.604   0.804   0.196
oliveDrab4              = C3 0.41   0.545   0.132

orange1                 :: DColour3
orange2                 :: DColour3
orange3                 :: DColour3
orange4                 :: DColour3
orange1                 = C3 1   0.648  0
orange2                 = C3 0.932   0.604  0
orange3                 = C3 0.804   0.52  0
orange4                 = C3 0.545   0.352  0

orangeRed1              :: DColour3
orangeRed2              :: DColour3
orangeRed3              :: DColour3
orangeRed4              :: DColour3
orangeRed1              = C3 1   0.27  0
orangeRed2              = C3 0.932   0.25  0
orangeRed3              = C3 0.804   0.215  0
orangeRed4              = C3 0.545   0.145  0

orchid1                 :: DColour3
orchid2                 :: DColour3
orchid3                 :: DColour3
orchid4                 :: DColour3
orchid1                 = C3 1   0.512   0.98
orchid2                 = C3 0.932   0.48   0.912
orchid3                 = C3 0.804   0.41   0.79
orchid4                 = C3 0.545   0.28   0.536

paleGreen1              :: DColour3
paleGreen2              :: DColour3
paleGreen3              :: DColour3
paleGreen4              :: DColour3
paleGreen1              = C3 0.604  1   0.604
paleGreen2              = C3 0.565   0.932   0.565
paleGreen3              = C3 0.488   0.804   0.488
paleGreen4              = C3 0.33   0.545   0.33

paleTurquoise1          :: DColour3
paleTurquoise2          :: DColour3
paleTurquoise3          :: DColour3
paleTurquoise4          :: DColour3
paleTurquoise1          = C3 0.732  1  1
paleTurquoise2          = C3 0.684   0.932   0.932
paleTurquoise3          = C3 0.59   0.804   0.804
paleTurquoise4          = C3 0.4   0.545   0.545

paleVioletRed1          :: DColour3
paleVioletRed2          :: DColour3
paleVioletRed3          :: DColour3
paleVioletRed4          :: DColour3
paleVioletRed1          = C3 1   0.51   0.67
paleVioletRed2          = C3 0.932   0.475   0.624
paleVioletRed3          = C3 0.804   0.408   0.536
paleVioletRed4          = C3 0.545   0.28   0.365

peachPuff1              :: DColour3
peachPuff2              :: DColour3
peachPuff3              :: DColour3
peachPuff4              :: DColour3
peachPuff1              = C3 1   0.855   0.725
peachPuff2              = C3 0.932   0.796   0.68
peachPuff3              = C3 0.804   0.688   0.585
peachPuff4              = C3 0.545   0.468   0.396

pink1                   :: DColour3
pink2                   :: DColour3
pink3                   :: DColour3
pink4                   :: DColour3
pink1                   = C3 1   0.71   0.772
pink2                   = C3 0.932   0.664   0.72
pink3                   = C3 0.804   0.57   0.62
pink4                   = C3 0.545   0.39   0.424

plum1                   :: DColour3
plum2                   :: DColour3
plum3                   :: DColour3
plum4                   :: DColour3
plum1                   = C3 1   0.732  1
plum2                   = C3 0.932   0.684   0.932
plum3                   = C3 0.804   0.59   0.804
plum4                   = C3 0.545   0.4   0.545

purple1                 :: DColour3
purple2                 :: DColour3
purple3                 :: DColour3
purple4                 :: DColour3
purple1                 = C3 0.608   0.19  1
purple2                 = C3 0.57   0.172   0.932
purple3                 = C3 0.49   0.15   0.804
purple4                 = C3 0.332   0.1   0.545

red1                    :: DColour3
red2                    :: DColour3
red3                    :: DColour3
red4                    :: DColour3
red1                    = C3 1  0  0
red2                    = C3 0.932  0  0
red3                    = C3 0.804  0  0
red4                    = C3 0.545  0  0

rosyBrown1              :: DColour3
rosyBrown2              :: DColour3
rosyBrown3              :: DColour3
rosyBrown4              :: DColour3
rosyBrown1              = C3 1   0.756   0.756
rosyBrown2              = C3 0.932   0.705   0.705
rosyBrown3              = C3 0.804   0.608   0.608
rosyBrown4              = C3 0.545   0.41   0.41

royalBlue1              :: DColour3
royalBlue2              :: DColour3
royalBlue3              :: DColour3
royalBlue4              :: DColour3
royalBlue1              = C3 0.284   0.464  1
royalBlue2              = C3 0.264   0.43   0.932
royalBlue3              = C3 0.228   0.372   0.804
royalBlue4              = C3 0.152   0.25   0.545

salmon1                 :: DColour3
salmon2                 :: DColour3
salmon3                 :: DColour3
salmon4                 :: DColour3
salmon1                 = C3 1   0.55   0.41
salmon2                 = C3 0.932   0.51   0.385
salmon3                 = C3 0.804   0.44   0.33
salmon4                 = C3 0.545   0.298   0.224

seaGreen1               :: DColour3
seaGreen2               :: DColour3
seaGreen3               :: DColour3
seaGreen4               :: DColour3
seaGreen1               = C3 0.33  1   0.624
seaGreen2               = C3 0.305   0.932   0.58
seaGreen3               = C3 0.264   0.804   0.5
seaGreen4               = C3 0.18   0.545   0.34

seashell1               :: DColour3
seashell2               :: DColour3
seashell3               :: DColour3
seashell4               :: DColour3
seashell1               = C3 1   0.96   0.932
seashell2               = C3 0.932   0.898   0.87
seashell3               = C3 0.804   0.772   0.75
seashell4               = C3 0.545   0.525   0.51

sienna1                 :: DColour3
sienna2                 :: DColour3
sienna3                 :: DColour3
sienna4                 :: DColour3
sienna1                 = C3 1   0.51   0.28
sienna2                 = C3 0.932   0.475   0.26
sienna3                 = C3 0.804   0.408   0.224
sienna4                 = C3 0.545   0.28   0.15

skyBlue1                :: DColour3
skyBlue2                :: DColour3
skyBlue3                :: DColour3
skyBlue4                :: DColour3
skyBlue1                = C3 0.53   0.808  1
skyBlue2                = C3 0.494   0.752   0.932
skyBlue3                = C3 0.424   0.65   0.804
skyBlue4                = C3 0.29   0.44   0.545

slateBlue1              :: DColour3
slateBlue2              :: DColour3
slateBlue3              :: DColour3
slateBlue4              :: DColour3
slateBlue1              = C3 0.512   0.435  1
slateBlue2              = C3 0.48   0.404   0.932
slateBlue3              = C3 0.41   0.35   0.804
slateBlue4              = C3 0.28   0.235   0.545

slateGray1              :: DColour3
slateGray2              :: DColour3
slateGray3              :: DColour3
slateGray4              :: DColour3
slateGray1              = C3 0.776   0.888  1
slateGray2              = C3 0.725   0.828   0.932
slateGray3              = C3 0.624   0.712   0.804
slateGray4              = C3 0.424   0.484   0.545

snow1                   :: DColour3
snow2                   :: DColour3
snow3                   :: DColour3
snow4                   :: DColour3
snow1                   = C3 1   0.98   0.98
snow2                   = C3 0.932   0.912   0.912
snow3                   = C3 0.804   0.79   0.79
snow4                   = C3 0.545   0.536   0.536

springGreen1            :: DColour3
springGreen2            :: DColour3
springGreen3            :: DColour3
springGreen4            :: DColour3
springGreen1            = C3 0  1   0.498
springGreen2            = C3 0   0.932   0.464
springGreen3            = C3 0   0.804   0.4
springGreen4            = C3 0   0.545   0.27

steelBlue1              :: DColour3
steelBlue2              :: DColour3
steelBlue3              :: DColour3
steelBlue4              :: DColour3
steelBlue1              = C3 0.39   0.72  1
steelBlue2              = C3 0.36   0.675   0.932
steelBlue3              = C3 0.31   0.58   0.804
steelBlue4              = C3 0.21   0.392   0.545

tan1                    :: DColour3
tan2                    :: DColour3
tan3                    :: DColour3
tan4                    :: DColour3
tan1                    = C3 1   0.648   0.31
tan2                    = C3 0.932   0.604   0.288
tan3                    = C3 0.804   0.52   0.248
tan4                    = C3 0.545   0.352   0.17

thistle1                :: DColour3
thistle2                :: DColour3
thistle3                :: DColour3
thistle4                :: DColour3
thistle1                = C3 1   0.884  1
thistle2                = C3 0.932   0.824   0.932
thistle3                = C3 0.804   0.71   0.804
thistle4                = C3 0.545   0.484   0.545

tomato1                 :: DColour3
tomato2                 :: DColour3
tomato3                 :: DColour3
tomato4                 :: DColour3
tomato1                 = C3 1   0.39   0.28
tomato2                 = C3 0.932   0.36   0.26
tomato3                 = C3 0.804   0.31   0.224
tomato4                 = C3 0.545   0.21   0.15

turquoise1              :: DColour3
turquoise2              :: DColour3
turquoise3              :: DColour3
turquoise4              :: DColour3
turquoise1              = C3 0   0.96  1
turquoise2              = C3 0   0.898   0.932
turquoise3              = C3 0   0.772   0.804
turquoise4              = C3 0   0.525   0.545

violetRed1              :: DColour3
violetRed2              :: DColour3
violetRed3              :: DColour3
violetRed4              :: DColour3
violetRed1              = C3 1   0.244   0.59
violetRed2              = C3 0.932   0.228   0.55
violetRed3              = C3 0.804   0.196   0.47
violetRed4              = C3 0.545   0.132   0.32

wheat1                  :: DColour3
wheat2                  :: DColour3
wheat3                  :: DColour3
wheat4                  :: DColour3
wheat1                  = C3 1   0.905   0.73
wheat2                  = C3 0.932   0.848   0.684
wheat3                  = C3 0.804   0.73   0.59
wheat4                  = C3 0.545   0.494   0.4

yellow1                 :: DColour3
yellow2                 :: DColour3
yellow3                 :: DColour3
yellow4                 :: DColour3
yellow1                 = C3 1  1  0
yellow2                 = C3 0.932   0.932  0
yellow3                 = C3 0.804   0.804  0
yellow4                 = C3 0.545   0.545  0

gray0                   :: DColour3
green0                  :: DColour3
grey0                   :: DColour3
maroon0                 :: DColour3
purple0                 :: DColour3
gray0                   = C3 0.745  0.745  0.745
green0                  = C3 0      1      0
grey0                   = C3 0.745  0.745  0.745
maroon0                 = C3 0.69   0.19   0.376
purple0                 = C3 0.628  0.125  0.94




