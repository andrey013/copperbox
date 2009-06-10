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

import Wumpus.Core.Colour ( RGB3(..), DRGB )


antiqueWhite1           :: DGRB
antiqueWhite2           :: DGRB
antiqueWhite3           :: DGRB
antiqueWhite4           :: DGRB
antiqueWhite1           = RGB3 1   0.936   0.86
antiqueWhite2           = RGB3 0.932   0.875   0.8
antiqueWhite3           = RGB3 0.804   0.752   0.69
antiqueWhite4           = RGB3 0.545   0.512   0.47

aquamarine1             :: DGRB
aquamarine2             :: DGRB
aquamarine3             :: DGRB
aquamarine4             :: DGRB
aquamarine1             = RGB3 0.498  1   0.83
aquamarine2             = RGB3 0.464   0.932   0.776
aquamarine3             = RGB3 0.4   0.804   0.668
aquamarine4             = RGB3 0.27   0.545   0.455

azure1                  :: DGRB
azure2                  :: DGRB
azure3                  :: DGRB
azure4                  :: DGRB
azure1                  = RGB3 0.94  1  1
azure2                  = RGB3 0.88   0.932   0.932
azure3                  = RGB3 0.756   0.804   0.804
azure4                  = RGB3 0.512   0.545   0.545

bisque1                 :: DGRB
bisque2                 :: DGRB
bisque3                 :: DGRB
bisque4                 :: DGRB
bisque1                 = RGB3 1   0.894   0.77
bisque2                 = RGB3 0.932   0.835   0.716
bisque3                 = RGB3 0.804   0.716   0.62
bisque4                 = RGB3 0.545   0.49   0.42

blue1                   :: DGRB
blue2                   :: DGRB
blue3                   :: DGRB
blue4                   :: DGRB
blue1                   = RGB3 0  0  1
blue2                   = RGB3 0  0   0.932
blue3                   = RGB3 0  0   0.804
blue4                   = RGB3 0  0   0.545

brown1                  :: DGRB
brown2                  :: DGRB
brown3                  :: DGRB
brown4                  :: DGRB
brown1                  = RGB3 1   0.25   0.25
brown2                  = RGB3 0.932   0.23   0.23
brown3                  = RGB3 0.804   0.2   0.2
brown4                  = RGB3 0.545   0.136   0.136

burlywood1              :: DGRB
burlywood2              :: DGRB
burlywood3              :: DGRB
burlywood4              :: DGRB
burlywood1              = RGB3 1   0.828   0.608
burlywood2              = RGB3 0.932   0.772   0.57
burlywood3              = RGB3 0.804   0.668   0.49
burlywood4              = RGB3 0.545   0.45   0.332

cadetBlue1              :: DGRB
cadetBlue2              :: DGRB
cadetBlue3              :: DGRB
cadetBlue4              :: DGRB
cadetBlue1              = RGB3 0.596   0.96  1
cadetBlue2              = RGB3 0.556   0.898   0.932
cadetBlue3              = RGB3 0.48   0.772   0.804
cadetBlue4              = RGB3 0.325   0.525   0.545

chartreuse1             :: DGRB
chartreuse2             :: DGRB
chartreuse3             :: DGRB
chartreuse4             :: DGRB
chartreuse1             = RGB3 0.498  1  0
chartreuse2             = RGB3 0.464   0.932  0
chartreuse3             = RGB3 0.4   0.804  0
chartreuse4             = RGB3 0.27   0.545  0

chocolate1              :: DGRB
chocolate2              :: DGRB
chocolate3              :: DGRB
chocolate4              :: DGRB
chocolate1              = RGB3 1   0.498   0.14
chocolate2              = RGB3 0.932   0.464   0.13
chocolate3              = RGB3 0.804   0.4   0.112
chocolate4              = RGB3 0.545   0.27   0.075

coral1                  :: DGRB
coral2                  :: DGRB
coral3                  :: DGRB
coral4                  :: DGRB
coral1                  = RGB3 1   0.448   0.336
coral2                  = RGB3 0.932   0.415   0.312
coral3                  = RGB3 0.804   0.356   0.27
coral4                  = RGB3 0.545   0.244   0.185

cornsilk1               :: DGRB
cornsilk2               :: DGRB
cornsilk3               :: DGRB
cornsilk4               :: DGRB
cornsilk1               = RGB3 1   0.972   0.864
cornsilk2               = RGB3 0.932   0.91   0.804
cornsilk3               = RGB3 0.804   0.785   0.694
cornsilk4               = RGB3 0.545   0.532   0.47

cyan1                   :: DGRB
cyan2                   :: DGRB
cyan3                   :: DGRB
cyan4                   :: DGRB
cyan1                   = RGB3 0   1   1
cyan2                   = RGB3 0   0.932   0.932
cyan3                   = RGB3 0   0.804   0.804
cyan4                   = RGB3 0   0.545   0.545

darkGoldenrod1          :: DGRB
darkGoldenrod2          :: DGRB
darkGoldenrod3          :: DGRB
darkGoldenrod4          :: DGRB
darkGoldenrod1          = RGB3 1   0.725   0.06
darkGoldenrod2          = RGB3 0.932   0.68   0.055
darkGoldenrod3          = RGB3 0.804   0.585   0.048
darkGoldenrod4          = RGB3 0.545   0.396   0.03

darkOliveGreen1         :: DGRB
darkOliveGreen2         :: DGRB
darkOliveGreen3         :: DGRB
darkOliveGreen4         :: DGRB
darkOliveGreen1         = RGB3 0.792  1   0.44
darkOliveGreen2         = RGB3 0.736   0.932   0.408
darkOliveGreen3         = RGB3 0.635   0.804   0.352
darkOliveGreen4         = RGB3 0.43   0.545   0.24

darkOrange1             :: DGRB
darkOrange2             :: DGRB
darkOrange3             :: DGRB
darkOrange4             :: DGRB
darkOrange1             = RGB3 1   0.498  0
darkOrange2             = RGB3 0.932   0.464  0
darkOrange3             = RGB3 0.804   0.4  0
darkOrange4             = RGB3 0.545   0.27  0

darkOrchid1             :: DGRB
darkOrchid2             :: DGRB
darkOrchid3             :: DGRB
darkOrchid4             :: DGRB
darkOrchid1             = RGB3 0.75   0.244  1
darkOrchid2             = RGB3 0.698   0.228   0.932
darkOrchid3             = RGB3 0.604   0.196   0.804
darkOrchid4             = RGB3 0.408   0.132   0.545

darkSeaGreen1           :: DGRB
darkSeaGreen2           :: DGRB
darkSeaGreen3           :: DGRB
darkSeaGreen4           :: DGRB
darkSeaGreen1           = RGB3 0.756  1   0.756
darkSeaGreen2           = RGB3 0.705   0.932   0.705
darkSeaGreen3           = RGB3 0.608   0.804   0.608
darkSeaGreen4           = RGB3 0.41   0.545   0.41

darkSlateGray1          :: DGRB
darkSlateGray2          :: DGRB
darkSlateGray3          :: DGRB
darkSlateGray4          :: DGRB
darkSlateGray1          = RGB3 0.592  1  1
darkSlateGray2          = RGB3 0.552   0.932   0.932
darkSlateGray3          = RGB3 0.475   0.804   0.804
darkSlateGray4          = RGB3 0.32   0.545   0.545

deepPink1               :: DGRB
deepPink2               :: DGRB
deepPink3               :: DGRB
deepPink4               :: DGRB
deepPink1               = RGB3 1   0.08   0.576
deepPink2               = RGB3 0.932   0.07   0.536
deepPink3               = RGB3 0.804   0.064   0.464
deepPink4               = RGB3 0.545   0.04   0.312

deepSkyBlue1            :: DGRB
deepSkyBlue2            :: DGRB
deepSkyBlue3            :: DGRB
deepSkyBlue4            :: DGRB
deepSkyBlue1            = RGB3 0   0.75  1
deepSkyBlue2            = RGB3 0   0.698   0.932
deepSkyBlue3            = RGB3 0   0.604   0.804
deepSkyBlue4            = RGB3 0   0.408   0.545

dodgerBlue1             :: DGRB
dodgerBlue2             :: DGRB
dodgerBlue3             :: DGRB
dodgerBlue4             :: DGRB
dodgerBlue1             = RGB3 0.116   0.565  1
dodgerBlue2             = RGB3 0.11   0.525   0.932
dodgerBlue3             = RGB3 0.094   0.455   0.804
dodgerBlue4             = RGB3 0.064   0.305   0.545

firebrick1              :: DGRB
firebrick2              :: DGRB
firebrick3              :: DGRB
firebrick4              :: DGRB
firebrick1              = RGB3 1   0.19   0.19
firebrick2              = RGB3 0.932   0.172   0.172
firebrick3              = RGB3 0.804   0.15   0.15
firebrick4              = RGB3 0.545   0.1   0.1

gold1                   :: DGRB
gold2                   :: DGRB
gold3                   :: DGRB
gold4                   :: DGRB
gold1                   = RGB3 1   0.844  0
gold2                   = RGB3 0.932   0.79  0
gold3                   = RGB3 0.804   0.68  0
gold4                   = RGB3 0.545   0.46  0

goldenrod1              :: DGRB
goldenrod2              :: DGRB
goldenrod3              :: DGRB
goldenrod4              :: DGRB
goldenrod1              = RGB3 1   0.756   0.145
goldenrod2              = RGB3 0.932   0.705   0.132
goldenrod3              = RGB3 0.804   0.608   0.112
goldenrod4              = RGB3 0.545   0.41   0.08

green1                  :: DGRB
green2                  :: DGRB
green3                  :: DGRB
green4                  :: DGRB
green1                  = RGB3 0  1  0
green2                  = RGB3 0   0.932  0
green3                  = RGB3 0   0.804  0
green4                  = RGB3 0   0.545  0

honeydew1               :: DGRB
honeydew2               :: DGRB
honeydew3               :: DGRB
honeydew4               :: DGRB
honeydew1               = RGB3 0.94  1   0.94
honeydew2               = RGB3 0.88   0.932   0.88
honeydew3               = RGB3 0.756   0.804   0.756
honeydew4               = RGB3 0.512   0.545   0.512

hotPink1                :: DGRB
hotPink2                :: DGRB
hotPink3                :: DGRB
hotPink4                :: DGRB
hotPink1                = RGB3 1   0.43   0.705
hotPink2                = RGB3 0.932   0.415   0.655
hotPink3                = RGB3 0.804   0.376   0.565
hotPink4                = RGB3 0.545   0.228   0.385

indianRed1              :: DGRB
indianRed2              :: DGRB
indianRed3              :: DGRB
indianRed4              :: DGRB
indianRed1              = RGB3 1   0.415   0.415
indianRed2              = RGB3 0.932   0.39   0.39
indianRed3              = RGB3 0.804   0.332   0.332
indianRed4              = RGB3 0.545   0.228   0.228

ivory1                  :: DGRB
ivory2                  :: DGRB
ivory3                  :: DGRB
ivory4                  :: DGRB
ivory1                  = RGB3 1   1   0.94
ivory2                  = RGB3 0.932   0.932   0.88
ivory3                  = RGB3 0.804   0.804   0.756
ivory4                  = RGB3 0.545   0.545   0.512

khaki1                  :: DGRB
khaki2                  :: DGRB
khaki3                  :: DGRB
khaki4                  :: DGRB
khaki1                  = RGB3 1   0.965   0.56
khaki2                  = RGB3 0.932   0.9   0.52
khaki3                  = RGB3 0.804   0.776   0.45
khaki4                  = RGB3 0.545   0.525   0.305

lavenderBlush1          :: DGRB
lavenderBlush2          :: DGRB
lavenderBlush3          :: DGRB
lavenderBlush4          :: DGRB
lavenderBlush1          = RGB3 1   0.94   0.96
lavenderBlush2          = RGB3 0.932   0.88   0.898
lavenderBlush3          = RGB3 0.804   0.756   0.772
lavenderBlush4          = RGB3 0.545   0.512   0.525

lemonChiffon1           :: DGRB
lemonChiffon2           :: DGRB
lemonChiffon3           :: DGRB
lemonChiffon4           :: DGRB
lemonChiffon1           = RGB3 1   0.98   0.804
lemonChiffon2           = RGB3 0.932   0.912   0.75
lemonChiffon3           = RGB3 0.804   0.79   0.648
lemonChiffon4           = RGB3 0.545   0.536   0.44

lightBlue1              :: DGRB
lightBlue2              :: DGRB
lightBlue3              :: DGRB
lightBlue4              :: DGRB
lightBlue1              = RGB3 0.75   0.936  1
lightBlue2              = RGB3 0.698   0.875   0.932
lightBlue3              = RGB3 0.604   0.752   0.804
lightBlue4              = RGB3 0.408   0.512   0.545

lightCyan1              :: DGRB
lightCyan2              :: DGRB
lightCyan3              :: DGRB
lightCyan4              :: DGRB
lightCyan1              = RGB3 0.88  1  1
lightCyan2              = RGB3 0.82   0.932   0.932
lightCyan3              = RGB3 0.705   0.804   0.804
lightCyan4              = RGB3 0.48   0.545   0.545

lightGoldenrod1         :: DGRB
lightGoldenrod2         :: DGRB
lightGoldenrod3         :: DGRB
lightGoldenrod4         :: DGRB
lightGoldenrod1         = RGB3 1   0.925   0.545
lightGoldenrod2         = RGB3 0.932   0.864   0.51
lightGoldenrod3         = RGB3 0.804   0.745   0.44
lightGoldenrod4         = RGB3 0.545   0.505   0.298

lightPink1              :: DGRB
lightPink2              :: DGRB
lightPink3              :: DGRB
lightPink4              :: DGRB
lightPink1              = RGB3 1   0.684   0.725
lightPink2              = RGB3 0.932   0.635   0.68
lightPink3              = RGB3 0.804   0.55   0.585
lightPink4              = RGB3 0.545   0.372   0.396

lightSalmon1            :: DGRB
lightSalmon2            :: DGRB
lightSalmon3            :: DGRB
lightSalmon4            :: DGRB
lightSalmon1            = RGB3 1   0.628   0.48
lightSalmon2            = RGB3 0.932   0.585   0.448
lightSalmon3            = RGB3 0.804   0.505   0.385
lightSalmon4            = RGB3 0.545   0.34   0.26

lightSkyBlue1           :: DGRB
lightSkyBlue2           :: DGRB
lightSkyBlue3           :: DGRB
lightSkyBlue4           :: DGRB
lightSkyBlue1           = RGB3 0.69   0.888  1
lightSkyBlue2           = RGB3 0.644   0.828   0.932
lightSkyBlue3           = RGB3 0.552   0.712   0.804
lightSkyBlue4           = RGB3 0.376   0.484   0.545

lightSteelBlue1         :: DGRB
lightSteelBlue2         :: DGRB
lightSteelBlue3         :: DGRB
lightSteelBlue4         :: DGRB
lightSteelBlue1         = RGB3 0.792   0.884  1
lightSteelBlue2         = RGB3 0.736   0.824   0.932
lightSteelBlue3         = RGB3 0.635   0.71   0.804
lightSteelBlue4         = RGB3 0.43   0.484   0.545

lightYellow1            :: DGRB
lightYellow2            :: DGRB
lightYellow3            :: DGRB
lightYellow4            :: DGRB
lightYellow1            = RGB3 1  1   0.88
lightYellow2            = RGB3 0.932   0.932   0.82
lightYellow3            = RGB3 0.804   0.804   0.705
lightYellow4            = RGB3 0.545   0.545   0.48

magenta1                :: DGRB
magenta2                :: DGRB
magenta3                :: DGRB
magenta4                :: DGRB
magenta1                = RGB3 1  0  1
magenta2                = RGB3 0.932  0   0.932
magenta3                = RGB3 0.804  0   0.804
magenta4                = RGB3 0.545  0   0.545

maroon1                 :: DGRB
maroon2                 :: DGRB
maroon3                 :: DGRB
maroon4                 :: DGRB
maroon1                 = RGB3 1   0.204   0.7
maroon2                 = RGB3 0.932   0.19   0.655
maroon3                 = RGB3 0.804   0.16   0.565
maroon4                 = RGB3 0.545   0.11   0.385

mediumOrchid1           :: DGRB
mediumOrchid2           :: DGRB
mediumOrchid3           :: DGRB
mediumOrchid4           :: DGRB
mediumOrchid1           = RGB3 0.88   0.4  1
mediumOrchid2           = RGB3 0.82   0.372   0.932
mediumOrchid3           = RGB3 0.705   0.32   0.804
mediumOrchid4           = RGB3 0.48   0.215   0.545

mediumPurple1           :: DGRB
mediumPurple2           :: DGRB
mediumPurple3           :: DGRB
mediumPurple4           :: DGRB
mediumPurple1           = RGB3 0.67   0.51  1
mediumPurple2           = RGB3 0.624   0.475   0.932
mediumPurple3           = RGB3 0.536   0.408   0.804
mediumPurple4           = RGB3 0.365   0.28   0.545

mistyRose1              :: DGRB
mistyRose2              :: DGRB
mistyRose3              :: DGRB
mistyRose4              :: DGRB
mistyRose1              = RGB3 1   0.894   0.884
mistyRose2              = RGB3 0.932   0.835   0.824
mistyRose3              = RGB3 0.804   0.716   0.71
mistyRose4              = RGB3 0.545   0.49   0.484

navajoWhite1            :: DGRB
navajoWhite2            :: DGRB
navajoWhite3            :: DGRB
navajoWhite4            :: DGRB
navajoWhite1            = RGB3 1   0.87   0.68
navajoWhite2            = RGB3 0.932   0.81   0.63
navajoWhite3            = RGB3 0.804   0.7   0.545
navajoWhite4            = RGB3 0.545   0.475   0.37

oliveDrab1              :: DGRB
oliveDrab2              :: DGRB
oliveDrab3              :: DGRB
oliveDrab4              :: DGRB
oliveDrab1              = RGB3 0.752  1   0.244
oliveDrab2              = RGB3 0.7   0.932   0.228
oliveDrab3              = RGB3 0.604   0.804   0.196
oliveDrab4              = RGB3 0.41   0.545   0.132

orange1                 :: DGRB
orange2                 :: DGRB
orange3                 :: DGRB
orange4                 :: DGRB
orange1                 = RGB3 1   0.648  0
orange2                 = RGB3 0.932   0.604  0
orange3                 = RGB3 0.804   0.52  0
orange4                 = RGB3 0.545   0.352  0

orangeRed1              :: DGRB
orangeRed2              :: DGRB
orangeRed3              :: DGRB
orangeRed4              :: DGRB
orangeRed1              = RGB3 1   0.27  0
orangeRed2              = RGB3 0.932   0.25  0
orangeRed3              = RGB3 0.804   0.215  0
orangeRed4              = RGB3 0.545   0.145  0

orchid1                 :: DGRB
orchid2                 :: DGRB
orchid3                 :: DGRB
orchid4                 :: DGRB
orchid1                 = RGB3 1   0.512   0.98
orchid2                 = RGB3 0.932   0.48   0.912
orchid3                 = RGB3 0.804   0.41   0.79
orchid4                 = RGB3 0.545   0.28   0.536

paleGreen1              :: DGRB
paleGreen2              :: DGRB
paleGreen3              :: DGRB
paleGreen4              :: DGRB
paleGreen1              = RGB3 0.604  1   0.604
paleGreen2              = RGB3 0.565   0.932   0.565
paleGreen3              = RGB3 0.488   0.804   0.488
paleGreen4              = RGB3 0.33   0.545   0.33

paleTurquoise1          :: DGRB
paleTurquoise2          :: DGRB
paleTurquoise3          :: DGRB
paleTurquoise4          :: DGRB
paleTurquoise1          = RGB3 0.732  1  1
paleTurquoise2          = RGB3 0.684   0.932   0.932
paleTurquoise3          = RGB3 0.59   0.804   0.804
paleTurquoise4          = RGB3 0.4   0.545   0.545

paleVioletRed1          :: DGRB
paleVioletRed2          :: DGRB
paleVioletRed3          :: DGRB
paleVioletRed4          :: DGRB
paleVioletRed1          = RGB3 1   0.51   0.67
paleVioletRed2          = RGB3 0.932   0.475   0.624
paleVioletRed3          = RGB3 0.804   0.408   0.536
paleVioletRed4          = RGB3 0.545   0.28   0.365

peachPuff1              :: DGRB
peachPuff2              :: DGRB
peachPuff3              :: DGRB
peachPuff4              :: DGRB
peachPuff1              = RGB3 1   0.855   0.725
peachPuff2              = RGB3 0.932   0.796   0.68
peachPuff3              = RGB3 0.804   0.688   0.585
peachPuff4              = RGB3 0.545   0.468   0.396

pink1                   :: DGRB
pink2                   :: DGRB
pink3                   :: DGRB
pink4                   :: DGRB
pink1                   = RGB3 1   0.71   0.772
pink2                   = RGB3 0.932   0.664   0.72
pink3                   = RGB3 0.804   0.57   0.62
pink4                   = RGB3 0.545   0.39   0.424

plum1                   :: DGRB
plum2                   :: DGRB
plum3                   :: DGRB
plum4                   :: DGRB
plum1                   = RGB3 1   0.732  1
plum2                   = RGB3 0.932   0.684   0.932
plum3                   = RGB3 0.804   0.59   0.804
plum4                   = RGB3 0.545   0.4   0.545

purple1                 :: DGRB
purple2                 :: DGRB
purple3                 :: DGRB
purple4                 :: DGRB
purple1                 = RGB3 0.608   0.19  1
purple2                 = RGB3 0.57   0.172   0.932
purple3                 = RGB3 0.49   0.15   0.804
purple4                 = RGB3 0.332   0.1   0.545

red1                    :: DGRB
red2                    :: DGRB
red3                    :: DGRB
red4                    :: DGRB
red1                    = RGB3 1  0  0
red2                    = RGB3 0.932  0  0
red3                    = RGB3 0.804  0  0
red4                    = RGB3 0.545  0  0

rosyBrown1              :: DGRB
rosyBrown2              :: DGRB
rosyBrown3              :: DGRB
rosyBrown4              :: DGRB
rosyBrown1              = RGB3 1   0.756   0.756
rosyBrown2              = RGB3 0.932   0.705   0.705
rosyBrown3              = RGB3 0.804   0.608   0.608
rosyBrown4              = RGB3 0.545   0.41   0.41

royalBlue1              :: DGRB
royalBlue2              :: DGRB
royalBlue3              :: DGRB
royalBlue4              :: DGRB
royalBlue1              = RGB3 0.284   0.464  1
royalBlue2              = RGB3 0.264   0.43   0.932
royalBlue3              = RGB3 0.228   0.372   0.804
royalBlue4              = RGB3 0.152   0.25   0.545

salmon1                 :: DGRB
salmon2                 :: DGRB
salmon3                 :: DGRB
salmon4                 :: DGRB
salmon1                 = RGB3 1   0.55   0.41
salmon2                 = RGB3 0.932   0.51   0.385
salmon3                 = RGB3 0.804   0.44   0.33
salmon4                 = RGB3 0.545   0.298   0.224

seaGreen1               :: DGRB
seaGreen2               :: DGRB
seaGreen3               :: DGRB
seaGreen4               :: DGRB
seaGreen1               = RGB3 0.33  1   0.624
seaGreen2               = RGB3 0.305   0.932   0.58
seaGreen3               = RGB3 0.264   0.804   0.5
seaGreen4               = RGB3 0.18   0.545   0.34

seashell1               :: DGRB
seashell2               :: DGRB
seashell3               :: DGRB
seashell4               :: DGRB
seashell1               = RGB3 1   0.96   0.932
seashell2               = RGB3 0.932   0.898   0.87
seashell3               = RGB3 0.804   0.772   0.75
seashell4               = RGB3 0.545   0.525   0.51

sienna1                 :: DGRB
sienna2                 :: DGRB
sienna3                 :: DGRB
sienna4                 :: DGRB
sienna1                 = RGB3 1   0.51   0.28
sienna2                 = RGB3 0.932   0.475   0.26
sienna3                 = RGB3 0.804   0.408   0.224
sienna4                 = RGB3 0.545   0.28   0.15

skyBlue1                :: DGRB
skyBlue2                :: DGRB
skyBlue3                :: DGRB
skyBlue4                :: DGRB
skyBlue1                = RGB3 0.53   0.808  1
skyBlue2                = RGB3 0.494   0.752   0.932
skyBlue3                = RGB3 0.424   0.65   0.804
skyBlue4                = RGB3 0.29   0.44   0.545

slateBlue1              :: DGRB
slateBlue2              :: DGRB
slateBlue3              :: DGRB
slateBlue4              :: DGRB
slateBlue1              = RGB3 0.512   0.435  1
slateBlue2              = RGB3 0.48   0.404   0.932
slateBlue3              = RGB3 0.41   0.35   0.804
slateBlue4              = RGB3 0.28   0.235   0.545

slateGray1              :: DGRB
slateGray2              :: DGRB
slateGray3              :: DGRB
slateGray4              :: DGRB
slateGray1              = RGB3 0.776   0.888  1
slateGray2              = RGB3 0.725   0.828   0.932
slateGray3              = RGB3 0.624   0.712   0.804
slateGray4              = RGB3 0.424   0.484   0.545

snow1                   :: DGRB
snow2                   :: DGRB
snow3                   :: DGRB
snow4                   :: DGRB
snow1                   = RGB3 1   0.98   0.98
snow2                   = RGB3 0.932   0.912   0.912
snow3                   = RGB3 0.804   0.79   0.79
snow4                   = RGB3 0.545   0.536   0.536

springGreen1            :: DGRB
springGreen2            :: DGRB
springGreen3            :: DGRB
springGreen4            :: DGRB
springGreen1            = RGB3 0  1   0.498
springGreen2            = RGB3 0   0.932   0.464
springGreen3            = RGB3 0   0.804   0.4
springGreen4            = RGB3 0   0.545   0.27

steelBlue1              :: DGRB
steelBlue2              :: DGRB
steelBlue3              :: DGRB
steelBlue4              :: DGRB
steelBlue1              = RGB3 0.39   0.72  1
steelBlue2              = RGB3 0.36   0.675   0.932
steelBlue3              = RGB3 0.31   0.58   0.804
steelBlue4              = RGB3 0.21   0.392   0.545

tan1                    :: DGRB
tan2                    :: DGRB
tan3                    :: DGRB
tan4                    :: DGRB
tan1                    = RGB3 1   0.648   0.31
tan2                    = RGB3 0.932   0.604   0.288
tan3                    = RGB3 0.804   0.52   0.248
tan4                    = RGB3 0.545   0.352   0.17

thistle1                :: DGRB
thistle2                :: DGRB
thistle3                :: DGRB
thistle4                :: DGRB
thistle1                = RGB3 1   0.884  1
thistle2                = RGB3 0.932   0.824   0.932
thistle3                = RGB3 0.804   0.71   0.804
thistle4                = RGB3 0.545   0.484   0.545

tomato1                 :: DGRB
tomato2                 :: DGRB
tomato3                 :: DGRB
tomato4                 :: DGRB
tomato1                 = RGB3 1   0.39   0.28
tomato2                 = RGB3 0.932   0.36   0.26
tomato3                 = RGB3 0.804   0.31   0.224
tomato4                 = RGB3 0.545   0.21   0.15

turquoise1              :: DGRB
turquoise2              :: DGRB
turquoise3              :: DGRB
turquoise4              :: DGRB
turquoise1              = RGB3 0   0.96  1
turquoise2              = RGB3 0   0.898   0.932
turquoise3              = RGB3 0   0.772   0.804
turquoise4              = RGB3 0   0.525   0.545

violetRed1              :: DGRB
violetRed2              :: DGRB
violetRed3              :: DGRB
violetRed4              :: DGRB
violetRed1              = RGB3 1   0.244   0.59
violetRed2              = RGB3 0.932   0.228   0.55
violetRed3              = RGB3 0.804   0.196   0.47
violetRed4              = RGB3 0.545   0.132   0.32

wheat1                  :: DGRB
wheat2                  :: DGRB
wheat3                  :: DGRB
wheat4                  :: DGRB
wheat1                  = RGB3 1   0.905   0.73
wheat2                  = RGB3 0.932   0.848   0.684
wheat3                  = RGB3 0.804   0.73   0.59
wheat4                  = RGB3 0.545   0.494   0.4

yellow1                 :: DGRB
yellow2                 :: DGRB
yellow3                 :: DGRB
yellow4                 :: DGRB
yellow1                 = RGB3 1  1  0
yellow2                 = RGB3 0.932   0.932  0
yellow3                 = RGB3 0.804   0.804  0
yellow4                 = RGB3 0.545   0.545  0

gray0                   :: DGRB
green0                  :: DGRB
grey0                   :: DGRB
maroon0                 :: DGRB
purple0                 :: DGRB
gray0                   = RGB3 0.745  0.745  0.745
green0                  = RGB3 0      1      0
grey0                   = RGB3 0.745  0.745  0.745
maroon0                 = RGB3 0.69   0.19   0.376
purple0                 = RGB3 0.628  0.125  0.94




