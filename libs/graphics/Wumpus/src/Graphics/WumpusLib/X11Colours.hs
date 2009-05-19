{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.WumpusLib.X11Colours
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

module Graphics.WumpusLib.X11Colours (
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

import Graphics.Wumpus.Vector ( Vec3(..) )
import Graphics.Wumpus.Colour ( Colour3 )


antiqueWhite1           :: Colour3
antiqueWhite2           :: Colour3
antiqueWhite3           :: Colour3
antiqueWhite4           :: Colour3
antiqueWhite1           = V3 1   0.936   0.86
antiqueWhite2           = V3 0.932   0.875   0.8
antiqueWhite3           = V3 0.804   0.752   0.69
antiqueWhite4           = V3 0.545   0.512   0.47

aquamarine1             :: Colour3
aquamarine2             :: Colour3
aquamarine3             :: Colour3
aquamarine4             :: Colour3
aquamarine1             = V3 0.498  1   0.83
aquamarine2             = V3 0.464   0.932   0.776
aquamarine3             = V3 0.4   0.804   0.668
aquamarine4             = V3 0.27   0.545   0.455

azure1                  :: Colour3
azure2                  :: Colour3
azure3                  :: Colour3
azure4                  :: Colour3
azure1                  = V3 0.94  1  1
azure2                  = V3 0.88   0.932   0.932
azure3                  = V3 0.756   0.804   0.804
azure4                  = V3 0.512   0.545   0.545

bisque1                 :: Colour3
bisque2                 :: Colour3
bisque3                 :: Colour3
bisque4                 :: Colour3
bisque1                 = V3 1   0.894   0.77
bisque2                 = V3 0.932   0.835   0.716
bisque3                 = V3 0.804   0.716   0.62
bisque4                 = V3 0.545   0.49   0.42

blue1                   :: Colour3
blue2                   :: Colour3
blue3                   :: Colour3
blue4                   :: Colour3
blue1                   = V3 0  0  1
blue2                   = V3 0  0   0.932
blue3                   = V3 0  0   0.804
blue4                   = V3 0  0   0.545

brown1                  :: Colour3
brown2                  :: Colour3
brown3                  :: Colour3
brown4                  :: Colour3
brown1                  = V3 1   0.25   0.25
brown2                  = V3 0.932   0.23   0.23
brown3                  = V3 0.804   0.2   0.2
brown4                  = V3 0.545   0.136   0.136

burlywood1              :: Colour3
burlywood2              :: Colour3
burlywood3              :: Colour3
burlywood4              :: Colour3
burlywood1              = V3 1   0.828   0.608
burlywood2              = V3 0.932   0.772   0.57
burlywood3              = V3 0.804   0.668   0.49
burlywood4              = V3 0.545   0.45   0.332

cadetBlue1              :: Colour3
cadetBlue2              :: Colour3
cadetBlue3              :: Colour3
cadetBlue4              :: Colour3
cadetBlue1              = V3 0.596   0.96  1
cadetBlue2              = V3 0.556   0.898   0.932
cadetBlue3              = V3 0.48   0.772   0.804
cadetBlue4              = V3 0.325   0.525   0.545

chartreuse1             :: Colour3
chartreuse2             :: Colour3
chartreuse3             :: Colour3
chartreuse4             :: Colour3
chartreuse1             = V3 0.498  1  0
chartreuse2             = V3 0.464   0.932  0
chartreuse3             = V3 0.4   0.804  0
chartreuse4             = V3 0.27   0.545  0

chocolate1              :: Colour3
chocolate2              :: Colour3
chocolate3              :: Colour3
chocolate4              :: Colour3
chocolate1              = V3 1   0.498   0.14
chocolate2              = V3 0.932   0.464   0.13
chocolate3              = V3 0.804   0.4   0.112
chocolate4              = V3 0.545   0.27   0.075

coral1                  :: Colour3
coral2                  :: Colour3
coral3                  :: Colour3
coral4                  :: Colour3
coral1                  = V3 1   0.448   0.336
coral2                  = V3 0.932   0.415   0.312
coral3                  = V3 0.804   0.356   0.27
coral4                  = V3 0.545   0.244   0.185

cornsilk1               :: Colour3
cornsilk2               :: Colour3
cornsilk3               :: Colour3
cornsilk4               :: Colour3
cornsilk1               = V3 1   0.972   0.864
cornsilk2               = V3 0.932   0.91   0.804
cornsilk3               = V3 0.804   0.785   0.694
cornsilk4               = V3 0.545   0.532   0.47

cyan1                   :: Colour3
cyan2                   :: Colour3
cyan3                   :: Colour3
cyan4                   :: Colour3
cyan1                   = V3 0   1   1
cyan2                   = V3 0   0.932   0.932
cyan3                   = V3 0   0.804   0.804
cyan4                   = V3 0   0.545   0.545

darkGoldenrod1          :: Colour3
darkGoldenrod2          :: Colour3
darkGoldenrod3          :: Colour3
darkGoldenrod4          :: Colour3
darkGoldenrod1          = V3 1   0.725   0.06
darkGoldenrod2          = V3 0.932   0.68   0.055
darkGoldenrod3          = V3 0.804   0.585   0.048
darkGoldenrod4          = V3 0.545   0.396   0.03

darkOliveGreen1         :: Colour3
darkOliveGreen2         :: Colour3
darkOliveGreen3         :: Colour3
darkOliveGreen4         :: Colour3
darkOliveGreen1         = V3 0.792  1   0.44
darkOliveGreen2         = V3 0.736   0.932   0.408
darkOliveGreen3         = V3 0.635   0.804   0.352
darkOliveGreen4         = V3 0.43   0.545   0.24

darkOrange1             :: Colour3
darkOrange2             :: Colour3
darkOrange3             :: Colour3
darkOrange4             :: Colour3
darkOrange1             = V3 1   0.498  0
darkOrange2             = V3 0.932   0.464  0
darkOrange3             = V3 0.804   0.4  0
darkOrange4             = V3 0.545   0.27  0

darkOrchid1             :: Colour3
darkOrchid2             :: Colour3
darkOrchid3             :: Colour3
darkOrchid4             :: Colour3
darkOrchid1             = V3 0.75   0.244  1
darkOrchid2             = V3 0.698   0.228   0.932
darkOrchid3             = V3 0.604   0.196   0.804
darkOrchid4             = V3 0.408   0.132   0.545

darkSeaGreen1           :: Colour3
darkSeaGreen2           :: Colour3
darkSeaGreen3           :: Colour3
darkSeaGreen4           :: Colour3
darkSeaGreen1           = V3 0.756  1   0.756
darkSeaGreen2           = V3 0.705   0.932   0.705
darkSeaGreen3           = V3 0.608   0.804   0.608
darkSeaGreen4           = V3 0.41   0.545   0.41

darkSlateGray1          :: Colour3
darkSlateGray2          :: Colour3
darkSlateGray3          :: Colour3
darkSlateGray4          :: Colour3
darkSlateGray1          = V3 0.592  1  1
darkSlateGray2          = V3 0.552   0.932   0.932
darkSlateGray3          = V3 0.475   0.804   0.804
darkSlateGray4          = V3 0.32   0.545   0.545

deepPink1               :: Colour3
deepPink2               :: Colour3
deepPink3               :: Colour3
deepPink4               :: Colour3
deepPink1               = V3 1   0.08   0.576
deepPink2               = V3 0.932   0.07   0.536
deepPink3               = V3 0.804   0.064   0.464
deepPink4               = V3 0.545   0.04   0.312

deepSkyBlue1            :: Colour3
deepSkyBlue2            :: Colour3
deepSkyBlue3            :: Colour3
deepSkyBlue4            :: Colour3
deepSkyBlue1            = V3 0   0.75  1
deepSkyBlue2            = V3 0   0.698   0.932
deepSkyBlue3            = V3 0   0.604   0.804
deepSkyBlue4            = V3 0   0.408   0.545

dodgerBlue1             :: Colour3
dodgerBlue2             :: Colour3
dodgerBlue3             :: Colour3
dodgerBlue4             :: Colour3
dodgerBlue1             = V3 0.116   0.565  1
dodgerBlue2             = V3 0.11   0.525   0.932
dodgerBlue3             = V3 0.094   0.455   0.804
dodgerBlue4             = V3 0.064   0.305   0.545

firebrick1              :: Colour3
firebrick2              :: Colour3
firebrick3              :: Colour3
firebrick4              :: Colour3
firebrick1              = V3 1   0.19   0.19
firebrick2              = V3 0.932   0.172   0.172
firebrick3              = V3 0.804   0.15   0.15
firebrick4              = V3 0.545   0.1   0.1

gold1                   :: Colour3
gold2                   :: Colour3
gold3                   :: Colour3
gold4                   :: Colour3
gold1                   = V3 1   0.844  0
gold2                   = V3 0.932   0.79  0
gold3                   = V3 0.804   0.68  0
gold4                   = V3 0.545   0.46  0

goldenrod1              :: Colour3
goldenrod2              :: Colour3
goldenrod3              :: Colour3
goldenrod4              :: Colour3
goldenrod1              = V3 1   0.756   0.145
goldenrod2              = V3 0.932   0.705   0.132
goldenrod3              = V3 0.804   0.608   0.112
goldenrod4              = V3 0.545   0.41   0.08

green1                  :: Colour3
green2                  :: Colour3
green3                  :: Colour3
green4                  :: Colour3
green1                  = V3 0  1  0
green2                  = V3 0   0.932  0
green3                  = V3 0   0.804  0
green4                  = V3 0   0.545  0

honeydew1               :: Colour3
honeydew2               :: Colour3
honeydew3               :: Colour3
honeydew4               :: Colour3
honeydew1               = V3 0.94  1   0.94
honeydew2               = V3 0.88   0.932   0.88
honeydew3               = V3 0.756   0.804   0.756
honeydew4               = V3 0.512   0.545   0.512

hotPink1                :: Colour3
hotPink2                :: Colour3
hotPink3                :: Colour3
hotPink4                :: Colour3
hotPink1                = V3 1   0.43   0.705
hotPink2                = V3 0.932   0.415   0.655
hotPink3                = V3 0.804   0.376   0.565
hotPink4                = V3 0.545   0.228   0.385

indianRed1              :: Colour3
indianRed2              :: Colour3
indianRed3              :: Colour3
indianRed4              :: Colour3
indianRed1              = V3 1   0.415   0.415
indianRed2              = V3 0.932   0.39   0.39
indianRed3              = V3 0.804   0.332   0.332
indianRed4              = V3 0.545   0.228   0.228

ivory1                  :: Colour3
ivory2                  :: Colour3
ivory3                  :: Colour3
ivory4                  :: Colour3
ivory1                  = V3 1   1   0.94
ivory2                  = V3 0.932   0.932   0.88
ivory3                  = V3 0.804   0.804   0.756
ivory4                  = V3 0.545   0.545   0.512

khaki1                  :: Colour3
khaki2                  :: Colour3
khaki3                  :: Colour3
khaki4                  :: Colour3
khaki1                  = V3 1   0.965   0.56
khaki2                  = V3 0.932   0.9   0.52
khaki3                  = V3 0.804   0.776   0.45
khaki4                  = V3 0.545   0.525   0.305

lavenderBlush1          :: Colour3
lavenderBlush2          :: Colour3
lavenderBlush3          :: Colour3
lavenderBlush4          :: Colour3
lavenderBlush1          = V3 1   0.94   0.96
lavenderBlush2          = V3 0.932   0.88   0.898
lavenderBlush3          = V3 0.804   0.756   0.772
lavenderBlush4          = V3 0.545   0.512   0.525

lemonChiffon1           :: Colour3
lemonChiffon2           :: Colour3
lemonChiffon3           :: Colour3
lemonChiffon4           :: Colour3
lemonChiffon1           = V3 1   0.98   0.804
lemonChiffon2           = V3 0.932   0.912   0.75
lemonChiffon3           = V3 0.804   0.79   0.648
lemonChiffon4           = V3 0.545   0.536   0.44

lightBlue1              :: Colour3
lightBlue2              :: Colour3
lightBlue3              :: Colour3
lightBlue4              :: Colour3
lightBlue1              = V3 0.75   0.936  1
lightBlue2              = V3 0.698   0.875   0.932
lightBlue3              = V3 0.604   0.752   0.804
lightBlue4              = V3 0.408   0.512   0.545

lightCyan1              :: Colour3
lightCyan2              :: Colour3
lightCyan3              :: Colour3
lightCyan4              :: Colour3
lightCyan1              = V3 0.88  1  1
lightCyan2              = V3 0.82   0.932   0.932
lightCyan3              = V3 0.705   0.804   0.804
lightCyan4              = V3 0.48   0.545   0.545

lightGoldenrod1         :: Colour3
lightGoldenrod2         :: Colour3
lightGoldenrod3         :: Colour3
lightGoldenrod4         :: Colour3
lightGoldenrod1         = V3 1   0.925   0.545
lightGoldenrod2         = V3 0.932   0.864   0.51
lightGoldenrod3         = V3 0.804   0.745   0.44
lightGoldenrod4         = V3 0.545   0.505   0.298

lightPink1              :: Colour3
lightPink2              :: Colour3
lightPink3              :: Colour3
lightPink4              :: Colour3
lightPink1              = V3 1   0.684   0.725
lightPink2              = V3 0.932   0.635   0.68
lightPink3              = V3 0.804   0.55   0.585
lightPink4              = V3 0.545   0.372   0.396

lightSalmon1            :: Colour3
lightSalmon2            :: Colour3
lightSalmon3            :: Colour3
lightSalmon4            :: Colour3
lightSalmon1            = V3 1   0.628   0.48
lightSalmon2            = V3 0.932   0.585   0.448
lightSalmon3            = V3 0.804   0.505   0.385
lightSalmon4            = V3 0.545   0.34   0.26

lightSkyBlue1           :: Colour3
lightSkyBlue2           :: Colour3
lightSkyBlue3           :: Colour3
lightSkyBlue4           :: Colour3
lightSkyBlue1           = V3 0.69   0.888  1
lightSkyBlue2           = V3 0.644   0.828   0.932
lightSkyBlue3           = V3 0.552   0.712   0.804
lightSkyBlue4           = V3 0.376   0.484   0.545

lightSteelBlue1         :: Colour3
lightSteelBlue2         :: Colour3
lightSteelBlue3         :: Colour3
lightSteelBlue4         :: Colour3
lightSteelBlue1         = V3 0.792   0.884  1
lightSteelBlue2         = V3 0.736   0.824   0.932
lightSteelBlue3         = V3 0.635   0.71   0.804
lightSteelBlue4         = V3 0.43   0.484   0.545

lightYellow1            :: Colour3
lightYellow2            :: Colour3
lightYellow3            :: Colour3
lightYellow4            :: Colour3
lightYellow1            = V3 1  1   0.88
lightYellow2            = V3 0.932   0.932   0.82
lightYellow3            = V3 0.804   0.804   0.705
lightYellow4            = V3 0.545   0.545   0.48

magenta1                :: Colour3
magenta2                :: Colour3
magenta3                :: Colour3
magenta4                :: Colour3
magenta1                = V3 1  0  1
magenta2                = V3 0.932  0   0.932
magenta3                = V3 0.804  0   0.804
magenta4                = V3 0.545  0   0.545

maroon1                 :: Colour3
maroon2                 :: Colour3
maroon3                 :: Colour3
maroon4                 :: Colour3
maroon1                 = V3 1   0.204   0.7
maroon2                 = V3 0.932   0.19   0.655
maroon3                 = V3 0.804   0.16   0.565
maroon4                 = V3 0.545   0.11   0.385

mediumOrchid1           :: Colour3
mediumOrchid2           :: Colour3
mediumOrchid3           :: Colour3
mediumOrchid4           :: Colour3
mediumOrchid1           = V3 0.88   0.4  1
mediumOrchid2           = V3 0.82   0.372   0.932
mediumOrchid3           = V3 0.705   0.32   0.804
mediumOrchid4           = V3 0.48   0.215   0.545

mediumPurple1           :: Colour3
mediumPurple2           :: Colour3
mediumPurple3           :: Colour3
mediumPurple4           :: Colour3
mediumPurple1           = V3 0.67   0.51  1
mediumPurple2           = V3 0.624   0.475   0.932
mediumPurple3           = V3 0.536   0.408   0.804
mediumPurple4           = V3 0.365   0.28   0.545

mistyRose1              :: Colour3
mistyRose2              :: Colour3
mistyRose3              :: Colour3
mistyRose4              :: Colour3
mistyRose1              = V3 1   0.894   0.884
mistyRose2              = V3 0.932   0.835   0.824
mistyRose3              = V3 0.804   0.716   0.71
mistyRose4              = V3 0.545   0.49   0.484

navajoWhite1            :: Colour3
navajoWhite2            :: Colour3
navajoWhite3            :: Colour3
navajoWhite4            :: Colour3
navajoWhite1            = V3 1   0.87   0.68
navajoWhite2            = V3 0.932   0.81   0.63
navajoWhite3            = V3 0.804   0.7   0.545
navajoWhite4            = V3 0.545   0.475   0.37

oliveDrab1              :: Colour3
oliveDrab2              :: Colour3
oliveDrab3              :: Colour3
oliveDrab4              :: Colour3
oliveDrab1              = V3 0.752  1   0.244
oliveDrab2              = V3 0.7   0.932   0.228
oliveDrab3              = V3 0.604   0.804   0.196
oliveDrab4              = V3 0.41   0.545   0.132

orange1                 :: Colour3
orange2                 :: Colour3
orange3                 :: Colour3
orange4                 :: Colour3
orange1                 = V3 1   0.648  0
orange2                 = V3 0.932   0.604  0
orange3                 = V3 0.804   0.52  0
orange4                 = V3 0.545   0.352  0

orangeRed1              :: Colour3
orangeRed2              :: Colour3
orangeRed3              :: Colour3
orangeRed4              :: Colour3
orangeRed1              = V3 1   0.27  0
orangeRed2              = V3 0.932   0.25  0
orangeRed3              = V3 0.804   0.215  0
orangeRed4              = V3 0.545   0.145  0

orchid1                 :: Colour3
orchid2                 :: Colour3
orchid3                 :: Colour3
orchid4                 :: Colour3
orchid1                 = V3 1   0.512   0.98
orchid2                 = V3 0.932   0.48   0.912
orchid3                 = V3 0.804   0.41   0.79
orchid4                 = V3 0.545   0.28   0.536

paleGreen1              :: Colour3
paleGreen2              :: Colour3
paleGreen3              :: Colour3
paleGreen4              :: Colour3
paleGreen1              = V3 0.604  1   0.604
paleGreen2              = V3 0.565   0.932   0.565
paleGreen3              = V3 0.488   0.804   0.488
paleGreen4              = V3 0.33   0.545   0.33

paleTurquoise1          :: Colour3
paleTurquoise2          :: Colour3
paleTurquoise3          :: Colour3
paleTurquoise4          :: Colour3
paleTurquoise1          = V3 0.732  1  1
paleTurquoise2          = V3 0.684   0.932   0.932
paleTurquoise3          = V3 0.59   0.804   0.804
paleTurquoise4          = V3 0.4   0.545   0.545

paleVioletRed1          :: Colour3
paleVioletRed2          :: Colour3
paleVioletRed3          :: Colour3
paleVioletRed4          :: Colour3
paleVioletRed1          = V3 1   0.51   0.67
paleVioletRed2          = V3 0.932   0.475   0.624
paleVioletRed3          = V3 0.804   0.408   0.536
paleVioletRed4          = V3 0.545   0.28   0.365

peachPuff1              :: Colour3
peachPuff2              :: Colour3
peachPuff3              :: Colour3
peachPuff4              :: Colour3
peachPuff1              = V3 1   0.855   0.725
peachPuff2              = V3 0.932   0.796   0.68
peachPuff3              = V3 0.804   0.688   0.585
peachPuff4              = V3 0.545   0.468   0.396

pink1                   :: Colour3
pink2                   :: Colour3
pink3                   :: Colour3
pink4                   :: Colour3
pink1                   = V3 1   0.71   0.772
pink2                   = V3 0.932   0.664   0.72
pink3                   = V3 0.804   0.57   0.62
pink4                   = V3 0.545   0.39   0.424

plum1                   :: Colour3
plum2                   :: Colour3
plum3                   :: Colour3
plum4                   :: Colour3
plum1                   = V3 1   0.732  1
plum2                   = V3 0.932   0.684   0.932
plum3                   = V3 0.804   0.59   0.804
plum4                   = V3 0.545   0.4   0.545

purple1                 :: Colour3
purple2                 :: Colour3
purple3                 :: Colour3
purple4                 :: Colour3
purple1                 = V3 0.608   0.19  1
purple2                 = V3 0.57   0.172   0.932
purple3                 = V3 0.49   0.15   0.804
purple4                 = V3 0.332   0.1   0.545

red1                    :: Colour3
red2                    :: Colour3
red3                    :: Colour3
red4                    :: Colour3
red1                    = V3 1  0  0
red2                    = V3 0.932  0  0
red3                    = V3 0.804  0  0
red4                    = V3 0.545  0  0

rosyBrown1              :: Colour3
rosyBrown2              :: Colour3
rosyBrown3              :: Colour3
rosyBrown4              :: Colour3
rosyBrown1              = V3 1   0.756   0.756
rosyBrown2              = V3 0.932   0.705   0.705
rosyBrown3              = V3 0.804   0.608   0.608
rosyBrown4              = V3 0.545   0.41   0.41

royalBlue1              :: Colour3
royalBlue2              :: Colour3
royalBlue3              :: Colour3
royalBlue4              :: Colour3
royalBlue1              = V3 0.284   0.464  1
royalBlue2              = V3 0.264   0.43   0.932
royalBlue3              = V3 0.228   0.372   0.804
royalBlue4              = V3 0.152   0.25   0.545

salmon1                 :: Colour3
salmon2                 :: Colour3
salmon3                 :: Colour3
salmon4                 :: Colour3
salmon1                 = V3 1   0.55   0.41
salmon2                 = V3 0.932   0.51   0.385
salmon3                 = V3 0.804   0.44   0.33
salmon4                 = V3 0.545   0.298   0.224

seaGreen1               :: Colour3
seaGreen2               :: Colour3
seaGreen3               :: Colour3
seaGreen4               :: Colour3
seaGreen1               = V3 0.33  1   0.624
seaGreen2               = V3 0.305   0.932   0.58
seaGreen3               = V3 0.264   0.804   0.5
seaGreen4               = V3 0.18   0.545   0.34

seashell1               :: Colour3
seashell2               :: Colour3
seashell3               :: Colour3
seashell4               :: Colour3
seashell1               = V3 1   0.96   0.932
seashell2               = V3 0.932   0.898   0.87
seashell3               = V3 0.804   0.772   0.75
seashell4               = V3 0.545   0.525   0.51

sienna1                 :: Colour3
sienna2                 :: Colour3
sienna3                 :: Colour3
sienna4                 :: Colour3
sienna1                 = V3 1   0.51   0.28
sienna2                 = V3 0.932   0.475   0.26
sienna3                 = V3 0.804   0.408   0.224
sienna4                 = V3 0.545   0.28   0.15

skyBlue1                :: Colour3
skyBlue2                :: Colour3
skyBlue3                :: Colour3
skyBlue4                :: Colour3
skyBlue1                = V3 0.53   0.808  1
skyBlue2                = V3 0.494   0.752   0.932
skyBlue3                = V3 0.424   0.65   0.804
skyBlue4                = V3 0.29   0.44   0.545

slateBlue1              :: Colour3
slateBlue2              :: Colour3
slateBlue3              :: Colour3
slateBlue4              :: Colour3
slateBlue1              = V3 0.512   0.435  1
slateBlue2              = V3 0.48   0.404   0.932
slateBlue3              = V3 0.41   0.35   0.804
slateBlue4              = V3 0.28   0.235   0.545

slateGray1              :: Colour3
slateGray2              :: Colour3
slateGray3              :: Colour3
slateGray4              :: Colour3
slateGray1              = V3 0.776   0.888  1
slateGray2              = V3 0.725   0.828   0.932
slateGray3              = V3 0.624   0.712   0.804
slateGray4              = V3 0.424   0.484   0.545

snow1                   :: Colour3
snow2                   :: Colour3
snow3                   :: Colour3
snow4                   :: Colour3
snow1                   = V3 1   0.98   0.98
snow2                   = V3 0.932   0.912   0.912
snow3                   = V3 0.804   0.79   0.79
snow4                   = V3 0.545   0.536   0.536

springGreen1            :: Colour3
springGreen2            :: Colour3
springGreen3            :: Colour3
springGreen4            :: Colour3
springGreen1            = V3 0  1   0.498
springGreen2            = V3 0   0.932   0.464
springGreen3            = V3 0   0.804   0.4
springGreen4            = V3 0   0.545   0.27

steelBlue1              :: Colour3
steelBlue2              :: Colour3
steelBlue3              :: Colour3
steelBlue4              :: Colour3
steelBlue1              = V3 0.39   0.72  1
steelBlue2              = V3 0.36   0.675   0.932
steelBlue3              = V3 0.31   0.58   0.804
steelBlue4              = V3 0.21   0.392   0.545

tan1                    :: Colour3
tan2                    :: Colour3
tan3                    :: Colour3
tan4                    :: Colour3
tan1                    = V3 1   0.648   0.31
tan2                    = V3 0.932   0.604   0.288
tan3                    = V3 0.804   0.52   0.248
tan4                    = V3 0.545   0.352   0.17

thistle1                :: Colour3
thistle2                :: Colour3
thistle3                :: Colour3
thistle4                :: Colour3
thistle1                = V3 1   0.884  1
thistle2                = V3 0.932   0.824   0.932
thistle3                = V3 0.804   0.71   0.804
thistle4                = V3 0.545   0.484   0.545

tomato1                 :: Colour3
tomato2                 :: Colour3
tomato3                 :: Colour3
tomato4                 :: Colour3
tomato1                 = V3 1   0.39   0.28
tomato2                 = V3 0.932   0.36   0.26
tomato3                 = V3 0.804   0.31   0.224
tomato4                 = V3 0.545   0.21   0.15

turquoise1              :: Colour3
turquoise2              :: Colour3
turquoise3              :: Colour3
turquoise4              :: Colour3
turquoise1              = V3 0   0.96  1
turquoise2              = V3 0   0.898   0.932
turquoise3              = V3 0   0.772   0.804
turquoise4              = V3 0   0.525   0.545

violetRed1              :: Colour3
violetRed2              :: Colour3
violetRed3              :: Colour3
violetRed4              :: Colour3
violetRed1              = V3 1   0.244   0.59
violetRed2              = V3 0.932   0.228   0.55
violetRed3              = V3 0.804   0.196   0.47
violetRed4              = V3 0.545   0.132   0.32

wheat1                  :: Colour3
wheat2                  :: Colour3
wheat3                  :: Colour3
wheat4                  :: Colour3
wheat1                  = V3 1   0.905   0.73
wheat2                  = V3 0.932   0.848   0.684
wheat3                  = V3 0.804   0.73   0.59
wheat4                  = V3 0.545   0.494   0.4

yellow1                 :: Colour3
yellow2                 :: Colour3
yellow3                 :: Colour3
yellow4                 :: Colour3
yellow1                 = V3 1  1  0
yellow2                 = V3 0.932   0.932  0
yellow3                 = V3 0.804   0.804  0
yellow4                 = V3 0.545   0.545  0

gray0                   :: Colour3
green0                  :: Colour3
grey0                   :: Colour3
maroon0                 :: Colour3
purple0                 :: Colour3
gray0                   = V3 0.745  0.745  0.745
green0                  = V3 0      1      0
grey0                   = V3 0.745  0.745  0.745
maroon0                 = V3 0.69   0.19   0.376
purple0                 = V3 0.628  0.125  0.94




