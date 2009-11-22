{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Extra.X11Colours
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

module Wumpus.Extra.X11Colours
  ( 
  
  -- * Named X11 colours
    antiqueWhite1
  , antiqueWhite2
  , antiqueWhite3
  , antiqueWhite4
  , aquamarine1
  , aquamarine2
  , aquamarine3
  , aquamarine4
  , azure1
  , azure2
  , azure3
  , azure4
  , bisque1
  , bisque2
  , bisque3
  , bisque4
  , blue1
  , blue2
  , blue3
  , blue4
  , brown1
  , brown2
  , brown3
  , brown4
  , burlywood1
  , burlywood2
  , burlywood3
  , burlywood4
  , cadetBlue1
  , cadetBlue2
  , cadetBlue3
  , cadetBlue4
  , chartreuse1
  , chartreuse2
  , chartreuse3
  , chartreuse4
  , chocolate1
  , chocolate2
  , chocolate3
  , chocolate4
  , coral1
  , coral2
  , coral3
  , coral4
  , cornsilk1
  , cornsilk2
  , cornsilk3
  , cornsilk4
  , cyan1
  , cyan2
  , cyan3
  , cyan4
  , darkGoldenrod1
  , darkGoldenrod2
  , darkGoldenrod3
  , darkGoldenrod4
  , darkOliveGreen1
  , darkOliveGreen2
  , darkOliveGreen3
  , darkOliveGreen4
  , darkOrange1
  , darkOrange2
  , darkOrange3
  , darkOrange4
  , darkOrchid1
  , darkOrchid2
  , darkOrchid3
  , darkOrchid4
  , darkSeaGreen1
  , darkSeaGreen2
  , darkSeaGreen3
  , darkSeaGreen4
  , darkSlateGray1
  , darkSlateGray2
  , darkSlateGray3
  , darkSlateGray4
  , deepPink1
  , deepPink2
  , deepPink3
  , deepPink4
  , deepSkyBlue1
  , deepSkyBlue2
  , deepSkyBlue3
  , deepSkyBlue4
  , dodgerBlue1
  , dodgerBlue2
  , dodgerBlue3
  , dodgerBlue4
  , firebrick1
  , firebrick2
  , firebrick3
  , firebrick4
  , gold1
  , gold2
  , gold3
  , gold4
  , goldenrod1
  , goldenrod2
  , goldenrod3
  , goldenrod4
  , green1
  , green2
  , green3
  , green4
  , honeydew1
  , honeydew2
  , honeydew3
  , honeydew4
  , hotPink1
  , hotPink2
  , hotPink3
  , hotPink4
  , indianRed1
  , indianRed2
  , indianRed3
  , indianRed4
  , ivory1
  , ivory2
  , ivory3
  , ivory4
  , khaki1
  , khaki2
  , khaki3
  , khaki4
  , lavenderBlush1
  , lavenderBlush2
  , lavenderBlush3
  , lavenderBlush4
  , lemonChiffon1
  , lemonChiffon2
  , lemonChiffon3
  , lemonChiffon4
  , lightBlue1
  , lightBlue2
  , lightBlue3
  , lightBlue4
  , lightCyan1
  , lightCyan2
  , lightCyan3
  , lightCyan4
  , lightGoldenrod1
  , lightGoldenrod2
  , lightGoldenrod3
  , lightGoldenrod4
  , lightPink1
  , lightPink2
  , lightPink3
  , lightPink4
  , lightSalmon1
  , lightSalmon2
  , lightSalmon3
  , lightSalmon4
  , lightSkyBlue1
  , lightSkyBlue2
  , lightSkyBlue3
  , lightSkyBlue4
  , lightSteelBlue1
  , lightSteelBlue2
  , lightSteelBlue3
  , lightSteelBlue4
  , lightYellow1
  , lightYellow2
  , lightYellow3
  , lightYellow4
  , magenta1
  , magenta2
  , magenta3
  , magenta4
  , maroon1
  , maroon2
  , maroon3
  , maroon4
  , mediumOrchid1
  , mediumOrchid2
  , mediumOrchid3
  , mediumOrchid4
  , mediumPurple1
  , mediumPurple2
  , mediumPurple3
  , mediumPurple4
  , mistyRose1
  , mistyRose2
  , mistyRose3
  , mistyRose4
  , navajoWhite1
  , navajoWhite2
  , navajoWhite3
  , navajoWhite4
  , oliveDrab1
  , oliveDrab2
  , oliveDrab3
  , oliveDrab4
  , orange1
  , orange2
  , orange3
  , orange4
  , orangeRed1
  , orangeRed2
  , orangeRed3
  , orangeRed4
  , orchid1
  , orchid2
  , orchid3
  , orchid4
  , paleGreen1
  , paleGreen2
  , paleGreen3
  , paleGreen4
  , paleTurquoise1
  , paleTurquoise2
  , paleTurquoise3
  , paleTurquoise4
  , paleVioletRed1
  , paleVioletRed2
  , paleVioletRed3
  , paleVioletRed4
  , peachPuff1
  , peachPuff2
  , peachPuff3
  , peachPuff4
  , pink1
  , pink2
  , pink3
  , pink4
  , plum1
  , plum2
  , plum3
  , plum4
  , purple1
  , purple2
  , purple3
  , purple4
  , red1
  , red2
  , red3
  , red4
  , rosyBrown1
  , rosyBrown2
  , rosyBrown3
  , rosyBrown4
  , royalBlue1
  , royalBlue2
  , royalBlue3
  , royalBlue4
  , salmon1
  , salmon2
  , salmon3
  , salmon4
  , seaGreen1
  , seaGreen2
  , seaGreen3
  , seaGreen4
  , seashell1
  , seashell2
  , seashell3
  , seashell4
  , sienna1
  , sienna2
  , sienna3
  , sienna4
  , skyBlue1
  , skyBlue2
  , skyBlue3
  , skyBlue4
  , slateBlue1
  , slateBlue2
  , slateBlue3
  , slateBlue4
  , slateGray1
  , slateGray2
  , slateGray3
  , slateGray4
  , snow1
  , snow2
  , snow3
  , snow4
  , springGreen1
  , springGreen2
  , springGreen3
  , springGreen4
  , steelBlue1
  , steelBlue2
  , steelBlue3
  , steelBlue4
  , tan1
  , tan2
  , tan3
  , tan4
  , thistle1
  , thistle2
  , thistle3
  , thistle4
  , tomato1
  , tomato2
  , tomato3
  , tomato4
  , turquoise1
  , turquoise2
  , turquoise3
  , turquoise4
  , violetRed1
  , violetRed2
  , violetRed3
  , violetRed4
  , wheat1
  , wheat2
  , wheat3
  , wheat4
  , yellow1
  , yellow2
  , yellow3
  , yellow4
  , gray0
  , green0
  , grey0
  , maroon0
  , purple0

  ) where

import Wumpus.Core.Colour ( RGB3(..), DRGB )

--------------------------------------------------------------------------------

antiqueWhite1           :: DRGB
antiqueWhite2           :: DRGB
antiqueWhite3           :: DRGB
antiqueWhite4           :: DRGB
antiqueWhite1           = RGB3 1   0.936   0.86
antiqueWhite2           = RGB3 0.932   0.875   0.8
antiqueWhite3           = RGB3 0.804   0.752   0.69
antiqueWhite4           = RGB3 0.545   0.512   0.47

aquamarine1             :: DRGB
aquamarine2             :: DRGB
aquamarine3             :: DRGB
aquamarine4             :: DRGB
aquamarine1             = RGB3 0.498  1   0.83
aquamarine2             = RGB3 0.464   0.932   0.776
aquamarine3             = RGB3 0.4   0.804   0.668
aquamarine4             = RGB3 0.27   0.545   0.455

azure1                  :: DRGB
azure2                  :: DRGB
azure3                  :: DRGB
azure4                  :: DRGB
azure1                  = RGB3 0.94  1  1
azure2                  = RGB3 0.88   0.932   0.932
azure3                  = RGB3 0.756   0.804   0.804
azure4                  = RGB3 0.512   0.545   0.545

bisque1                 :: DRGB
bisque2                 :: DRGB
bisque3                 :: DRGB
bisque4                 :: DRGB
bisque1                 = RGB3 1   0.894   0.77
bisque2                 = RGB3 0.932   0.835   0.716
bisque3                 = RGB3 0.804   0.716   0.62
bisque4                 = RGB3 0.545   0.49   0.42

blue1                   :: DRGB
blue2                   :: DRGB
blue3                   :: DRGB
blue4                   :: DRGB
blue1                   = RGB3 0  0  1
blue2                   = RGB3 0  0   0.932
blue3                   = RGB3 0  0   0.804
blue4                   = RGB3 0  0   0.545

brown1                  :: DRGB
brown2                  :: DRGB
brown3                  :: DRGB
brown4                  :: DRGB
brown1                  = RGB3 1   0.25   0.25
brown2                  = RGB3 0.932   0.23   0.23
brown3                  = RGB3 0.804   0.2   0.2
brown4                  = RGB3 0.545   0.136   0.136

burlywood1              :: DRGB
burlywood2              :: DRGB
burlywood3              :: DRGB
burlywood4              :: DRGB
burlywood1              = RGB3 1   0.828   0.608
burlywood2              = RGB3 0.932   0.772   0.57
burlywood3              = RGB3 0.804   0.668   0.49
burlywood4              = RGB3 0.545   0.45   0.332

cadetBlue1              :: DRGB
cadetBlue2              :: DRGB
cadetBlue3              :: DRGB
cadetBlue4              :: DRGB
cadetBlue1              = RGB3 0.596   0.96  1
cadetBlue2              = RGB3 0.556   0.898   0.932
cadetBlue3              = RGB3 0.48   0.772   0.804
cadetBlue4              = RGB3 0.325   0.525   0.545

chartreuse1             :: DRGB
chartreuse2             :: DRGB
chartreuse3             :: DRGB
chartreuse4             :: DRGB
chartreuse1             = RGB3 0.498  1  0
chartreuse2             = RGB3 0.464   0.932  0
chartreuse3             = RGB3 0.4   0.804  0
chartreuse4             = RGB3 0.27   0.545  0

chocolate1              :: DRGB
chocolate2              :: DRGB
chocolate3              :: DRGB
chocolate4              :: DRGB
chocolate1              = RGB3 1   0.498   0.14
chocolate2              = RGB3 0.932   0.464   0.13
chocolate3              = RGB3 0.804   0.4   0.112
chocolate4              = RGB3 0.545   0.27   0.075

coral1                  :: DRGB
coral2                  :: DRGB
coral3                  :: DRGB
coral4                  :: DRGB
coral1                  = RGB3 1   0.448   0.336
coral2                  = RGB3 0.932   0.415   0.312
coral3                  = RGB3 0.804   0.356   0.27
coral4                  = RGB3 0.545   0.244   0.185

cornsilk1               :: DRGB
cornsilk2               :: DRGB
cornsilk3               :: DRGB
cornsilk4               :: DRGB
cornsilk1               = RGB3 1   0.972   0.864
cornsilk2               = RGB3 0.932   0.91   0.804
cornsilk3               = RGB3 0.804   0.785   0.694
cornsilk4               = RGB3 0.545   0.532   0.47

cyan1                   :: DRGB
cyan2                   :: DRGB
cyan3                   :: DRGB
cyan4                   :: DRGB
cyan1                   = RGB3 0   1   1
cyan2                   = RGB3 0   0.932   0.932
cyan3                   = RGB3 0   0.804   0.804
cyan4                   = RGB3 0   0.545   0.545

darkGoldenrod1          :: DRGB
darkGoldenrod2          :: DRGB
darkGoldenrod3          :: DRGB
darkGoldenrod4          :: DRGB
darkGoldenrod1          = RGB3 1   0.725   0.06
darkGoldenrod2          = RGB3 0.932   0.68   0.055
darkGoldenrod3          = RGB3 0.804   0.585   0.048
darkGoldenrod4          = RGB3 0.545   0.396   0.03

darkOliveGreen1         :: DRGB
darkOliveGreen2         :: DRGB
darkOliveGreen3         :: DRGB
darkOliveGreen4         :: DRGB
darkOliveGreen1         = RGB3 0.792  1   0.44
darkOliveGreen2         = RGB3 0.736   0.932   0.408
darkOliveGreen3         = RGB3 0.635   0.804   0.352
darkOliveGreen4         = RGB3 0.43   0.545   0.24

darkOrange1             :: DRGB
darkOrange2             :: DRGB
darkOrange3             :: DRGB
darkOrange4             :: DRGB
darkOrange1             = RGB3 1   0.498  0
darkOrange2             = RGB3 0.932   0.464  0
darkOrange3             = RGB3 0.804   0.4  0
darkOrange4             = RGB3 0.545   0.27  0

darkOrchid1             :: DRGB
darkOrchid2             :: DRGB
darkOrchid3             :: DRGB
darkOrchid4             :: DRGB
darkOrchid1             = RGB3 0.75   0.244  1
darkOrchid2             = RGB3 0.698   0.228   0.932
darkOrchid3             = RGB3 0.604   0.196   0.804
darkOrchid4             = RGB3 0.408   0.132   0.545

darkSeaGreen1           :: DRGB
darkSeaGreen2           :: DRGB
darkSeaGreen3           :: DRGB
darkSeaGreen4           :: DRGB
darkSeaGreen1           = RGB3 0.756  1   0.756
darkSeaGreen2           = RGB3 0.705   0.932   0.705
darkSeaGreen3           = RGB3 0.608   0.804   0.608
darkSeaGreen4           = RGB3 0.41   0.545   0.41

darkSlateGray1          :: DRGB
darkSlateGray2          :: DRGB
darkSlateGray3          :: DRGB
darkSlateGray4          :: DRGB
darkSlateGray1          = RGB3 0.592  1  1
darkSlateGray2          = RGB3 0.552   0.932   0.932
darkSlateGray3          = RGB3 0.475   0.804   0.804
darkSlateGray4          = RGB3 0.32   0.545   0.545

deepPink1               :: DRGB
deepPink2               :: DRGB
deepPink3               :: DRGB
deepPink4               :: DRGB
deepPink1               = RGB3 1   0.08   0.576
deepPink2               = RGB3 0.932   0.07   0.536
deepPink3               = RGB3 0.804   0.064   0.464
deepPink4               = RGB3 0.545   0.04   0.312

deepSkyBlue1            :: DRGB
deepSkyBlue2            :: DRGB
deepSkyBlue3            :: DRGB
deepSkyBlue4            :: DRGB
deepSkyBlue1            = RGB3 0   0.75  1
deepSkyBlue2            = RGB3 0   0.698   0.932
deepSkyBlue3            = RGB3 0   0.604   0.804
deepSkyBlue4            = RGB3 0   0.408   0.545

dodgerBlue1             :: DRGB
dodgerBlue2             :: DRGB
dodgerBlue3             :: DRGB
dodgerBlue4             :: DRGB
dodgerBlue1             = RGB3 0.116   0.565  1
dodgerBlue2             = RGB3 0.11   0.525   0.932
dodgerBlue3             = RGB3 0.094   0.455   0.804
dodgerBlue4             = RGB3 0.064   0.305   0.545

firebrick1              :: DRGB
firebrick2              :: DRGB
firebrick3              :: DRGB
firebrick4              :: DRGB
firebrick1              = RGB3 1   0.19   0.19
firebrick2              = RGB3 0.932   0.172   0.172
firebrick3              = RGB3 0.804   0.15   0.15
firebrick4              = RGB3 0.545   0.1   0.1

gold1                   :: DRGB
gold2                   :: DRGB
gold3                   :: DRGB
gold4                   :: DRGB
gold1                   = RGB3 1   0.844  0
gold2                   = RGB3 0.932   0.79  0
gold3                   = RGB3 0.804   0.68  0
gold4                   = RGB3 0.545   0.46  0

goldenrod1              :: DRGB
goldenrod2              :: DRGB
goldenrod3              :: DRGB
goldenrod4              :: DRGB
goldenrod1              = RGB3 1   0.756   0.145
goldenrod2              = RGB3 0.932   0.705   0.132
goldenrod3              = RGB3 0.804   0.608   0.112
goldenrod4              = RGB3 0.545   0.41   0.08

green1                  :: DRGB
green2                  :: DRGB
green3                  :: DRGB
green4                  :: DRGB
green1                  = RGB3 0  1  0
green2                  = RGB3 0   0.932  0
green3                  = RGB3 0   0.804  0
green4                  = RGB3 0   0.545  0

honeydew1               :: DRGB
honeydew2               :: DRGB
honeydew3               :: DRGB
honeydew4               :: DRGB
honeydew1               = RGB3 0.94  1   0.94
honeydew2               = RGB3 0.88   0.932   0.88
honeydew3               = RGB3 0.756   0.804   0.756
honeydew4               = RGB3 0.512   0.545   0.512

hotPink1                :: DRGB
hotPink2                :: DRGB
hotPink3                :: DRGB
hotPink4                :: DRGB
hotPink1                = RGB3 1   0.43   0.705
hotPink2                = RGB3 0.932   0.415   0.655
hotPink3                = RGB3 0.804   0.376   0.565
hotPink4                = RGB3 0.545   0.228   0.385

indianRed1              :: DRGB
indianRed2              :: DRGB
indianRed3              :: DRGB
indianRed4              :: DRGB
indianRed1              = RGB3 1   0.415   0.415
indianRed2              = RGB3 0.932   0.39   0.39
indianRed3              = RGB3 0.804   0.332   0.332
indianRed4              = RGB3 0.545   0.228   0.228

ivory1                  :: DRGB
ivory2                  :: DRGB
ivory3                  :: DRGB
ivory4                  :: DRGB
ivory1                  = RGB3 1   1   0.94
ivory2                  = RGB3 0.932   0.932   0.88
ivory3                  = RGB3 0.804   0.804   0.756
ivory4                  = RGB3 0.545   0.545   0.512

khaki1                  :: DRGB
khaki2                  :: DRGB
khaki3                  :: DRGB
khaki4                  :: DRGB
khaki1                  = RGB3 1   0.965   0.56
khaki2                  = RGB3 0.932   0.9   0.52
khaki3                  = RGB3 0.804   0.776   0.45
khaki4                  = RGB3 0.545   0.525   0.305

lavenderBlush1          :: DRGB
lavenderBlush2          :: DRGB
lavenderBlush3          :: DRGB
lavenderBlush4          :: DRGB
lavenderBlush1          = RGB3 1   0.94   0.96
lavenderBlush2          = RGB3 0.932   0.88   0.898
lavenderBlush3          = RGB3 0.804   0.756   0.772
lavenderBlush4          = RGB3 0.545   0.512   0.525

lemonChiffon1           :: DRGB
lemonChiffon2           :: DRGB
lemonChiffon3           :: DRGB
lemonChiffon4           :: DRGB
lemonChiffon1           = RGB3 1   0.98   0.804
lemonChiffon2           = RGB3 0.932   0.912   0.75
lemonChiffon3           = RGB3 0.804   0.79   0.648
lemonChiffon4           = RGB3 0.545   0.536   0.44

lightBlue1              :: DRGB
lightBlue2              :: DRGB
lightBlue3              :: DRGB
lightBlue4              :: DRGB
lightBlue1              = RGB3 0.75   0.936  1
lightBlue2              = RGB3 0.698   0.875   0.932
lightBlue3              = RGB3 0.604   0.752   0.804
lightBlue4              = RGB3 0.408   0.512   0.545

lightCyan1              :: DRGB
lightCyan2              :: DRGB
lightCyan3              :: DRGB
lightCyan4              :: DRGB
lightCyan1              = RGB3 0.88  1  1
lightCyan2              = RGB3 0.82   0.932   0.932
lightCyan3              = RGB3 0.705   0.804   0.804
lightCyan4              = RGB3 0.48   0.545   0.545

lightGoldenrod1         :: DRGB
lightGoldenrod2         :: DRGB
lightGoldenrod3         :: DRGB
lightGoldenrod4         :: DRGB
lightGoldenrod1         = RGB3 1   0.925   0.545
lightGoldenrod2         = RGB3 0.932   0.864   0.51
lightGoldenrod3         = RGB3 0.804   0.745   0.44
lightGoldenrod4         = RGB3 0.545   0.505   0.298

lightPink1              :: DRGB
lightPink2              :: DRGB
lightPink3              :: DRGB
lightPink4              :: DRGB
lightPink1              = RGB3 1   0.684   0.725
lightPink2              = RGB3 0.932   0.635   0.68
lightPink3              = RGB3 0.804   0.55   0.585
lightPink4              = RGB3 0.545   0.372   0.396

lightSalmon1            :: DRGB
lightSalmon2            :: DRGB
lightSalmon3            :: DRGB
lightSalmon4            :: DRGB
lightSalmon1            = RGB3 1   0.628   0.48
lightSalmon2            = RGB3 0.932   0.585   0.448
lightSalmon3            = RGB3 0.804   0.505   0.385
lightSalmon4            = RGB3 0.545   0.34   0.26

lightSkyBlue1           :: DRGB
lightSkyBlue2           :: DRGB
lightSkyBlue3           :: DRGB
lightSkyBlue4           :: DRGB
lightSkyBlue1           = RGB3 0.69   0.888  1
lightSkyBlue2           = RGB3 0.644   0.828   0.932
lightSkyBlue3           = RGB3 0.552   0.712   0.804
lightSkyBlue4           = RGB3 0.376   0.484   0.545

lightSteelBlue1         :: DRGB
lightSteelBlue2         :: DRGB
lightSteelBlue3         :: DRGB
lightSteelBlue4         :: DRGB
lightSteelBlue1         = RGB3 0.792   0.884  1
lightSteelBlue2         = RGB3 0.736   0.824   0.932
lightSteelBlue3         = RGB3 0.635   0.71   0.804
lightSteelBlue4         = RGB3 0.43   0.484   0.545

lightYellow1            :: DRGB
lightYellow2            :: DRGB
lightYellow3            :: DRGB
lightYellow4            :: DRGB
lightYellow1            = RGB3 1  1   0.88
lightYellow2            = RGB3 0.932   0.932   0.82
lightYellow3            = RGB3 0.804   0.804   0.705
lightYellow4            = RGB3 0.545   0.545   0.48

magenta1                :: DRGB
magenta2                :: DRGB
magenta3                :: DRGB
magenta4                :: DRGB
magenta1                = RGB3 1  0  1
magenta2                = RGB3 0.932  0   0.932
magenta3                = RGB3 0.804  0   0.804
magenta4                = RGB3 0.545  0   0.545

maroon1                 :: DRGB
maroon2                 :: DRGB
maroon3                 :: DRGB
maroon4                 :: DRGB
maroon1                 = RGB3 1   0.204   0.7
maroon2                 = RGB3 0.932   0.19   0.655
maroon3                 = RGB3 0.804   0.16   0.565
maroon4                 = RGB3 0.545   0.11   0.385

mediumOrchid1           :: DRGB
mediumOrchid2           :: DRGB
mediumOrchid3           :: DRGB
mediumOrchid4           :: DRGB
mediumOrchid1           = RGB3 0.88   0.4  1
mediumOrchid2           = RGB3 0.82   0.372   0.932
mediumOrchid3           = RGB3 0.705   0.32   0.804
mediumOrchid4           = RGB3 0.48   0.215   0.545

mediumPurple1           :: DRGB
mediumPurple2           :: DRGB
mediumPurple3           :: DRGB
mediumPurple4           :: DRGB
mediumPurple1           = RGB3 0.67   0.51  1
mediumPurple2           = RGB3 0.624   0.475   0.932
mediumPurple3           = RGB3 0.536   0.408   0.804
mediumPurple4           = RGB3 0.365   0.28   0.545

mistyRose1              :: DRGB
mistyRose2              :: DRGB
mistyRose3              :: DRGB
mistyRose4              :: DRGB
mistyRose1              = RGB3 1   0.894   0.884
mistyRose2              = RGB3 0.932   0.835   0.824
mistyRose3              = RGB3 0.804   0.716   0.71
mistyRose4              = RGB3 0.545   0.49   0.484

navajoWhite1            :: DRGB
navajoWhite2            :: DRGB
navajoWhite3            :: DRGB
navajoWhite4            :: DRGB
navajoWhite1            = RGB3 1   0.87   0.68
navajoWhite2            = RGB3 0.932   0.81   0.63
navajoWhite3            = RGB3 0.804   0.7   0.545
navajoWhite4            = RGB3 0.545   0.475   0.37

oliveDrab1              :: DRGB
oliveDrab2              :: DRGB
oliveDrab3              :: DRGB
oliveDrab4              :: DRGB
oliveDrab1              = RGB3 0.752  1   0.244
oliveDrab2              = RGB3 0.7   0.932   0.228
oliveDrab3              = RGB3 0.604   0.804   0.196
oliveDrab4              = RGB3 0.41   0.545   0.132

orange1                 :: DRGB
orange2                 :: DRGB
orange3                 :: DRGB
orange4                 :: DRGB
orange1                 = RGB3 1   0.648  0
orange2                 = RGB3 0.932   0.604  0
orange3                 = RGB3 0.804   0.52  0
orange4                 = RGB3 0.545   0.352  0

orangeRed1              :: DRGB
orangeRed2              :: DRGB
orangeRed3              :: DRGB
orangeRed4              :: DRGB
orangeRed1              = RGB3 1   0.27  0
orangeRed2              = RGB3 0.932   0.25  0
orangeRed3              = RGB3 0.804   0.215  0
orangeRed4              = RGB3 0.545   0.145  0

orchid1                 :: DRGB
orchid2                 :: DRGB
orchid3                 :: DRGB
orchid4                 :: DRGB
orchid1                 = RGB3 1   0.512   0.98
orchid2                 = RGB3 0.932   0.48   0.912
orchid3                 = RGB3 0.804   0.41   0.79
orchid4                 = RGB3 0.545   0.28   0.536

paleGreen1              :: DRGB
paleGreen2              :: DRGB
paleGreen3              :: DRGB
paleGreen4              :: DRGB
paleGreen1              = RGB3 0.604  1   0.604
paleGreen2              = RGB3 0.565   0.932   0.565
paleGreen3              = RGB3 0.488   0.804   0.488
paleGreen4              = RGB3 0.33   0.545   0.33

paleTurquoise1          :: DRGB
paleTurquoise2          :: DRGB
paleTurquoise3          :: DRGB
paleTurquoise4          :: DRGB
paleTurquoise1          = RGB3 0.732  1  1
paleTurquoise2          = RGB3 0.684   0.932   0.932
paleTurquoise3          = RGB3 0.59   0.804   0.804
paleTurquoise4          = RGB3 0.4   0.545   0.545

paleVioletRed1          :: DRGB
paleVioletRed2          :: DRGB
paleVioletRed3          :: DRGB
paleVioletRed4          :: DRGB
paleVioletRed1          = RGB3 1   0.51   0.67
paleVioletRed2          = RGB3 0.932   0.475   0.624
paleVioletRed3          = RGB3 0.804   0.408   0.536
paleVioletRed4          = RGB3 0.545   0.28   0.365

peachPuff1              :: DRGB
peachPuff2              :: DRGB
peachPuff3              :: DRGB
peachPuff4              :: DRGB
peachPuff1              = RGB3 1   0.855   0.725
peachPuff2              = RGB3 0.932   0.796   0.68
peachPuff3              = RGB3 0.804   0.688   0.585
peachPuff4              = RGB3 0.545   0.468   0.396

pink1                   :: DRGB
pink2                   :: DRGB
pink3                   :: DRGB
pink4                   :: DRGB
pink1                   = RGB3 1   0.71   0.772
pink2                   = RGB3 0.932   0.664   0.72
pink3                   = RGB3 0.804   0.57   0.62
pink4                   = RGB3 0.545   0.39   0.424

plum1                   :: DRGB
plum2                   :: DRGB
plum3                   :: DRGB
plum4                   :: DRGB
plum1                   = RGB3 1   0.732  1
plum2                   = RGB3 0.932   0.684   0.932
plum3                   = RGB3 0.804   0.59   0.804
plum4                   = RGB3 0.545   0.4   0.545

purple1                 :: DRGB
purple2                 :: DRGB
purple3                 :: DRGB
purple4                 :: DRGB
purple1                 = RGB3 0.608   0.19  1
purple2                 = RGB3 0.57   0.172   0.932
purple3                 = RGB3 0.49   0.15   0.804
purple4                 = RGB3 0.332   0.1   0.545

red1                    :: DRGB
red2                    :: DRGB
red3                    :: DRGB
red4                    :: DRGB
red1                    = RGB3 1  0  0
red2                    = RGB3 0.932  0  0
red3                    = RGB3 0.804  0  0
red4                    = RGB3 0.545  0  0

rosyBrown1              :: DRGB
rosyBrown2              :: DRGB
rosyBrown3              :: DRGB
rosyBrown4              :: DRGB
rosyBrown1              = RGB3 1   0.756   0.756
rosyBrown2              = RGB3 0.932   0.705   0.705
rosyBrown3              = RGB3 0.804   0.608   0.608
rosyBrown4              = RGB3 0.545   0.41   0.41

royalBlue1              :: DRGB
royalBlue2              :: DRGB
royalBlue3              :: DRGB
royalBlue4              :: DRGB
royalBlue1              = RGB3 0.284   0.464  1
royalBlue2              = RGB3 0.264   0.43   0.932
royalBlue3              = RGB3 0.228   0.372   0.804
royalBlue4              = RGB3 0.152   0.25   0.545

salmon1                 :: DRGB
salmon2                 :: DRGB
salmon3                 :: DRGB
salmon4                 :: DRGB
salmon1                 = RGB3 1   0.55   0.41
salmon2                 = RGB3 0.932   0.51   0.385
salmon3                 = RGB3 0.804   0.44   0.33
salmon4                 = RGB3 0.545   0.298   0.224

seaGreen1               :: DRGB
seaGreen2               :: DRGB
seaGreen3               :: DRGB
seaGreen4               :: DRGB
seaGreen1               = RGB3 0.33  1   0.624
seaGreen2               = RGB3 0.305   0.932   0.58
seaGreen3               = RGB3 0.264   0.804   0.5
seaGreen4               = RGB3 0.18   0.545   0.34

seashell1               :: DRGB
seashell2               :: DRGB
seashell3               :: DRGB
seashell4               :: DRGB
seashell1               = RGB3 1   0.96   0.932
seashell2               = RGB3 0.932   0.898   0.87
seashell3               = RGB3 0.804   0.772   0.75
seashell4               = RGB3 0.545   0.525   0.51

sienna1                 :: DRGB
sienna2                 :: DRGB
sienna3                 :: DRGB
sienna4                 :: DRGB
sienna1                 = RGB3 1   0.51   0.28
sienna2                 = RGB3 0.932   0.475   0.26
sienna3                 = RGB3 0.804   0.408   0.224
sienna4                 = RGB3 0.545   0.28   0.15

skyBlue1                :: DRGB
skyBlue2                :: DRGB
skyBlue3                :: DRGB
skyBlue4                :: DRGB
skyBlue1                = RGB3 0.53   0.808  1
skyBlue2                = RGB3 0.494   0.752   0.932
skyBlue3                = RGB3 0.424   0.65   0.804
skyBlue4                = RGB3 0.29   0.44   0.545

slateBlue1              :: DRGB
slateBlue2              :: DRGB
slateBlue3              :: DRGB
slateBlue4              :: DRGB
slateBlue1              = RGB3 0.512   0.435  1
slateBlue2              = RGB3 0.48   0.404   0.932
slateBlue3              = RGB3 0.41   0.35   0.804
slateBlue4              = RGB3 0.28   0.235   0.545

slateGray1              :: DRGB
slateGray2              :: DRGB
slateGray3              :: DRGB
slateGray4              :: DRGB
slateGray1              = RGB3 0.776   0.888  1
slateGray2              = RGB3 0.725   0.828   0.932
slateGray3              = RGB3 0.624   0.712   0.804
slateGray4              = RGB3 0.424   0.484   0.545

snow1                   :: DRGB
snow2                   :: DRGB
snow3                   :: DRGB
snow4                   :: DRGB
snow1                   = RGB3 1   0.98   0.98
snow2                   = RGB3 0.932   0.912   0.912
snow3                   = RGB3 0.804   0.79   0.79
snow4                   = RGB3 0.545   0.536   0.536

springGreen1            :: DRGB
springGreen2            :: DRGB
springGreen3            :: DRGB
springGreen4            :: DRGB
springGreen1            = RGB3 0  1   0.498
springGreen2            = RGB3 0   0.932   0.464
springGreen3            = RGB3 0   0.804   0.4
springGreen4            = RGB3 0   0.545   0.27

steelBlue1              :: DRGB
steelBlue2              :: DRGB
steelBlue3              :: DRGB
steelBlue4              :: DRGB
steelBlue1              = RGB3 0.39   0.72  1
steelBlue2              = RGB3 0.36   0.675   0.932
steelBlue3              = RGB3 0.31   0.58   0.804
steelBlue4              = RGB3 0.21   0.392   0.545

tan1                    :: DRGB
tan2                    :: DRGB
tan3                    :: DRGB
tan4                    :: DRGB
tan1                    = RGB3 1   0.648   0.31
tan2                    = RGB3 0.932   0.604   0.288
tan3                    = RGB3 0.804   0.52   0.248
tan4                    = RGB3 0.545   0.352   0.17

thistle1                :: DRGB
thistle2                :: DRGB
thistle3                :: DRGB
thistle4                :: DRGB
thistle1                = RGB3 1   0.884  1
thistle2                = RGB3 0.932   0.824   0.932
thistle3                = RGB3 0.804   0.71   0.804
thistle4                = RGB3 0.545   0.484   0.545

tomato1                 :: DRGB
tomato2                 :: DRGB
tomato3                 :: DRGB
tomato4                 :: DRGB
tomato1                 = RGB3 1   0.39   0.28
tomato2                 = RGB3 0.932   0.36   0.26
tomato3                 = RGB3 0.804   0.31   0.224
tomato4                 = RGB3 0.545   0.21   0.15

turquoise1              :: DRGB
turquoise2              :: DRGB
turquoise3              :: DRGB
turquoise4              :: DRGB
turquoise1              = RGB3 0   0.96  1
turquoise2              = RGB3 0   0.898   0.932
turquoise3              = RGB3 0   0.772   0.804
turquoise4              = RGB3 0   0.525   0.545

violetRed1              :: DRGB
violetRed2              :: DRGB
violetRed3              :: DRGB
violetRed4              :: DRGB
violetRed1              = RGB3 1   0.244   0.59
violetRed2              = RGB3 0.932   0.228   0.55
violetRed3              = RGB3 0.804   0.196   0.47
violetRed4              = RGB3 0.545   0.132   0.32

wheat1                  :: DRGB
wheat2                  :: DRGB
wheat3                  :: DRGB
wheat4                  :: DRGB
wheat1                  = RGB3 1   0.905   0.73
wheat2                  = RGB3 0.932   0.848   0.684
wheat3                  = RGB3 0.804   0.73   0.59
wheat4                  = RGB3 0.545   0.494   0.4

yellow1                 :: DRGB
yellow2                 :: DRGB
yellow3                 :: DRGB
yellow4                 :: DRGB
yellow1                 = RGB3 1  1  0
yellow2                 = RGB3 0.932   0.932  0
yellow3                 = RGB3 0.804   0.804  0
yellow4                 = RGB3 0.545   0.545  0

gray0                   :: DRGB
green0                  :: DRGB
grey0                   :: DRGB
maroon0                 :: DRGB
purple0                 :: DRGB
gray0                   = RGB3 0.745  0.745  0.745
green0                  = RGB3 0      1      0
grey0                   = RGB3 0.745  0.745  0.745
maroon0                 = RGB3 0.69   0.19   0.376
purple0                 = RGB3 0.628  0.125  0.94




