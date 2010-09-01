{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Colour.X11Colours
-- Copyright   :  (c) Stephen Tetley 2009-2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC with TypeFamilies and more
--
-- The X11 \'named colours\', as rgb [0,1] values 
--
--------------------------------------------------------------------------------

module Wumpus.Basic.Colour.X11Colours
  ( 
  
  -- * Named X11 colours
    antique_white1
  , antique_white2
  , antique_white3
  , antique_white4
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
  , cadet_blue1
  , cadet_blue2
  , cadet_blue3
  , cadet_blue4
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
  , dark_goldenrod1
  , dark_goldenrod2
  , dark_goldenrod3
  , dark_goldenrod4
  , dark_olive_green1
  , dark_olive_green2
  , dark_olive_green3
  , dark_olive_green4
  , dark_orange1
  , dark_orange2
  , dark_orange3
  , dark_orange4
  , dark_orchid1
  , dark_orchid2
  , dark_orchid3
  , dark_orchid4
  , dark_sea_green1
  , dark_sea_green2
  , dark_sea_green3
  , dark_sea_green4
  , dark_slate_gray1
  , dark_slate_gray2
  , dark_slate_gray3
  , dark_slate_gray4
  , deep_pink1
  , deep_pink2
  , deep_pink3
  , deep_pink4
  , deep_sky_blue1
  , deep_sky_blue2
  , deep_sky_blue3
  , deep_sky_blue4
  , dodger_blue1
  , dodger_blue2
  , dodger_blue3
  , dodger_blue4
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
  , hot_pink1
  , hot_pink2
  , hot_pink3
  , hot_pink4
  , indian_red1
  , indian_red2
  , indian_red3
  , indian_red4
  , ivory1
  , ivory2
  , ivory3
  , ivory4
  , khaki1
  , khaki2
  , khaki3
  , khaki4
  , lavender_blush1
  , lavender_blush2
  , lavender_blush3
  , lavender_blush4
  , lemon_chiffon1
  , lemon_chiffon2
  , lemon_chiffon3
  , lemon_chiffon4
  , light_blue1
  , light_blue2
  , light_blue3
  , light_blue4
  , light_cyan1
  , light_cyan2
  , light_cyan3
  , light_cyan4
  , light_goldenrod1
  , light_goldenrod2
  , light_goldenrod3
  , light_goldenrod4
  , light_pink1
  , light_pink2
  , light_pink3
  , light_pink4
  , light_salmon1
  , light_salmon2
  , light_salmon3
  , light_salmon4
  , light_sky_blue1
  , light_sky_blue2
  , light_sky_blue3
  , light_sky_blue4
  , light_steel_blue1
  , light_steel_blue2
  , light_steel_blue3
  , light_steel_blue4
  , light_yellow1
  , light_yellow2
  , light_yellow3
  , light_yellow4
  , magenta1
  , magenta2
  , magenta3
  , magenta4
  , maroon1
  , maroon2
  , maroon3
  , maroon4
  , medium_orchid1
  , medium_orchid2
  , medium_orchid3
  , medium_orchid4
  , medium_purple1
  , medium_purple2
  , medium_purple3
  , medium_purple4
  , misty_rose1
  , misty_rose2
  , misty_rose3
  , misty_rose4
  , navajo_white1
  , navajo_white2
  , navajo_white3
  , navajo_white4
  , olive_drab1
  , olive_drab2
  , olive_drab3
  , olive_drab4
  , orange1
  , orange2
  , orange3
  , orange4
  , orange_red1
  , orange_red2
  , orange_red3
  , orange_red4
  , orchid1
  , orchid2
  , orchid3
  , orchid4
  , pale_green1
  , pale_green2
  , pale_green3
  , pale_green4
  , pale_turquoise1
  , pale_turquoise2
  , pale_turquoise3
  , pale_turquoise4
  , pale_violet_red1
  , pale_violet_red2
  , pale_violet_red3
  , pale_violet_red4
  , peach_puff1
  , peach_puff2
  , peach_puff3
  , peach_puff4
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
  , rosy_brown1
  , rosy_brown2
  , rosy_brown3
  , rosy_brown4
  , royal_blue1
  , royal_blue2
  , royal_blue3
  , royal_blue4
  , salmon1
  , salmon2
  , salmon3
  , salmon4
  , sea_green1
  , sea_green2
  , sea_green3
  , sea_green4
  , seashell1
  , seashell2
  , seashell3
  , seashell4
  , sienna1
  , sienna2
  , sienna3
  , sienna4
  , sky_blue1
  , sky_blue2
  , sky_blue3
  , sky_blue4
  , slate_blue1
  , slate_blue2
  , slate_blue3
  , slate_blue4
  , slate_gray1
  , slate_gray2
  , slate_gray3
  , slate_gray4
  , snow1
  , snow2
  , snow3
  , snow4
  , spring_green1
  , spring_green2
  , spring_green3
  , spring_green4
  , steel_blue1
  , steel_blue2
  , steel_blue3
  , steel_blue4
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
  , violet_red1
  , violet_red2
  , violet_red3
  , violet_red4
  , wheat1
  , wheat2
  , wheat3
  , wheat4
  , yellow1
  , yellow2
  , yellow3
  , yellow4

  ) where

import Wumpus.Core.Colour ( RGB255(..), iRGB )


--------------------------------------------------------------------------------

antique_white1          :: RGB255
antique_white1          = iRGB 0xff 0xef 0xdb

antique_white2          :: RGB255
antique_white2          = iRGB 0xee 0xdf 0xcc

antique_white3          :: RGB255
antique_white3          = iRGB 0xcd 0xc0 0xb0

antique_white4          :: RGB255
antique_white4          = iRGB 0x8b 0x83 0x78

aquamarine1             :: RGB255
aquamarine1             = iRGB 0x7f 0xff 0xd4

aquamarine2             :: RGB255
aquamarine2             = iRGB 0x76 0xee 0xc6

aquamarine3             :: RGB255
aquamarine3             = iRGB 0x66 0xcd 0xaa

aquamarine4             :: RGB255
aquamarine4             = iRGB 0x45 0x8b 0x74

azure1                  :: RGB255
azure1                  = iRGB 0xf0 0xff 0xff

azure2                  :: RGB255
azure2                  = iRGB 0xe0 0xee 0xee

azure3                  :: RGB255
azure3                  = iRGB 0xc1 0xcd 0xcd

azure4                  :: RGB255
azure4                  = iRGB 0x83 0x8b 0x8b

bisque1                 :: RGB255
bisque1                 = iRGB 0xff 0xe4 0xc4

bisque2                 :: RGB255
bisque2                 = iRGB 0xee 0xd5 0xb7

bisque3                 :: RGB255
bisque3                 = iRGB 0xcd 0xb7 0x9e

bisque4                 :: RGB255
bisque4                 = iRGB 0x8b 0x7d 0x6b

blue1                   :: RGB255
blue1                   = iRGB 0x00 0x00 0xff

blue2                   :: RGB255
blue2                   = iRGB 0x00 0x00 0xee

blue3                   :: RGB255
blue3                   = iRGB 0x00 0x00 0xcd

blue4                   :: RGB255
blue4                   = iRGB 0x00 0x00 0x8b

brown1                  :: RGB255
brown1                  = iRGB 0xff 0x40 0x40

brown2                  :: RGB255
brown2                  = iRGB 0xee 0x3b 0x3b

brown3                  :: RGB255
brown3                  = iRGB 0xcd 0x33 0x33

brown4                  :: RGB255
brown4                  = iRGB 0x8b 0x23 0x23

burlywood1              :: RGB255
burlywood1              = iRGB 0xff 0xd3 0x9b

burlywood2              :: RGB255
burlywood2              = iRGB 0xee 0xc5 0x91

burlywood3              :: RGB255
burlywood3              = iRGB 0xcd 0xaa 0x7d

burlywood4              :: RGB255
burlywood4              = iRGB 0x8b 0x73 0x55

cadet_blue1             :: RGB255
cadet_blue1             = iRGB 0x98 0xf5 0xff

cadet_blue2             :: RGB255
cadet_blue2             = iRGB 0x8e 0xe5 0xee

cadet_blue3             :: RGB255
cadet_blue3             = iRGB 0x7a 0xc5 0xcd

cadet_blue4             :: RGB255
cadet_blue4             = iRGB 0x53 0x86 0x8b

chartreuse1             :: RGB255
chartreuse1             = iRGB 0x7f 0xff 0x00

chartreuse2             :: RGB255
chartreuse2             = iRGB 0x76 0xee 0x00

chartreuse3             :: RGB255
chartreuse3             = iRGB 0x66 0xcd 0x00

chartreuse4             :: RGB255
chartreuse4             = iRGB 0x45 0x8b 0x00

chocolate1              :: RGB255
chocolate1              = iRGB 0xff 0x7f 0x24

chocolate2              :: RGB255
chocolate2              = iRGB 0xee 0x76 0x21

chocolate3              :: RGB255
chocolate3              = iRGB 0xcd 0x66 0x1d

chocolate4              :: RGB255
chocolate4              = iRGB 0x8b 0x45 0x13

coral1                  :: RGB255
coral1                  = iRGB 0xff 0x72 0x56

coral2                  :: RGB255
coral2                  = iRGB 0xee 0x6a 0x50

coral3                  :: RGB255
coral3                  = iRGB 0xcd 0x5b 0x45

coral4                  :: RGB255
coral4                  = iRGB 0x8b 0x3e 0x2f

cornsilk1               :: RGB255
cornsilk1               = iRGB 0xff 0xf8 0xdc

cornsilk2               :: RGB255
cornsilk2               = iRGB 0xee 0xe8 0xcd

cornsilk3               :: RGB255
cornsilk3               = iRGB 0xcd 0xc8 0xb1

cornsilk4               :: RGB255
cornsilk4               = iRGB 0x8b 0x88 0x78

cyan1                   :: RGB255
cyan1                   = iRGB 0x00 0xff 0xff

cyan2                   :: RGB255
cyan2                   = iRGB 0x00 0xee 0xee

cyan3                   :: RGB255
cyan3                   = iRGB 0x00 0xcd 0xcd

cyan4                   :: RGB255
cyan4                   = iRGB 0x00 0x8b 0x8b

dark_goldenrod1         :: RGB255
dark_goldenrod1         = iRGB 0xff 0xb9 0x0f

dark_goldenrod2         :: RGB255
dark_goldenrod2         = iRGB 0xee 0xad 0x0e

dark_goldenrod3         :: RGB255
dark_goldenrod3         = iRGB 0xcd 0x95 0x0c

dark_goldenrod4         :: RGB255
dark_goldenrod4         = iRGB 0x8b 0x65 0x08

dark_olive_green1       :: RGB255
dark_olive_green1       = iRGB 0xca 0xff 0x70

dark_olive_green2       :: RGB255
dark_olive_green2       = iRGB 0xbc 0xee 0x68

dark_olive_green3       :: RGB255
dark_olive_green3       = iRGB 0xa2 0xcd 0x5a

dark_olive_green4       :: RGB255
dark_olive_green4       = iRGB 0x6e 0x8b 0x3d

dark_orange1            :: RGB255
dark_orange1            = iRGB 0xff 0x7f 0x00

dark_orange2            :: RGB255
dark_orange2            = iRGB 0xee 0x76 0x00

dark_orange3            :: RGB255
dark_orange3            = iRGB 0xcd 0x66 0x00

dark_orange4            :: RGB255
dark_orange4            = iRGB 0x8b 0x45 0x00

dark_orchid1            :: RGB255
dark_orchid1            = iRGB 0xbf 0x3e 0xff

dark_orchid2            :: RGB255
dark_orchid2            = iRGB 0xb2 0x3a 0xee

dark_orchid3            :: RGB255
dark_orchid3            = iRGB 0x9a 0x32 0xcd

dark_orchid4            :: RGB255
dark_orchid4            = iRGB 0x68 0x22 0x8b

dark_sea_green1         :: RGB255
dark_sea_green1         = iRGB 0xc1 0xff 0xc1

dark_sea_green2         :: RGB255
dark_sea_green2         = iRGB 0xb4 0xee 0xb4

dark_sea_green3         :: RGB255
dark_sea_green3         = iRGB 0x9b 0xcd 0x9b

dark_sea_green4         :: RGB255
dark_sea_green4         = iRGB 0x69 0x8b 0x69

dark_slate_gray1        :: RGB255
dark_slate_gray1        = iRGB 0x97 0xff 0xff

dark_slate_gray2        :: RGB255
dark_slate_gray2        = iRGB 0x8d 0xee 0xee

dark_slate_gray3        :: RGB255
dark_slate_gray3        = iRGB 0x79 0xcd 0xcd

dark_slate_gray4        :: RGB255
dark_slate_gray4        = iRGB 0x52 0x8b 0x8b

deep_pink1              :: RGB255
deep_pink1              = iRGB 0xff 0x14 0x93

deep_pink2              :: RGB255
deep_pink2              = iRGB 0xee 0x12 0x89

deep_pink3              :: RGB255
deep_pink3              = iRGB 0xcd 0x10 0x76

deep_pink4              :: RGB255
deep_pink4              = iRGB 0x8b 0x0a 0x50

deep_sky_blue1          :: RGB255
deep_sky_blue1          = iRGB 0x00 0xbf 0xff

deep_sky_blue2          :: RGB255
deep_sky_blue2          = iRGB 0x00 0xb2 0xee

deep_sky_blue3          :: RGB255
deep_sky_blue3          = iRGB 0x00 0x9a 0xcd

deep_sky_blue4          :: RGB255
deep_sky_blue4          = iRGB 0x00 0x68 0x8b

dodger_blue1            :: RGB255
dodger_blue1            = iRGB 0x1e 0x90 0xff

dodger_blue2            :: RGB255
dodger_blue2            = iRGB 0x1c 0x86 0xee

dodger_blue3            :: RGB255
dodger_blue3            = iRGB 0x18 0x74 0xcd

dodger_blue4            :: RGB255
dodger_blue4            = iRGB 0x10 0x4e 0x8b

firebrick1              :: RGB255
firebrick1              = iRGB 0xff 0x30 0x30

firebrick2              :: RGB255
firebrick2              = iRGB 0xee 0x2c 0x2c

firebrick3              :: RGB255
firebrick3              = iRGB 0xcd 0x26 0x26

firebrick4              :: RGB255
firebrick4              = iRGB 0x8b 0x1a 0x1a

gold1                   :: RGB255
gold1                   = iRGB 0xff 0xd7 0x00

gold2                   :: RGB255
gold2                   = iRGB 0xee 0xc9 0x00

gold3                   :: RGB255
gold3                   = iRGB 0xcd 0xad 0x00

gold4                   :: RGB255
gold4                   = iRGB 0x8b 0x75 0x00

goldenrod1              :: RGB255
goldenrod1              = iRGB 0xff 0xc1 0x25

goldenrod2              :: RGB255
goldenrod2              = iRGB 0xee 0xb4 0x22

goldenrod3              :: RGB255
goldenrod3              = iRGB 0xcd 0x9b 0x1d

goldenrod4              :: RGB255
goldenrod4              = iRGB 0x8b 0x69 0x14

green1                  :: RGB255
green1                  = iRGB 0x00 0xff 0x00

green2                  :: RGB255
green2                  = iRGB 0x00 0xee 0x00

green3                  :: RGB255
green3                  = iRGB 0x00 0xcd 0x00

green4                  :: RGB255
green4                  = iRGB 0x00 0x8b 0x00

honeydew1               :: RGB255
honeydew1               = iRGB 0xf0 0xff 0xf0

honeydew2               :: RGB255
honeydew2               = iRGB 0xe0 0xee 0xe0

honeydew3               :: RGB255
honeydew3               = iRGB 0xc1 0xcd 0xc1

honeydew4               :: RGB255
honeydew4               = iRGB 0x83 0x8b 0x83

hot_pink1               :: RGB255
hot_pink1               = iRGB 0xff 0x6e 0xb4

hot_pink2               :: RGB255
hot_pink2               = iRGB 0xee 0x6a 0xa7

hot_pink3               :: RGB255
hot_pink3               = iRGB 0xcd 0x60 0x90

hot_pink4               :: RGB255
hot_pink4               = iRGB 0x8b 0x3a 0x62

indian_red1             :: RGB255
indian_red1             = iRGB 0xff 0x6a 0x6a

indian_red2             :: RGB255
indian_red2             = iRGB 0xee 0x63 0x63

indian_red3             :: RGB255
indian_red3             = iRGB 0xcd 0x55 0x55

indian_red4             :: RGB255
indian_red4             = iRGB 0x8b 0x3a 0x3a

ivory1                  :: RGB255
ivory1                  = iRGB 0xff 0xff 0xf0

ivory2                  :: RGB255
ivory2                  = iRGB 0xee 0xee 0xe0

ivory3                  :: RGB255
ivory3                  = iRGB 0xcd 0xcd 0xc1

ivory4                  :: RGB255
ivory4                  = iRGB 0x8b 0x8b 0x83

khaki1                  :: RGB255
khaki1                  = iRGB 0xff 0xf6 0x8f

khaki2                  :: RGB255
khaki2                  = iRGB 0xee 0xe6 0x85

khaki3                  :: RGB255
khaki3                  = iRGB 0xcd 0xc6 0x73

khaki4                  :: RGB255
khaki4                  = iRGB 0x8b 0x86 0x4e

lavender_blush1         :: RGB255
lavender_blush1         = iRGB 0xff 0xf0 0xf5

lavender_blush2         :: RGB255
lavender_blush2         = iRGB 0xee 0xe0 0xe5

lavender_blush3         :: RGB255
lavender_blush3         = iRGB 0xcd 0xc1 0xc5

lavender_blush4         :: RGB255
lavender_blush4         = iRGB 0x8b 0x83 0x86

lemon_chiffon1          :: RGB255
lemon_chiffon1          = iRGB 0xff 0xfa 0xcd

lemon_chiffon2          :: RGB255
lemon_chiffon2          = iRGB 0xee 0xe9 0xbf

lemon_chiffon3          :: RGB255
lemon_chiffon3          = iRGB 0xcd 0xc9 0xa5

lemon_chiffon4          :: RGB255
lemon_chiffon4          = iRGB 0x8b 0x89 0x70

light_blue1             :: RGB255
light_blue1             = iRGB 0xbf 0xef 0xff

light_blue2             :: RGB255
light_blue2             = iRGB 0xb2 0xdf 0xee

light_blue3             :: RGB255
light_blue3             = iRGB 0x9a 0xc0 0xcd

light_blue4             :: RGB255
light_blue4             = iRGB 0x68 0x83 0x8b

light_cyan1             :: RGB255
light_cyan1             = iRGB 0xe0 0xff 0xff

light_cyan2             :: RGB255
light_cyan2             = iRGB 0xd1 0xee 0xee

light_cyan3             :: RGB255
light_cyan3             = iRGB 0xb4 0xcd 0xcd

light_cyan4             :: RGB255
light_cyan4             = iRGB 0x7a 0x8b 0x8b

light_goldenrod1        :: RGB255
light_goldenrod1        = iRGB 0xff 0xec 0x8b

light_goldenrod2        :: RGB255
light_goldenrod2        = iRGB 0xee 0xdc 0x82

light_goldenrod3        :: RGB255
light_goldenrod3        = iRGB 0xcd 0xbe 0x70

light_goldenrod4        :: RGB255
light_goldenrod4        = iRGB 0x8b 0x81 0x4c

light_pink1             :: RGB255
light_pink1             = iRGB 0xff 0xae 0xb9

light_pink2             :: RGB255
light_pink2             = iRGB 0xee 0xa2 0xad

light_pink3             :: RGB255
light_pink3             = iRGB 0xcd 0x8c 0x95

light_pink4             :: RGB255
light_pink4             = iRGB 0x8b 0x5f 0x65

light_salmon1           :: RGB255
light_salmon1           = iRGB 0xff 0xa0 0x7a

light_salmon2           :: RGB255
light_salmon2           = iRGB 0xee 0x95 0x72

light_salmon3           :: RGB255
light_salmon3           = iRGB 0xcd 0x81 0x62

light_salmon4           :: RGB255
light_salmon4           = iRGB 0x8b 0x57 0x42

light_sky_blue1         :: RGB255
light_sky_blue1         = iRGB 0xb0 0xe2 0xff

light_sky_blue2         :: RGB255
light_sky_blue2         = iRGB 0xa4 0xd3 0xee

light_sky_blue3         :: RGB255
light_sky_blue3         = iRGB 0x8d 0xb6 0xcd

light_sky_blue4         :: RGB255
light_sky_blue4         = iRGB 0x60 0x7b 0x8b

light_steel_blue1       :: RGB255
light_steel_blue1       = iRGB 0xca 0xe1 0xff

light_steel_blue2       :: RGB255
light_steel_blue2       = iRGB 0xbc 0xd2 0xee

light_steel_blue3       :: RGB255
light_steel_blue3       = iRGB 0xa2 0xb5 0xcd

light_steel_blue4       :: RGB255
light_steel_blue4       = iRGB 0x6e 0x7b 0x8b

light_yellow1           :: RGB255
light_yellow1           = iRGB 0xff 0xff 0xe0

light_yellow2           :: RGB255
light_yellow2           = iRGB 0xee 0xee 0xd1

light_yellow3           :: RGB255
light_yellow3           = iRGB 0xcd 0xcd 0xb4

light_yellow4           :: RGB255
light_yellow4           = iRGB 0x8b 0x8b 0x7a

magenta1                :: RGB255
magenta1                = iRGB 0xff 0x00 0xff

magenta2                :: RGB255
magenta2                = iRGB 0xee 0x00 0xee

magenta3                :: RGB255
magenta3                = iRGB 0xcd 0x00 0xcd

magenta4                :: RGB255
magenta4                = iRGB 0x8b 0x00 0x8b

maroon1                 :: RGB255
maroon1                 = iRGB 0xff 0x34 0xb3

maroon2                 :: RGB255
maroon2                 = iRGB 0xee 0x30 0xa7

maroon3                 :: RGB255
maroon3                 = iRGB 0xcd 0x29 0x90

maroon4                 :: RGB255
maroon4                 = iRGB 0x8b 0x1c 0x62

medium_orchid1          :: RGB255
medium_orchid1          = iRGB 0xe0 0x66 0xff

medium_orchid2          :: RGB255
medium_orchid2          = iRGB 0xd1 0x5f 0xee

medium_orchid3          :: RGB255
medium_orchid3          = iRGB 0xb4 0x52 0xcd

medium_orchid4          :: RGB255
medium_orchid4          = iRGB 0x7a 0x37 0x8b

medium_purple1          :: RGB255
medium_purple1          = iRGB 0xab 0x82 0xff

medium_purple2          :: RGB255
medium_purple2          = iRGB 0x9f 0x79 0xee

medium_purple3          :: RGB255
medium_purple3          = iRGB 0x89 0x68 0xcd

medium_purple4          :: RGB255
medium_purple4          = iRGB 0x5d 0x47 0x8b

misty_rose1             :: RGB255
misty_rose1             = iRGB 0xff 0xe4 0xe1

misty_rose2             :: RGB255
misty_rose2             = iRGB 0xee 0xd5 0xd2

misty_rose3             :: RGB255
misty_rose3             = iRGB 0xcd 0xb7 0xb5

misty_rose4             :: RGB255
misty_rose4             = iRGB 0x8b 0x7d 0x7b

navajo_white1           :: RGB255
navajo_white1           = iRGB 0xff 0xde 0xad

navajo_white2           :: RGB255
navajo_white2           = iRGB 0xee 0xcf 0xa1

navajo_white3           :: RGB255
navajo_white3           = iRGB 0xcd 0xb3 0x8b

navajo_white4           :: RGB255
navajo_white4           = iRGB 0x8b 0x79 0x5e

olive_drab1             :: RGB255
olive_drab1             = iRGB 0xc0 0xff 0x3e

olive_drab2             :: RGB255
olive_drab2             = iRGB 0xb3 0xee 0x3a

olive_drab3             :: RGB255
olive_drab3             = iRGB 0x9a 0xcd 0x32

olive_drab4             :: RGB255
olive_drab4             = iRGB 0x69 0x8b 0x22

orange1                 :: RGB255
orange1                 = iRGB 0xff 0xa5 0x00

orange2                 :: RGB255
orange2                 = iRGB 0xee 0x9a 0x00

orange3                 :: RGB255
orange3                 = iRGB 0xcd 0x85 0x00

orange4                 :: RGB255
orange4                 = iRGB 0x8b 0x5a 0x00

orange_red1             :: RGB255
orange_red1             = iRGB 0xff 0x45 0x00

orange_red2             :: RGB255
orange_red2             = iRGB 0xee 0x40 0x00

orange_red3             :: RGB255
orange_red3             = iRGB 0xcd 0x37 0x00

orange_red4             :: RGB255
orange_red4             = iRGB 0x8b 0x25 0x00

orchid1                 :: RGB255
orchid1                 = iRGB 0xff 0x83 0xfa

orchid2                 :: RGB255
orchid2                 = iRGB 0xee 0x7a 0xe9

orchid3                 :: RGB255
orchid3                 = iRGB 0xcd 0x69 0xc9

orchid4                 :: RGB255
orchid4                 = iRGB 0x8b 0x47 0x89

pale_green1             :: RGB255
pale_green1             = iRGB 0x9a 0xff 0x9a

pale_green2             :: RGB255
pale_green2             = iRGB 0x90 0xee 0x90

pale_green3             :: RGB255
pale_green3             = iRGB 0x7c 0xcd 0x7c

pale_green4             :: RGB255
pale_green4             = iRGB 0x54 0x8b 0x54

pale_turquoise1         :: RGB255
pale_turquoise1         = iRGB 0xbb 0xff 0xff

pale_turquoise2         :: RGB255
pale_turquoise2         = iRGB 0xae 0xee 0xee

pale_turquoise3         :: RGB255
pale_turquoise3         = iRGB 0x96 0xcd 0xcd

pale_turquoise4         :: RGB255
pale_turquoise4         = iRGB 0x66 0x8b 0x8b

pale_violet_red1        :: RGB255
pale_violet_red1        = iRGB 0xff 0x82 0xab

pale_violet_red2        :: RGB255
pale_violet_red2        = iRGB 0xee 0x79 0x9f

pale_violet_red3        :: RGB255
pale_violet_red3        = iRGB 0xcd 0x68 0x89

pale_violet_red4        :: RGB255
pale_violet_red4        = iRGB 0x8b 0x47 0x5d

peach_puff1             :: RGB255
peach_puff1             = iRGB 0xff 0xda 0xb9

peach_puff2             :: RGB255
peach_puff2             = iRGB 0xee 0xcb 0xad

peach_puff3             :: RGB255
peach_puff3             = iRGB 0xcd 0xaf 0x95

peach_puff4             :: RGB255
peach_puff4             = iRGB 0x8b 0x77 0x65

pink1                   :: RGB255
pink1                   = iRGB 0xff 0xb5 0xc5

pink2                   :: RGB255
pink2                   = iRGB 0xee 0xa9 0xb8

pink3                   :: RGB255
pink3                   = iRGB 0xcd 0x91 0x9e

pink4                   :: RGB255
pink4                   = iRGB 0x8b 0x63 0x6c

plum1                   :: RGB255
plum1                   = iRGB 0xff 0xbb 0xff

plum2                   :: RGB255
plum2                   = iRGB 0xee 0xae 0xee

plum3                   :: RGB255
plum3                   = iRGB 0xcd 0x96 0xcd

plum4                   :: RGB255
plum4                   = iRGB 0x8b 0x66 0x8b

purple1                 :: RGB255
purple1                 = iRGB 0x9b 0x30 0xff

purple2                 :: RGB255
purple2                 = iRGB 0x91 0x2c 0xee

purple3                 :: RGB255
purple3                 = iRGB 0x7d 0x26 0xcd

purple4                 :: RGB255
purple4                 = iRGB 0x55 0x1a 0x8b

red1                    :: RGB255
red1                    = iRGB 0xff 0x00 0x00

red2                    :: RGB255
red2                    = iRGB 0xee 0x00 0x00

red3                    :: RGB255
red3                    = iRGB 0xcd 0x00 0x00

red4                    :: RGB255
red4                    = iRGB 0x8b 0x00 0x00

rosy_brown1             :: RGB255
rosy_brown1             = iRGB 0xff 0xc1 0xc1

rosy_brown2             :: RGB255
rosy_brown2             = iRGB 0xee 0xb4 0xb4

rosy_brown3             :: RGB255
rosy_brown3             = iRGB 0xcd 0x9b 0x9b

rosy_brown4             :: RGB255
rosy_brown4             = iRGB 0x8b 0x69 0x69

royal_blue1             :: RGB255
royal_blue1             = iRGB 0x48 0x76 0xff

royal_blue2             :: RGB255
royal_blue2             = iRGB 0x43 0x6e 0xee

royal_blue3             :: RGB255
royal_blue3             = iRGB 0x3a 0x5f 0xcd

royal_blue4             :: RGB255
royal_blue4             = iRGB 0x27 0x40 0x8b


salmon1                 :: RGB255
salmon1                 = iRGB 0xff 0x8c 0x69

salmon2                 :: RGB255
salmon2                 = iRGB 0xee 0x82 0x62

salmon3                 :: RGB255
salmon3                 = iRGB 0xcd 0x70 0x54

salmon4                 :: RGB255
salmon4                 = iRGB 0x8b 0x4c 0x39

sea_green1              :: RGB255
sea_green1              = iRGB 0x54 0xff 0x9f

sea_green2              :: RGB255
sea_green2              = iRGB 0x4e 0xee 0x94

sea_green3              :: RGB255
sea_green3              = iRGB 0x43 0xcd 0x80

sea_green4              :: RGB255
sea_green4              = iRGB 0x2e 0x8b 0x57

seashell1               :: RGB255
seashell1               = iRGB 0xff 0xf5 0xee

seashell2               :: RGB255
seashell2               = iRGB 0xee 0xe5 0xde

seashell3               :: RGB255
seashell3               = iRGB 0xcd 0xc5 0xbf

seashell4               :: RGB255
seashell4               = iRGB 0x8b 0x86 0x82

sienna1                 :: RGB255
sienna1                 = iRGB 0xff 0x82 0x47

sienna2                 :: RGB255
sienna2                 = iRGB 0xee 0x79 0x42

sienna3                 :: RGB255
sienna3                 = iRGB 0xcd 0x68 0x39

sienna4                 :: RGB255
sienna4                 = iRGB 0x8b 0x47 0x26

sky_blue1               :: RGB255
sky_blue1               = iRGB 0x87 0xce 0xff

sky_blue2               :: RGB255
sky_blue2               = iRGB 0x7e 0xc0 0xee

sky_blue3               :: RGB255
sky_blue3               = iRGB 0x6c 0xa6 0xcd

sky_blue4               :: RGB255
sky_blue4               = iRGB 0x4a 0x70 0x8b

slate_blue1             :: RGB255
slate_blue1             = iRGB 0x83 0x6f 0xff

slate_blue2             :: RGB255
slate_blue2             = iRGB 0x7a 0x67 0xee

slate_blue3             :: RGB255
slate_blue3             = iRGB 0x69 0x59 0xcd

slate_blue4             :: RGB255
slate_blue4             = iRGB 0x47 0x3c 0x8b

slate_gray1             :: RGB255
slate_gray1             = iRGB 0xc6 0xe2 0xff

slate_gray2             :: RGB255
slate_gray2             = iRGB 0xb9 0xd3 0xee

slate_gray3             :: RGB255
slate_gray3             = iRGB 0x9f 0xb6 0xcd

slate_gray4             :: RGB255
slate_gray4             = iRGB 0x6c 0x7b 0x8b

snow1                   :: RGB255
snow1                   = iRGB 0xff 0xfa 0xfa

snow2                   :: RGB255
snow2                   = iRGB 0xee 0xe9 0xe9

snow3                   :: RGB255
snow3                   = iRGB 0xcd 0xc9 0xc9

snow4                   :: RGB255
snow4                   = iRGB 0x8b 0x89 0x89

spring_green1           :: RGB255
spring_green1           = iRGB 0x00 0xff 0x7f

spring_green2           :: RGB255
spring_green2           = iRGB 0x00 0xee 0x76

spring_green3           :: RGB255
spring_green3           = iRGB 0x00 0xcd 0x66

spring_green4           :: RGB255
spring_green4           = iRGB 0x00 0x8b 0x45

steel_blue1             :: RGB255
steel_blue1             = iRGB 0x63 0xb8 0xff

steel_blue2             :: RGB255
steel_blue2             = iRGB 0x5c 0xac 0xee

steel_blue3             :: RGB255
steel_blue3             = iRGB 0x4f 0x94 0xcd

steel_blue4             :: RGB255
steel_blue4             = iRGB 0x36 0x64 0x8b

tan1                    :: RGB255
tan1                    = iRGB 0xff 0xa5 0x4f

tan2                    :: RGB255
tan2                    = iRGB 0xee 0x9a 0x49

tan3                    :: RGB255
tan3                    = iRGB 0xcd 0x85 0x3f

tan4                    :: RGB255
tan4                    = iRGB 0x8b 0x5a 0x2b

thistle1                :: RGB255
thistle1                = iRGB 0xff 0xe1 0xff

thistle2                :: RGB255
thistle2                = iRGB 0xee 0xd2 0xee

thistle3                :: RGB255
thistle3                = iRGB 0xcd 0xb5 0xcd

thistle4                :: RGB255
thistle4                = iRGB 0x8b 0x7b 0x8b

tomato1                 :: RGB255
tomato1                 = iRGB 0xff 0x63 0x47

tomato2                 :: RGB255
tomato2                 = iRGB 0xee 0x5c 0x42

tomato3                 :: RGB255
tomato3                 = iRGB 0xcd 0x4f 0x39

tomato4                 :: RGB255
tomato4                 = iRGB 0x8b 0x36 0x26

turquoise1              :: RGB255
turquoise1              = iRGB 0x00 0xf5 0xff

turquoise2              :: RGB255
turquoise2              = iRGB 0x00 0xe5 0xee

turquoise3              :: RGB255
turquoise3              = iRGB 0x00 0xc5 0xcd

turquoise4              :: RGB255
turquoise4              = iRGB 0x00 0x86 0x8b

violet_red1             :: RGB255
violet_red1             = iRGB 0xff 0x3e 0x96

violet_red2             :: RGB255
violet_red2             = iRGB 0xee 0x3a 0x8c

violet_red3             :: RGB255
violet_red3             = iRGB 0xcd 0x32 0x78

violet_red4             :: RGB255
violet_red4             = iRGB 0x8b 0x22 0x52

wheat1                  :: RGB255
wheat1                  = iRGB 0xff 0xe7 0xba

wheat2                  :: RGB255
wheat2                  = iRGB 0xee 0xd8 0xae

wheat3                  :: RGB255
wheat3                  = iRGB 0xcd 0xba 0x96

wheat4                  :: RGB255
wheat4                  = iRGB 0x8b 0x7e 0x66

yellow1                 :: RGB255
yellow1                 = iRGB 0xff 0xff 0x00

yellow2                 :: RGB255
yellow2                 = iRGB 0xee 0xee 0x00

yellow3                 :: RGB255
yellow3                 = iRGB 0xcd 0xcd 0x00

yellow4                 :: RGB255
yellow4                 = iRGB 0x8b 0x8b 0x00






