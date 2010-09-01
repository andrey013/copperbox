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

import Wumpus.Core.Colour ( RGBi(..) )


--------------------------------------------------------------------------------

antique_white1          :: RGBi
antique_white1          = RGBi 0xff 0xef 0xdb

antique_white2          :: RGBi
antique_white2          = RGBi 0xee 0xdf 0xcc

antique_white3          :: RGBi
antique_white3          = RGBi 0xcd 0xc0 0xb0

antique_white4          :: RGBi
antique_white4          = RGBi 0x8b 0x83 0x78

aquamarine1             :: RGBi
aquamarine1             = RGBi 0x7f 0xff 0xd4

aquamarine2             :: RGBi
aquamarine2             = RGBi 0x76 0xee 0xc6

aquamarine3             :: RGBi
aquamarine3             = RGBi 0x66 0xcd 0xaa

aquamarine4             :: RGBi
aquamarine4             = RGBi 0x45 0x8b 0x74

azure1                  :: RGBi
azure1                  = RGBi 0xf0 0xff 0xff

azure2                  :: RGBi
azure2                  = RGBi 0xe0 0xee 0xee

azure3                  :: RGBi
azure3                  = RGBi 0xc1 0xcd 0xcd

azure4                  :: RGBi
azure4                  = RGBi 0x83 0x8b 0x8b

bisque1                 :: RGBi
bisque1                 = RGBi 0xff 0xe4 0xc4

bisque2                 :: RGBi
bisque2                 = RGBi 0xee 0xd5 0xb7

bisque3                 :: RGBi
bisque3                 = RGBi 0xcd 0xb7 0x9e

bisque4                 :: RGBi
bisque4                 = RGBi 0x8b 0x7d 0x6b

blue1                   :: RGBi
blue1                   = RGBi 0x00 0x00 0xff

blue2                   :: RGBi
blue2                   = RGBi 0x00 0x00 0xee

blue3                   :: RGBi
blue3                   = RGBi 0x00 0x00 0xcd

blue4                   :: RGBi
blue4                   = RGBi 0x00 0x00 0x8b

brown1                  :: RGBi
brown1                  = RGBi 0xff 0x40 0x40

brown2                  :: RGBi
brown2                  = RGBi 0xee 0x3b 0x3b

brown3                  :: RGBi
brown3                  = RGBi 0xcd 0x33 0x33

brown4                  :: RGBi
brown4                  = RGBi 0x8b 0x23 0x23

burlywood1              :: RGBi
burlywood1              = RGBi 0xff 0xd3 0x9b

burlywood2              :: RGBi
burlywood2              = RGBi 0xee 0xc5 0x91

burlywood3              :: RGBi
burlywood3              = RGBi 0xcd 0xaa 0x7d

burlywood4              :: RGBi
burlywood4              = RGBi 0x8b 0x73 0x55

cadet_blue1             :: RGBi
cadet_blue1             = RGBi 0x98 0xf5 0xff

cadet_blue2             :: RGBi
cadet_blue2             = RGBi 0x8e 0xe5 0xee

cadet_blue3             :: RGBi
cadet_blue3             = RGBi 0x7a 0xc5 0xcd

cadet_blue4             :: RGBi
cadet_blue4             = RGBi 0x53 0x86 0x8b

chartreuse1             :: RGBi
chartreuse1             = RGBi 0x7f 0xff 0x00

chartreuse2             :: RGBi
chartreuse2             = RGBi 0x76 0xee 0x00

chartreuse3             :: RGBi
chartreuse3             = RGBi 0x66 0xcd 0x00

chartreuse4             :: RGBi
chartreuse4             = RGBi 0x45 0x8b 0x00

chocolate1              :: RGBi
chocolate1              = RGBi 0xff 0x7f 0x24

chocolate2              :: RGBi
chocolate2              = RGBi 0xee 0x76 0x21

chocolate3              :: RGBi
chocolate3              = RGBi 0xcd 0x66 0x1d

chocolate4              :: RGBi
chocolate4              = RGBi 0x8b 0x45 0x13

coral1                  :: RGBi
coral1                  = RGBi 0xff 0x72 0x56

coral2                  :: RGBi
coral2                  = RGBi 0xee 0x6a 0x50

coral3                  :: RGBi
coral3                  = RGBi 0xcd 0x5b 0x45

coral4                  :: RGBi
coral4                  = RGBi 0x8b 0x3e 0x2f

cornsilk1               :: RGBi
cornsilk1               = RGBi 0xff 0xf8 0xdc

cornsilk2               :: RGBi
cornsilk2               = RGBi 0xee 0xe8 0xcd

cornsilk3               :: RGBi
cornsilk3               = RGBi 0xcd 0xc8 0xb1

cornsilk4               :: RGBi
cornsilk4               = RGBi 0x8b 0x88 0x78

cyan1                   :: RGBi
cyan1                   = RGBi 0x00 0xff 0xff

cyan2                   :: RGBi
cyan2                   = RGBi 0x00 0xee 0xee

cyan3                   :: RGBi
cyan3                   = RGBi 0x00 0xcd 0xcd

cyan4                   :: RGBi
cyan4                   = RGBi 0x00 0x8b 0x8b

dark_goldenrod1         :: RGBi
dark_goldenrod1         = RGBi 0xff 0xb9 0x0f

dark_goldenrod2         :: RGBi
dark_goldenrod2         = RGBi 0xee 0xad 0x0e

dark_goldenrod3         :: RGBi
dark_goldenrod3         = RGBi 0xcd 0x95 0x0c

dark_goldenrod4         :: RGBi
dark_goldenrod4         = RGBi 0x8b 0x65 0x08

dark_olive_green1       :: RGBi
dark_olive_green1       = RGBi 0xca 0xff 0x70

dark_olive_green2       :: RGBi
dark_olive_green2       = RGBi 0xbc 0xee 0x68

dark_olive_green3       :: RGBi
dark_olive_green3       = RGBi 0xa2 0xcd 0x5a

dark_olive_green4       :: RGBi
dark_olive_green4       = RGBi 0x6e 0x8b 0x3d

dark_orange1            :: RGBi
dark_orange1            = RGBi 0xff 0x7f 0x00

dark_orange2            :: RGBi
dark_orange2            = RGBi 0xee 0x76 0x00

dark_orange3            :: RGBi
dark_orange3            = RGBi 0xcd 0x66 0x00

dark_orange4            :: RGBi
dark_orange4            = RGBi 0x8b 0x45 0x00

dark_orchid1            :: RGBi
dark_orchid1            = RGBi 0xbf 0x3e 0xff

dark_orchid2            :: RGBi
dark_orchid2            = RGBi 0xb2 0x3a 0xee

dark_orchid3            :: RGBi
dark_orchid3            = RGBi 0x9a 0x32 0xcd

dark_orchid4            :: RGBi
dark_orchid4            = RGBi 0x68 0x22 0x8b

dark_sea_green1         :: RGBi
dark_sea_green1         = RGBi 0xc1 0xff 0xc1

dark_sea_green2         :: RGBi
dark_sea_green2         = RGBi 0xb4 0xee 0xb4

dark_sea_green3         :: RGBi
dark_sea_green3         = RGBi 0x9b 0xcd 0x9b

dark_sea_green4         :: RGBi
dark_sea_green4         = RGBi 0x69 0x8b 0x69

dark_slate_gray1        :: RGBi
dark_slate_gray1        = RGBi 0x97 0xff 0xff

dark_slate_gray2        :: RGBi
dark_slate_gray2        = RGBi 0x8d 0xee 0xee

dark_slate_gray3        :: RGBi
dark_slate_gray3        = RGBi 0x79 0xcd 0xcd

dark_slate_gray4        :: RGBi
dark_slate_gray4        = RGBi 0x52 0x8b 0x8b

deep_pink1              :: RGBi
deep_pink1              = RGBi 0xff 0x14 0x93

deep_pink2              :: RGBi
deep_pink2              = RGBi 0xee 0x12 0x89

deep_pink3              :: RGBi
deep_pink3              = RGBi 0xcd 0x10 0x76

deep_pink4              :: RGBi
deep_pink4              = RGBi 0x8b 0x0a 0x50

deep_sky_blue1          :: RGBi
deep_sky_blue1          = RGBi 0x00 0xbf 0xff

deep_sky_blue2          :: RGBi
deep_sky_blue2          = RGBi 0x00 0xb2 0xee

deep_sky_blue3          :: RGBi
deep_sky_blue3          = RGBi 0x00 0x9a 0xcd

deep_sky_blue4          :: RGBi
deep_sky_blue4          = RGBi 0x00 0x68 0x8b

dodger_blue1            :: RGBi
dodger_blue1            = RGBi 0x1e 0x90 0xff

dodger_blue2            :: RGBi
dodger_blue2            = RGBi 0x1c 0x86 0xee

dodger_blue3            :: RGBi
dodger_blue3            = RGBi 0x18 0x74 0xcd

dodger_blue4            :: RGBi
dodger_blue4            = RGBi 0x10 0x4e 0x8b

firebrick1              :: RGBi
firebrick1              = RGBi 0xff 0x30 0x30

firebrick2              :: RGBi
firebrick2              = RGBi 0xee 0x2c 0x2c

firebrick3              :: RGBi
firebrick3              = RGBi 0xcd 0x26 0x26

firebrick4              :: RGBi
firebrick4              = RGBi 0x8b 0x1a 0x1a

gold1                   :: RGBi
gold1                   = RGBi 0xff 0xd7 0x00

gold2                   :: RGBi
gold2                   = RGBi 0xee 0xc9 0x00

gold3                   :: RGBi
gold3                   = RGBi 0xcd 0xad 0x00

gold4                   :: RGBi
gold4                   = RGBi 0x8b 0x75 0x00

goldenrod1              :: RGBi
goldenrod1              = RGBi 0xff 0xc1 0x25

goldenrod2              :: RGBi
goldenrod2              = RGBi 0xee 0xb4 0x22

goldenrod3              :: RGBi
goldenrod3              = RGBi 0xcd 0x9b 0x1d

goldenrod4              :: RGBi
goldenrod4              = RGBi 0x8b 0x69 0x14

green1                  :: RGBi
green1                  = RGBi 0x00 0xff 0x00

green2                  :: RGBi
green2                  = RGBi 0x00 0xee 0x00

green3                  :: RGBi
green3                  = RGBi 0x00 0xcd 0x00

green4                  :: RGBi
green4                  = RGBi 0x00 0x8b 0x00

honeydew1               :: RGBi
honeydew1               = RGBi 0xf0 0xff 0xf0

honeydew2               :: RGBi
honeydew2               = RGBi 0xe0 0xee 0xe0

honeydew3               :: RGBi
honeydew3               = RGBi 0xc1 0xcd 0xc1

honeydew4               :: RGBi
honeydew4               = RGBi 0x83 0x8b 0x83

hot_pink1               :: RGBi
hot_pink1               = RGBi 0xff 0x6e 0xb4

hot_pink2               :: RGBi
hot_pink2               = RGBi 0xee 0x6a 0xa7

hot_pink3               :: RGBi
hot_pink3               = RGBi 0xcd 0x60 0x90

hot_pink4               :: RGBi
hot_pink4               = RGBi 0x8b 0x3a 0x62

indian_red1             :: RGBi
indian_red1             = RGBi 0xff 0x6a 0x6a

indian_red2             :: RGBi
indian_red2             = RGBi 0xee 0x63 0x63

indian_red3             :: RGBi
indian_red3             = RGBi 0xcd 0x55 0x55

indian_red4             :: RGBi
indian_red4             = RGBi 0x8b 0x3a 0x3a

ivory1                  :: RGBi
ivory1                  = RGBi 0xff 0xff 0xf0

ivory2                  :: RGBi
ivory2                  = RGBi 0xee 0xee 0xe0

ivory3                  :: RGBi
ivory3                  = RGBi 0xcd 0xcd 0xc1

ivory4                  :: RGBi
ivory4                  = RGBi 0x8b 0x8b 0x83

khaki1                  :: RGBi
khaki1                  = RGBi 0xff 0xf6 0x8f

khaki2                  :: RGBi
khaki2                  = RGBi 0xee 0xe6 0x85

khaki3                  :: RGBi
khaki3                  = RGBi 0xcd 0xc6 0x73

khaki4                  :: RGBi
khaki4                  = RGBi 0x8b 0x86 0x4e

lavender_blush1         :: RGBi
lavender_blush1         = RGBi 0xff 0xf0 0xf5

lavender_blush2         :: RGBi
lavender_blush2         = RGBi 0xee 0xe0 0xe5

lavender_blush3         :: RGBi
lavender_blush3         = RGBi 0xcd 0xc1 0xc5

lavender_blush4         :: RGBi
lavender_blush4         = RGBi 0x8b 0x83 0x86

lemon_chiffon1          :: RGBi
lemon_chiffon1          = RGBi 0xff 0xfa 0xcd

lemon_chiffon2          :: RGBi
lemon_chiffon2          = RGBi 0xee 0xe9 0xbf

lemon_chiffon3          :: RGBi
lemon_chiffon3          = RGBi 0xcd 0xc9 0xa5

lemon_chiffon4          :: RGBi
lemon_chiffon4          = RGBi 0x8b 0x89 0x70

light_blue1             :: RGBi
light_blue1             = RGBi 0xbf 0xef 0xff

light_blue2             :: RGBi
light_blue2             = RGBi 0xb2 0xdf 0xee

light_blue3             :: RGBi
light_blue3             = RGBi 0x9a 0xc0 0xcd

light_blue4             :: RGBi
light_blue4             = RGBi 0x68 0x83 0x8b

light_cyan1             :: RGBi
light_cyan1             = RGBi 0xe0 0xff 0xff

light_cyan2             :: RGBi
light_cyan2             = RGBi 0xd1 0xee 0xee

light_cyan3             :: RGBi
light_cyan3             = RGBi 0xb4 0xcd 0xcd

light_cyan4             :: RGBi
light_cyan4             = RGBi 0x7a 0x8b 0x8b

light_goldenrod1        :: RGBi
light_goldenrod1        = RGBi 0xff 0xec 0x8b

light_goldenrod2        :: RGBi
light_goldenrod2        = RGBi 0xee 0xdc 0x82

light_goldenrod3        :: RGBi
light_goldenrod3        = RGBi 0xcd 0xbe 0x70

light_goldenrod4        :: RGBi
light_goldenrod4        = RGBi 0x8b 0x81 0x4c

light_pink1             :: RGBi
light_pink1             = RGBi 0xff 0xae 0xb9

light_pink2             :: RGBi
light_pink2             = RGBi 0xee 0xa2 0xad

light_pink3             :: RGBi
light_pink3             = RGBi 0xcd 0x8c 0x95

light_pink4             :: RGBi
light_pink4             = RGBi 0x8b 0x5f 0x65

light_salmon1           :: RGBi
light_salmon1           = RGBi 0xff 0xa0 0x7a

light_salmon2           :: RGBi
light_salmon2           = RGBi 0xee 0x95 0x72

light_salmon3           :: RGBi
light_salmon3           = RGBi 0xcd 0x81 0x62

light_salmon4           :: RGBi
light_salmon4           = RGBi 0x8b 0x57 0x42

light_sky_blue1         :: RGBi
light_sky_blue1         = RGBi 0xb0 0xe2 0xff

light_sky_blue2         :: RGBi
light_sky_blue2         = RGBi 0xa4 0xd3 0xee

light_sky_blue3         :: RGBi
light_sky_blue3         = RGBi 0x8d 0xb6 0xcd

light_sky_blue4         :: RGBi
light_sky_blue4         = RGBi 0x60 0x7b 0x8b

light_steel_blue1       :: RGBi
light_steel_blue1       = RGBi 0xca 0xe1 0xff

light_steel_blue2       :: RGBi
light_steel_blue2       = RGBi 0xbc 0xd2 0xee

light_steel_blue3       :: RGBi
light_steel_blue3       = RGBi 0xa2 0xb5 0xcd

light_steel_blue4       :: RGBi
light_steel_blue4       = RGBi 0x6e 0x7b 0x8b

light_yellow1           :: RGBi
light_yellow1           = RGBi 0xff 0xff 0xe0

light_yellow2           :: RGBi
light_yellow2           = RGBi 0xee 0xee 0xd1

light_yellow3           :: RGBi
light_yellow3           = RGBi 0xcd 0xcd 0xb4

light_yellow4           :: RGBi
light_yellow4           = RGBi 0x8b 0x8b 0x7a

magenta1                :: RGBi
magenta1                = RGBi 0xff 0x00 0xff

magenta2                :: RGBi
magenta2                = RGBi 0xee 0x00 0xee

magenta3                :: RGBi
magenta3                = RGBi 0xcd 0x00 0xcd

magenta4                :: RGBi
magenta4                = RGBi 0x8b 0x00 0x8b

maroon1                 :: RGBi
maroon1                 = RGBi 0xff 0x34 0xb3

maroon2                 :: RGBi
maroon2                 = RGBi 0xee 0x30 0xa7

maroon3                 :: RGBi
maroon3                 = RGBi 0xcd 0x29 0x90

maroon4                 :: RGBi
maroon4                 = RGBi 0x8b 0x1c 0x62

medium_orchid1          :: RGBi
medium_orchid1          = RGBi 0xe0 0x66 0xff

medium_orchid2          :: RGBi
medium_orchid2          = RGBi 0xd1 0x5f 0xee

medium_orchid3          :: RGBi
medium_orchid3          = RGBi 0xb4 0x52 0xcd

medium_orchid4          :: RGBi
medium_orchid4          = RGBi 0x7a 0x37 0x8b

medium_purple1          :: RGBi
medium_purple1          = RGBi 0xab 0x82 0xff

medium_purple2          :: RGBi
medium_purple2          = RGBi 0x9f 0x79 0xee

medium_purple3          :: RGBi
medium_purple3          = RGBi 0x89 0x68 0xcd

medium_purple4          :: RGBi
medium_purple4          = RGBi 0x5d 0x47 0x8b

misty_rose1             :: RGBi
misty_rose1             = RGBi 0xff 0xe4 0xe1

misty_rose2             :: RGBi
misty_rose2             = RGBi 0xee 0xd5 0xd2

misty_rose3             :: RGBi
misty_rose3             = RGBi 0xcd 0xb7 0xb5

misty_rose4             :: RGBi
misty_rose4             = RGBi 0x8b 0x7d 0x7b

navajo_white1           :: RGBi
navajo_white1           = RGBi 0xff 0xde 0xad

navajo_white2           :: RGBi
navajo_white2           = RGBi 0xee 0xcf 0xa1

navajo_white3           :: RGBi
navajo_white3           = RGBi 0xcd 0xb3 0x8b

navajo_white4           :: RGBi
navajo_white4           = RGBi 0x8b 0x79 0x5e

olive_drab1             :: RGBi
olive_drab1             = RGBi 0xc0 0xff 0x3e

olive_drab2             :: RGBi
olive_drab2             = RGBi 0xb3 0xee 0x3a

olive_drab3             :: RGBi
olive_drab3             = RGBi 0x9a 0xcd 0x32

olive_drab4             :: RGBi
olive_drab4             = RGBi 0x69 0x8b 0x22

orange1                 :: RGBi
orange1                 = RGBi 0xff 0xa5 0x00

orange2                 :: RGBi
orange2                 = RGBi 0xee 0x9a 0x00

orange3                 :: RGBi
orange3                 = RGBi 0xcd 0x85 0x00

orange4                 :: RGBi
orange4                 = RGBi 0x8b 0x5a 0x00

orange_red1             :: RGBi
orange_red1             = RGBi 0xff 0x45 0x00

orange_red2             :: RGBi
orange_red2             = RGBi 0xee 0x40 0x00

orange_red3             :: RGBi
orange_red3             = RGBi 0xcd 0x37 0x00

orange_red4             :: RGBi
orange_red4             = RGBi 0x8b 0x25 0x00

orchid1                 :: RGBi
orchid1                 = RGBi 0xff 0x83 0xfa

orchid2                 :: RGBi
orchid2                 = RGBi 0xee 0x7a 0xe9

orchid3                 :: RGBi
orchid3                 = RGBi 0xcd 0x69 0xc9

orchid4                 :: RGBi
orchid4                 = RGBi 0x8b 0x47 0x89

pale_green1             :: RGBi
pale_green1             = RGBi 0x9a 0xff 0x9a

pale_green2             :: RGBi
pale_green2             = RGBi 0x90 0xee 0x90

pale_green3             :: RGBi
pale_green3             = RGBi 0x7c 0xcd 0x7c

pale_green4             :: RGBi
pale_green4             = RGBi 0x54 0x8b 0x54

pale_turquoise1         :: RGBi
pale_turquoise1         = RGBi 0xbb 0xff 0xff

pale_turquoise2         :: RGBi
pale_turquoise2         = RGBi 0xae 0xee 0xee

pale_turquoise3         :: RGBi
pale_turquoise3         = RGBi 0x96 0xcd 0xcd

pale_turquoise4         :: RGBi
pale_turquoise4         = RGBi 0x66 0x8b 0x8b

pale_violet_red1        :: RGBi
pale_violet_red1        = RGBi 0xff 0x82 0xab

pale_violet_red2        :: RGBi
pale_violet_red2        = RGBi 0xee 0x79 0x9f

pale_violet_red3        :: RGBi
pale_violet_red3        = RGBi 0xcd 0x68 0x89

pale_violet_red4        :: RGBi
pale_violet_red4        = RGBi 0x8b 0x47 0x5d

peach_puff1             :: RGBi
peach_puff1             = RGBi 0xff 0xda 0xb9

peach_puff2             :: RGBi
peach_puff2             = RGBi 0xee 0xcb 0xad

peach_puff3             :: RGBi
peach_puff3             = RGBi 0xcd 0xaf 0x95

peach_puff4             :: RGBi
peach_puff4             = RGBi 0x8b 0x77 0x65

pink1                   :: RGBi
pink1                   = RGBi 0xff 0xb5 0xc5

pink2                   :: RGBi
pink2                   = RGBi 0xee 0xa9 0xb8

pink3                   :: RGBi
pink3                   = RGBi 0xcd 0x91 0x9e

pink4                   :: RGBi
pink4                   = RGBi 0x8b 0x63 0x6c

plum1                   :: RGBi
plum1                   = RGBi 0xff 0xbb 0xff

plum2                   :: RGBi
plum2                   = RGBi 0xee 0xae 0xee

plum3                   :: RGBi
plum3                   = RGBi 0xcd 0x96 0xcd

plum4                   :: RGBi
plum4                   = RGBi 0x8b 0x66 0x8b

purple1                 :: RGBi
purple1                 = RGBi 0x9b 0x30 0xff

purple2                 :: RGBi
purple2                 = RGBi 0x91 0x2c 0xee

purple3                 :: RGBi
purple3                 = RGBi 0x7d 0x26 0xcd

purple4                 :: RGBi
purple4                 = RGBi 0x55 0x1a 0x8b

red1                    :: RGBi
red1                    = RGBi 0xff 0x00 0x00

red2                    :: RGBi
red2                    = RGBi 0xee 0x00 0x00

red3                    :: RGBi
red3                    = RGBi 0xcd 0x00 0x00

red4                    :: RGBi
red4                    = RGBi 0x8b 0x00 0x00

rosy_brown1             :: RGBi
rosy_brown1             = RGBi 0xff 0xc1 0xc1

rosy_brown2             :: RGBi
rosy_brown2             = RGBi 0xee 0xb4 0xb4

rosy_brown3             :: RGBi
rosy_brown3             = RGBi 0xcd 0x9b 0x9b

rosy_brown4             :: RGBi
rosy_brown4             = RGBi 0x8b 0x69 0x69

royal_blue1             :: RGBi
royal_blue1             = RGBi 0x48 0x76 0xff

royal_blue2             :: RGBi
royal_blue2             = RGBi 0x43 0x6e 0xee

royal_blue3             :: RGBi
royal_blue3             = RGBi 0x3a 0x5f 0xcd

royal_blue4             :: RGBi
royal_blue4             = RGBi 0x27 0x40 0x8b


salmon1                 :: RGBi
salmon1                 = RGBi 0xff 0x8c 0x69

salmon2                 :: RGBi
salmon2                 = RGBi 0xee 0x82 0x62

salmon3                 :: RGBi
salmon3                 = RGBi 0xcd 0x70 0x54

salmon4                 :: RGBi
salmon4                 = RGBi 0x8b 0x4c 0x39

sea_green1              :: RGBi
sea_green1              = RGBi 0x54 0xff 0x9f

sea_green2              :: RGBi
sea_green2              = RGBi 0x4e 0xee 0x94

sea_green3              :: RGBi
sea_green3              = RGBi 0x43 0xcd 0x80

sea_green4              :: RGBi
sea_green4              = RGBi 0x2e 0x8b 0x57

seashell1               :: RGBi
seashell1               = RGBi 0xff 0xf5 0xee

seashell2               :: RGBi
seashell2               = RGBi 0xee 0xe5 0xde

seashell3               :: RGBi
seashell3               = RGBi 0xcd 0xc5 0xbf

seashell4               :: RGBi
seashell4               = RGBi 0x8b 0x86 0x82

sienna1                 :: RGBi
sienna1                 = RGBi 0xff 0x82 0x47

sienna2                 :: RGBi
sienna2                 = RGBi 0xee 0x79 0x42

sienna3                 :: RGBi
sienna3                 = RGBi 0xcd 0x68 0x39

sienna4                 :: RGBi
sienna4                 = RGBi 0x8b 0x47 0x26

sky_blue1               :: RGBi
sky_blue1               = RGBi 0x87 0xce 0xff

sky_blue2               :: RGBi
sky_blue2               = RGBi 0x7e 0xc0 0xee

sky_blue3               :: RGBi
sky_blue3               = RGBi 0x6c 0xa6 0xcd

sky_blue4               :: RGBi
sky_blue4               = RGBi 0x4a 0x70 0x8b

slate_blue1             :: RGBi
slate_blue1             = RGBi 0x83 0x6f 0xff

slate_blue2             :: RGBi
slate_blue2             = RGBi 0x7a 0x67 0xee

slate_blue3             :: RGBi
slate_blue3             = RGBi 0x69 0x59 0xcd

slate_blue4             :: RGBi
slate_blue4             = RGBi 0x47 0x3c 0x8b

slate_gray1             :: RGBi
slate_gray1             = RGBi 0xc6 0xe2 0xff

slate_gray2             :: RGBi
slate_gray2             = RGBi 0xb9 0xd3 0xee

slate_gray3             :: RGBi
slate_gray3             = RGBi 0x9f 0xb6 0xcd

slate_gray4             :: RGBi
slate_gray4             = RGBi 0x6c 0x7b 0x8b

snow1                   :: RGBi
snow1                   = RGBi 0xff 0xfa 0xfa

snow2                   :: RGBi
snow2                   = RGBi 0xee 0xe9 0xe9

snow3                   :: RGBi
snow3                   = RGBi 0xcd 0xc9 0xc9

snow4                   :: RGBi
snow4                   = RGBi 0x8b 0x89 0x89

spring_green1           :: RGBi
spring_green1           = RGBi 0x00 0xff 0x7f

spring_green2           :: RGBi
spring_green2           = RGBi 0x00 0xee 0x76

spring_green3           :: RGBi
spring_green3           = RGBi 0x00 0xcd 0x66

spring_green4           :: RGBi
spring_green4           = RGBi 0x00 0x8b 0x45

steel_blue1             :: RGBi
steel_blue1             = RGBi 0x63 0xb8 0xff

steel_blue2             :: RGBi
steel_blue2             = RGBi 0x5c 0xac 0xee

steel_blue3             :: RGBi
steel_blue3             = RGBi 0x4f 0x94 0xcd

steel_blue4             :: RGBi
steel_blue4             = RGBi 0x36 0x64 0x8b

tan1                    :: RGBi
tan1                    = RGBi 0xff 0xa5 0x4f

tan2                    :: RGBi
tan2                    = RGBi 0xee 0x9a 0x49

tan3                    :: RGBi
tan3                    = RGBi 0xcd 0x85 0x3f

tan4                    :: RGBi
tan4                    = RGBi 0x8b 0x5a 0x2b

thistle1                :: RGBi
thistle1                = RGBi 0xff 0xe1 0xff

thistle2                :: RGBi
thistle2                = RGBi 0xee 0xd2 0xee

thistle3                :: RGBi
thistle3                = RGBi 0xcd 0xb5 0xcd

thistle4                :: RGBi
thistle4                = RGBi 0x8b 0x7b 0x8b

tomato1                 :: RGBi
tomato1                 = RGBi 0xff 0x63 0x47

tomato2                 :: RGBi
tomato2                 = RGBi 0xee 0x5c 0x42

tomato3                 :: RGBi
tomato3                 = RGBi 0xcd 0x4f 0x39

tomato4                 :: RGBi
tomato4                 = RGBi 0x8b 0x36 0x26

turquoise1              :: RGBi
turquoise1              = RGBi 0x00 0xf5 0xff

turquoise2              :: RGBi
turquoise2              = RGBi 0x00 0xe5 0xee

turquoise3              :: RGBi
turquoise3              = RGBi 0x00 0xc5 0xcd

turquoise4              :: RGBi
turquoise4              = RGBi 0x00 0x86 0x8b

violet_red1             :: RGBi
violet_red1             = RGBi 0xff 0x3e 0x96

violet_red2             :: RGBi
violet_red2             = RGBi 0xee 0x3a 0x8c

violet_red3             :: RGBi
violet_red3             = RGBi 0xcd 0x32 0x78

violet_red4             :: RGBi
violet_red4             = RGBi 0x8b 0x22 0x52

wheat1                  :: RGBi
wheat1                  = RGBi 0xff 0xe7 0xba

wheat2                  :: RGBi
wheat2                  = RGBi 0xee 0xd8 0xae

wheat3                  :: RGBi
wheat3                  = RGBi 0xcd 0xba 0x96

wheat4                  :: RGBi
wheat4                  = RGBi 0x8b 0x7e 0x66

yellow1                 :: RGBi
yellow1                 = RGBi 0xff 0xff 0x00

yellow2                 :: RGBi
yellow2                 = RGBi 0xee 0xee 0x00

yellow3                 :: RGBi
yellow3                 = RGBi 0xcd 0xcd 0x00

yellow4                 :: RGBi
yellow4                 = RGBi 0x8b 0x8b 0x00






