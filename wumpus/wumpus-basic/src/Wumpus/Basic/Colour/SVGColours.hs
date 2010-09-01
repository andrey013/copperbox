{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Colour.SVGColours
-- Copyright   :  (c) Stephen Tetley 2009-2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC with TypeFamilies and more
--
-- The SVG \'named colours\', as rgb [0,1] values 
--
--------------------------------------------------------------------------------

module Wumpus.Basic.Colour.SVGColours 
  (
    
  -- * Named colours
    alice_blue
  , antique_white
  , aqua
  , aquamarine
  , azure
  , beige
  , bisque
  , black
  , blanched_almond
  , blue
  , blue_violet
  , brown
  , burlywood
  , cadet_blue
  , chartreuse
  , chocolate
  , coral
  , cornflower_blue
  , cornsilk
  , crimson
  , cyan
  , dark_blue
  , dark_cyan
  , dark_goldenrod
  , dark_gray
  , dark_green
  , dark_grey
  , dark_khaki
  , dark_magenta
  , dark_olive_green
  , dark_orange
  , dark_orchid
  , dark_red
  , dark_salmon
  , dark_sea_green
  , dark_slate_blue
  , dark_slate_gray
  , dark_slate_grey
  , dark_turquoise
  , dark_violet
  , deep_pink
  , deep_sky_blue
  , dim_gray
  , dim_grey
  , dodger_blue
  , firebrick
  , floral_white
  , forest_green
  , fuchsia
  , gainsboro
  , ghost_white
  , gold
  , goldenrod
  , gray
  , grey
  , green
  , green_yellow
  , honeydew
  , hot_pink
  , indian_red
  , indigo
  , ivory
  , khaki
  , lavender
  , lavender_blush
  , lawn_green
  , lemon_chiffon
  , light_blue
  , light_coral
  , light_cyan
  , light_goldenrod_yellow
  , light_gray
  , light_green
  , light_grey
  , light_pink
  , light_salmon
  , light_sea_green
  , light_sky_blue
  , light_slate_gray
  , light_slate_grey
  , light_steel_blue
  , light_yellow
  , lime
  , lime_green
  , linen
  , magenta
  , maroon
  , medium_aquamarine
  , medium_blue
  , medium_orchid
  , medium_purple
  , medium_sea_green
  , medium_slate_blue
  , medium_spring_green
  , medium_turquoise
  , medium_violet_red
  , midnight_blue
  , mintcream
  , mistyrose
  , moccasin
  , navajo_white
  , navy
  , old_lace
  , olive
  , olive_drab
  , orange
  , orange_red
  , orchid
  , pale_goldenrod
  , pale_green
  , pale_turquoise
  , pale_violet_red
  , papayawhip
  , peachpuff
  , peru
  , pink
  , plum
  , powder_blue
  , purple
  , red
  , rosy_brown
  , royal_blue
  , saddle_brown
  , salmon
  , sandy_brown
  , sea_green
  , seashell
  , sienna
  , silver
  , sky_blue
  , slate_blue
  , slate_gray
  , slate_grey
  , snow
  , spring_green
  , steel_blue
  , tan
  , teal
  , thistle
  , tomato
  , turquoise
  , violet
  , wheat
  , white
  , whitesmoke
  , yellow
  , yellow_green
  
  ) where


import Wumpus.Core.Colour ( RGB255(..), iRGB )


import Prelude ( )
  


--------------------------------------------------------------------------------
  
alice_blue              :: RGB255
alice_blue              = iRGB 0xf0 0xf8 0xff

antique_white           :: RGB255 
antique_white           = iRGB 0xfa 0xeb 0xd7

aqua                    :: RGB255
aqua                    = iRGB 0x00 0xff 0xff

aquamarine              :: RGB255
aquamarine              = iRGB 0x7f 0xff 0xd4

azure                   :: RGB255
azure                   = iRGB 0xf0 0xff 0xff

beige                   :: RGB255
beige                   = iRGB 0xf5 0xf5 0xdc

bisque                  :: RGB255
bisque                  = iRGB 0xff 0xe4 0xc4

black                   :: RGB255
black                   = iRGB 0x00 0x00 0x00

blanched_almond         :: RGB255
blanched_almond         = iRGB 0xff 0xeb 0xcd

blue                    :: RGB255
blue                    = iRGB 0x00 0x00 0xff

blue_violet             :: RGB255
blue_violet             = iRGB 0x8a 0x2b 0xe2

brown                   :: RGB255
brown                   = iRGB 0xa5 0x2a 0x2a

burlywood               :: RGB255
burlywood               = iRGB 0xde 0xb8 0x87

cadet_blue              :: RGB255
cadet_blue              = iRGB 0x5f 0x9e 0xa0

chartreuse              :: RGB255
chartreuse              = iRGB 0x7f 0xff 0x00

chocolate               :: RGB255
chocolate               = iRGB 0xd2 0x69 0x1e

coral                   :: RGB255
coral                   = iRGB 0xff 0x7f 0x50

cornflower_blue         :: RGB255
cornflower_blue         = iRGB 0x64 0x95 0xed

cornsilk                :: RGB255
cornsilk                = iRGB 0xff 0xf8 0xdc

crimson                 :: RGB255
crimson                 = iRGB 0xdc 0x14 0x3c

cyan                    :: RGB255
cyan                    = iRGB 0x00 0xff 0xff

dark_blue               :: RGB255
dark_blue               = iRGB 0x00 0x00 0x8b

dark_cyan               :: RGB255
dark_cyan               = iRGB 0x00 0x8b 0x8b

dark_goldenrod          :: RGB255
dark_goldenrod          = iRGB 0xb8 0x86 0x0b

dark_gray               :: RGB255
dark_gray               = iRGB 0xa9 0xa9 0xa9

dark_green              :: RGB255
dark_green              = iRGB 0x00 0x64 0x00

dark_grey               :: RGB255
dark_grey               = iRGB 0xa9 0xa9 0xa9

dark_khaki              :: RGB255
dark_khaki              = iRGB 0xbd 0xb7 0x6b

dark_magenta            :: RGB255
dark_magenta            = iRGB 0x8b 0x00 0x8b

dark_olive_green        :: RGB255
dark_olive_green        = iRGB 0x55 0x6b 0x2f

dark_orange             :: RGB255
dark_orange             = iRGB 0xff 0x8c 0x00

dark_orchid             :: RGB255
dark_orchid             = iRGB 0x99 0x32 0xcc

dark_red                :: RGB255
dark_red                = iRGB 0x8b 0x00 0x00

dark_salmon             :: RGB255
dark_salmon             = iRGB 0xe9 0x96 0x7a

dark_sea_green          :: RGB255
dark_sea_green          = iRGB 0x8f 0xbc 0x8f

dark_slate_blue         :: RGB255
dark_slate_blue         = iRGB 0x48 0x3d 0x8b

dark_slate_gray         :: RGB255
dark_slate_gray         = iRGB 0x2f 0x4f 0x4f

dark_slate_grey         :: RGB255
dark_slate_grey         = iRGB 0x2f 0x4f 0x4f

dark_turquoise          :: RGB255
dark_turquoise          = iRGB 0x00 0xce 0xd1

dark_violet             :: RGB255
dark_violet             = iRGB 0x94 0x00 0xd3

deep_pink               :: RGB255
deep_pink               = iRGB 0xff 0x14 0x93

deep_sky_blue           :: RGB255
deep_sky_blue           = iRGB 0x00 0xbf 0xff

dim_gray                :: RGB255
dim_gray                = iRGB 0x69 0x69 0x69

dim_grey                :: RGB255
dim_grey                = iRGB 0x69 0x69 0x69

dodger_blue             :: RGB255
dodger_blue             = iRGB 0x1e 0x90 0xff

firebrick               :: RGB255
firebrick               = iRGB 0xb2 0x22 0x22

floral_white            :: RGB255
floral_white            = iRGB 0xff 0xfa 0xf0

forest_green            :: RGB255
forest_green            = iRGB 0x22 0x8b 0x22

fuchsia                 :: RGB255
fuchsia                 = iRGB 0xff 0x00 0xff

gainsboro               :: RGB255
gainsboro               = iRGB 0xdc 0xdc 0xdc

ghost_white             :: RGB255
ghost_white             = iRGB 0xf8 0xf8 0xff

gold                    :: RGB255
gold                    = iRGB 0xff 0xd7 0x00

goldenrod               :: RGB255
goldenrod               = iRGB 0xda 0xa5 0x20

gray                    :: RGB255
gray                    = iRGB 0x80 0x80 0x80

green                   :: RGB255
green                   = iRGB 0x00 0x80 0x00

green_yellow            :: RGB255
green_yellow            = iRGB 0xad 0xff 0x2f

grey                    :: RGB255
grey                    = iRGB 0x80 0x80 0x80

honeydew                :: RGB255
honeydew                = iRGB 0xf0 0xff 0xf0

hot_pink                :: RGB255
hot_pink                = iRGB 0xff 0x69 0xb4

indian_red              :: RGB255
indian_red              = iRGB 0xcd 0x5c 0x5c

indigo                  :: RGB255
indigo                  = iRGB 0x4b 0x00 0x82

ivory                   :: RGB255
ivory                   = iRGB 0xff 0xff 0xf0

khaki                   :: RGB255
khaki                   = iRGB 0xf0 0xe6 0x8c

lavender                :: RGB255
lavender                = iRGB 0xe6 0xe6 0xfa

lavender_blush          :: RGB255
lavender_blush          = iRGB 0xff 0xf0 0xf5

lawn_green              :: RGB255
lawn_green              = iRGB 0x7c 0xfc 0x00

lemon_chiffon           :: RGB255
lemon_chiffon           = iRGB 0xff 0xfa 0xcd

light_blue              :: RGB255
light_blue              = iRGB 0xad 0xd8 0xe6

light_coral             :: RGB255
light_coral             = iRGB 0xf0 0x80 0x80

light_cyan              :: RGB255
light_cyan              = iRGB 0xe0 0xff 0xff

light_goldenrod_yellow  :: RGB255
light_goldenrod_yellow  = iRGB 0xfa 0xfa 0xd2

light_gray              :: RGB255
light_gray              = iRGB 0xd3 0xd3 0xd3

light_green             :: RGB255
light_green             = iRGB 0x90 0xee 0x90

light_grey              :: RGB255
light_grey              = iRGB 0xd3 0xd3 0xd3

light_pink              :: RGB255
light_pink              = iRGB 0xff 0xb6 0xc1

light_salmon            :: RGB255
light_salmon            = iRGB 0xff 0xa0 0x7a

light_sea_green         :: RGB255
light_sea_green         = iRGB 0x20 0xb2 0xaa

light_sky_blue          :: RGB255
light_sky_blue          = iRGB 0x87 0xce 0xfa

light_slate_gray        :: RGB255
light_slate_gray        = iRGB 0x77 0x88 0x99

light_slate_grey        :: RGB255
light_slate_grey        = iRGB 0x77 0x88 0x99

light_steel_blue        :: RGB255
light_steel_blue        = iRGB 0xb0 0xc4 0xde

light_yellow            :: RGB255
light_yellow            = iRGB 0xff 0xff 0xe0

lime                    :: RGB255
lime                    = iRGB 0x00 0xff 0x00

lime_green              :: RGB255
lime_green              = iRGB 0x32 0xcd 0x32

linen                   :: RGB255
linen                   = iRGB 0xfa 0xf0 0xe6

magenta                 :: RGB255
magenta                 = iRGB 0xff 0x00 0xff

maroon                  :: RGB255
maroon                  = iRGB 0x80 0x00 0x00

medium_aquamarine       :: RGB255
medium_aquamarine       = iRGB 0x66 0xcd 0xaa

medium_blue             :: RGB255
medium_blue             = iRGB 0x00 0x00 0xcd

medium_orchid           :: RGB255
medium_orchid           = iRGB 0xba 0x55 0xd3

medium_purple           :: RGB255
medium_purple           = iRGB 0x93 0x70 0xdb

medium_sea_green        :: RGB255
medium_sea_green        = iRGB 0x3c 0xb3 0x71

medium_slate_blue       :: RGB255
medium_slate_blue       = iRGB 0x7b 0x68 0xee

medium_spring_green     :: RGB255
medium_spring_green     = iRGB 0x00 0xfa 0x9a

medium_turquoise        :: RGB255
medium_turquoise        = iRGB 0x48 0xd1 0xcc

medium_violet_red       :: RGB255
medium_violet_red       = iRGB 0xc7 0x15 0x85

midnight_blue           :: RGB255
midnight_blue           = iRGB 0x19 0x19 0x70

mintcream               :: RGB255
mintcream               = iRGB 0xf5 0xff 0xfa

mistyrose               :: RGB255
mistyrose               = iRGB 0xff 0xe4 0xe1

moccasin                :: RGB255
moccasin                = iRGB 0xff 0xe4 0xb5

navajo_white            :: RGB255
navajo_white            = iRGB 0xff 0xde 0xad

navy                    :: RGB255
navy                    = iRGB 0x00 0x00 0x80

old_lace                :: RGB255
old_lace                = iRGB 0xfd 0xf5 0xe6

olive                   :: RGB255
olive                   = iRGB 0x80 0x80 0x00

olive_drab              :: RGB255
olive_drab              = iRGB 0x6b 0x8e 0x23

orange                  :: RGB255
orange                  = iRGB 0xff 0xa5 0x00

orange_red              :: RGB255
orange_red              = iRGB 0xff 0x45 0x00

orchid                  :: RGB255
orchid                  = iRGB 0xda 0x70 0xd6

pale_goldenrod          :: RGB255
pale_goldenrod          = iRGB 0xee 0xe8 0xaa

pale_green              :: RGB255
pale_green              = iRGB 0x98 0xfb 0x98

pale_turquoise          :: RGB255
pale_turquoise          = iRGB 0xaf 0xee 0xee

pale_violet_red         :: RGB255
pale_violet_red         = iRGB 0xdb 0x70 0x93

papayawhip              :: RGB255
papayawhip              = iRGB 0xff 0xef 0xd5

peachpuff               :: RGB255
peachpuff               = iRGB 0xff 0xda 0xb9

peru                    :: RGB255
peru                    = iRGB 0xcd 0x85 0x3f

pink                    :: RGB255
pink                    = iRGB 0xff 0xc0 0xcb

plum                    :: RGB255
plum                    = iRGB 0xdd 0xa0 0xdd

powder_blue             :: RGB255
powder_blue             = iRGB 0xb0 0xe0 0xe6

purple                  :: RGB255
purple                  = iRGB 0x80 0x00 0x80

red                     :: RGB255
red                     = iRGB 0xff 0x00 0x00

rosy_brown              :: RGB255
rosy_brown              = iRGB 0xbc 0x8f 0x8f

royal_blue              :: RGB255
royal_blue              = iRGB 0x41 0x69 0xe1

saddle_brown            :: RGB255
saddle_brown            = iRGB 0x8b 0x45 0x13

salmon                  :: RGB255
salmon                  = iRGB 0xfa 0x80 0x72

sandy_brown             :: RGB255
sandy_brown             = iRGB 0xf4 0xa4 0x60

sea_green               :: RGB255
sea_green               = iRGB 0x2e 0x8b 0x57

seashell                :: RGB255
seashell                = iRGB 0xff 0xf5 0xee

sienna                  :: RGB255
sienna                  = iRGB 0xa0 0x52 0x2d

silver                  :: RGB255
silver                  = iRGB 0xc0 0xc0 0xc0

sky_blue                :: RGB255
sky_blue                = iRGB 0x87 0xce 0xeb

slate_blue              :: RGB255
slate_blue              = iRGB 0x6a 0x5a 0xcd

slate_gray              :: RGB255
slate_gray              = iRGB 0x70 0x80 0x90

slate_grey              :: RGB255
slate_grey              = iRGB 0x70 0x80 0x90

snow                    :: RGB255
snow                    = iRGB 0xff 0xfa 0xfa

spring_green            :: RGB255
spring_green            = iRGB 0x00 0xff 0x7f

steel_blue              :: RGB255
steel_blue              = iRGB 0x46 0x82 0xb4

tan                     :: RGB255
tan                     = iRGB 0xd2 0xb4 0x8c

teal                    :: RGB255
teal                    = iRGB 0x00 0x80 0x80

thistle                 :: RGB255
thistle                 = iRGB 0xd8 0xbf 0xd8

tomato                  :: RGB255
tomato                  = iRGB 0xff 0x63 0x47

turquoise               :: RGB255
turquoise               = iRGB 0x40 0xe0 0xd0

violet                  :: RGB255
violet                  = iRGB 0xee 0x82 0xee

wheat                   :: RGB255
wheat                   = iRGB 0xf5 0xde 0xb3

white                   :: RGB255
white                   = iRGB 0xff 0xff 0xff

whitesmoke              :: RGB255
whitesmoke              = iRGB 0xf5 0xf5 0xf5

yellow                  :: RGB255
yellow                  = iRGB 0xff 0xff 0x00

yellow_green            :: RGB255
yellow_green            = iRGB 0x9a 0xcd 0x32


