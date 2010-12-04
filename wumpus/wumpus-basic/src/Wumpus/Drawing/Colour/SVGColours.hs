{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Drawing.Colour.SVGColours
-- Copyright   :  (c) Stephen Tetley 2009-2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  unstable
-- Portability :  GHC
--
-- The SVG \'named colours\', as rgb [0,1] values 
--
--------------------------------------------------------------------------------

module Wumpus.Drawing.Colour.SVGColours 
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
  , papaya_whip
  , peach_puff
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


import Wumpus.Core.Colour ( RGBi(..) )


import Prelude ( )
  


--------------------------------------------------------------------------------
  
alice_blue              :: RGBi
alice_blue              = RGBi 0xf0 0xf8 0xff

antique_white           :: RGBi 
antique_white           = RGBi 0xfa 0xeb 0xd7

aqua                    :: RGBi
aqua                    = RGBi 0x00 0xff 0xff

aquamarine              :: RGBi
aquamarine              = RGBi 0x7f 0xff 0xd4

azure                   :: RGBi
azure                   = RGBi 0xf0 0xff 0xff

beige                   :: RGBi
beige                   = RGBi 0xf5 0xf5 0xdc

bisque                  :: RGBi
bisque                  = RGBi 0xff 0xe4 0xc4

black                   :: RGBi
black                   = RGBi 0x00 0x00 0x00

blanched_almond         :: RGBi
blanched_almond         = RGBi 0xff 0xeb 0xcd

blue                    :: RGBi
blue                    = RGBi 0x00 0x00 0xff

blue_violet             :: RGBi
blue_violet             = RGBi 0x8a 0x2b 0xe2

brown                   :: RGBi
brown                   = RGBi 0xa5 0x2a 0x2a

burlywood               :: RGBi
burlywood               = RGBi 0xde 0xb8 0x87

cadet_blue              :: RGBi
cadet_blue              = RGBi 0x5f 0x9e 0xa0

chartreuse              :: RGBi
chartreuse              = RGBi 0x7f 0xff 0x00

chocolate               :: RGBi
chocolate               = RGBi 0xd2 0x69 0x1e

coral                   :: RGBi
coral                   = RGBi 0xff 0x7f 0x50

cornflower_blue         :: RGBi
cornflower_blue         = RGBi 0x64 0x95 0xed

cornsilk                :: RGBi
cornsilk                = RGBi 0xff 0xf8 0xdc

crimson                 :: RGBi
crimson                 = RGBi 0xdc 0x14 0x3c

cyan                    :: RGBi
cyan                    = RGBi 0x00 0xff 0xff

dark_blue               :: RGBi
dark_blue               = RGBi 0x00 0x00 0x8b

dark_cyan               :: RGBi
dark_cyan               = RGBi 0x00 0x8b 0x8b

dark_goldenrod          :: RGBi
dark_goldenrod          = RGBi 0xb8 0x86 0x0b

dark_gray               :: RGBi
dark_gray               = RGBi 0xa9 0xa9 0xa9

dark_green              :: RGBi
dark_green              = RGBi 0x00 0x64 0x00

dark_grey               :: RGBi
dark_grey               = RGBi 0xa9 0xa9 0xa9

dark_khaki              :: RGBi
dark_khaki              = RGBi 0xbd 0xb7 0x6b

dark_magenta            :: RGBi
dark_magenta            = RGBi 0x8b 0x00 0x8b

dark_olive_green        :: RGBi
dark_olive_green        = RGBi 0x55 0x6b 0x2f

dark_orange             :: RGBi
dark_orange             = RGBi 0xff 0x8c 0x00

dark_orchid             :: RGBi
dark_orchid             = RGBi 0x99 0x32 0xcc

dark_red                :: RGBi
dark_red                = RGBi 0x8b 0x00 0x00

dark_salmon             :: RGBi
dark_salmon             = RGBi 0xe9 0x96 0x7a

dark_sea_green          :: RGBi
dark_sea_green          = RGBi 0x8f 0xbc 0x8f

dark_slate_blue         :: RGBi
dark_slate_blue         = RGBi 0x48 0x3d 0x8b

dark_slate_gray         :: RGBi
dark_slate_gray         = RGBi 0x2f 0x4f 0x4f

dark_slate_grey         :: RGBi
dark_slate_grey         = RGBi 0x2f 0x4f 0x4f

dark_turquoise          :: RGBi
dark_turquoise          = RGBi 0x00 0xce 0xd1

dark_violet             :: RGBi
dark_violet             = RGBi 0x94 0x00 0xd3

deep_pink               :: RGBi
deep_pink               = RGBi 0xff 0x14 0x93

deep_sky_blue           :: RGBi
deep_sky_blue           = RGBi 0x00 0xbf 0xff

dim_gray                :: RGBi
dim_gray                = RGBi 0x69 0x69 0x69

dim_grey                :: RGBi
dim_grey                = RGBi 0x69 0x69 0x69

dodger_blue             :: RGBi
dodger_blue             = RGBi 0x1e 0x90 0xff

firebrick               :: RGBi
firebrick               = RGBi 0xb2 0x22 0x22

floral_white            :: RGBi
floral_white            = RGBi 0xff 0xfa 0xf0

forest_green            :: RGBi
forest_green            = RGBi 0x22 0x8b 0x22

fuchsia                 :: RGBi
fuchsia                 = RGBi 0xff 0x00 0xff

gainsboro               :: RGBi
gainsboro               = RGBi 0xdc 0xdc 0xdc

ghost_white             :: RGBi
ghost_white             = RGBi 0xf8 0xf8 0xff

gold                    :: RGBi
gold                    = RGBi 0xff 0xd7 0x00

goldenrod               :: RGBi
goldenrod               = RGBi 0xda 0xa5 0x20

gray                    :: RGBi
gray                    = RGBi 0x80 0x80 0x80

green                   :: RGBi
green                   = RGBi 0x00 0x80 0x00

green_yellow            :: RGBi
green_yellow            = RGBi 0xad 0xff 0x2f

grey                    :: RGBi
grey                    = RGBi 0x80 0x80 0x80

honeydew                :: RGBi
honeydew                = RGBi 0xf0 0xff 0xf0

hot_pink                :: RGBi
hot_pink                = RGBi 0xff 0x69 0xb4

indian_red              :: RGBi
indian_red              = RGBi 0xcd 0x5c 0x5c

indigo                  :: RGBi
indigo                  = RGBi 0x4b 0x00 0x82

ivory                   :: RGBi
ivory                   = RGBi 0xff 0xff 0xf0

khaki                   :: RGBi
khaki                   = RGBi 0xf0 0xe6 0x8c

lavender                :: RGBi
lavender                = RGBi 0xe6 0xe6 0xfa

lavender_blush          :: RGBi
lavender_blush          = RGBi 0xff 0xf0 0xf5

lawn_green              :: RGBi
lawn_green              = RGBi 0x7c 0xfc 0x00

lemon_chiffon           :: RGBi
lemon_chiffon           = RGBi 0xff 0xfa 0xcd

light_blue              :: RGBi
light_blue              = RGBi 0xad 0xd8 0xe6

light_coral             :: RGBi
light_coral             = RGBi 0xf0 0x80 0x80

light_cyan              :: RGBi
light_cyan              = RGBi 0xe0 0xff 0xff

light_goldenrod_yellow  :: RGBi
light_goldenrod_yellow  = RGBi 0xfa 0xfa 0xd2

light_gray              :: RGBi
light_gray              = RGBi 0xd3 0xd3 0xd3

light_green             :: RGBi
light_green             = RGBi 0x90 0xee 0x90

light_grey              :: RGBi
light_grey              = RGBi 0xd3 0xd3 0xd3

light_pink              :: RGBi
light_pink              = RGBi 0xff 0xb6 0xc1

light_salmon            :: RGBi
light_salmon            = RGBi 0xff 0xa0 0x7a

light_sea_green         :: RGBi
light_sea_green         = RGBi 0x20 0xb2 0xaa

light_sky_blue          :: RGBi
light_sky_blue          = RGBi 0x87 0xce 0xfa

light_slate_gray        :: RGBi
light_slate_gray        = RGBi 0x77 0x88 0x99

light_slate_grey        :: RGBi
light_slate_grey        = RGBi 0x77 0x88 0x99

light_steel_blue        :: RGBi
light_steel_blue        = RGBi 0xb0 0xc4 0xde

light_yellow            :: RGBi
light_yellow            = RGBi 0xff 0xff 0xe0

lime                    :: RGBi
lime                    = RGBi 0x00 0xff 0x00

lime_green              :: RGBi
lime_green              = RGBi 0x32 0xcd 0x32

linen                   :: RGBi
linen                   = RGBi 0xfa 0xf0 0xe6

magenta                 :: RGBi
magenta                 = RGBi 0xff 0x00 0xff

maroon                  :: RGBi
maroon                  = RGBi 0x80 0x00 0x00

medium_aquamarine       :: RGBi
medium_aquamarine       = RGBi 0x66 0xcd 0xaa

medium_blue             :: RGBi
medium_blue             = RGBi 0x00 0x00 0xcd

medium_orchid           :: RGBi
medium_orchid           = RGBi 0xba 0x55 0xd3

medium_purple           :: RGBi
medium_purple           = RGBi 0x93 0x70 0xdb

medium_sea_green        :: RGBi
medium_sea_green        = RGBi 0x3c 0xb3 0x71

medium_slate_blue       :: RGBi
medium_slate_blue       = RGBi 0x7b 0x68 0xee

medium_spring_green     :: RGBi
medium_spring_green     = RGBi 0x00 0xfa 0x9a

medium_turquoise        :: RGBi
medium_turquoise        = RGBi 0x48 0xd1 0xcc

medium_violet_red       :: RGBi
medium_violet_red       = RGBi 0xc7 0x15 0x85

midnight_blue           :: RGBi
midnight_blue           = RGBi 0x19 0x19 0x70

mintcream               :: RGBi
mintcream               = RGBi 0xf5 0xff 0xfa

mistyrose               :: RGBi
mistyrose               = RGBi 0xff 0xe4 0xe1

moccasin                :: RGBi
moccasin                = RGBi 0xff 0xe4 0xb5

navajo_white            :: RGBi
navajo_white            = RGBi 0xff 0xde 0xad

navy                    :: RGBi
navy                    = RGBi 0x00 0x00 0x80

old_lace                :: RGBi
old_lace                = RGBi 0xfd 0xf5 0xe6

olive                   :: RGBi
olive                   = RGBi 0x80 0x80 0x00

olive_drab              :: RGBi
olive_drab              = RGBi 0x6b 0x8e 0x23

orange                  :: RGBi
orange                  = RGBi 0xff 0xa5 0x00

orange_red              :: RGBi
orange_red              = RGBi 0xff 0x45 0x00

orchid                  :: RGBi
orchid                  = RGBi 0xda 0x70 0xd6

pale_goldenrod          :: RGBi
pale_goldenrod          = RGBi 0xee 0xe8 0xaa

pale_green              :: RGBi
pale_green              = RGBi 0x98 0xfb 0x98

pale_turquoise          :: RGBi
pale_turquoise          = RGBi 0xaf 0xee 0xee

pale_violet_red         :: RGBi
pale_violet_red         = RGBi 0xdb 0x70 0x93

papaya_whip             :: RGBi
papaya_whip             = RGBi 0xff 0xef 0xd5

peach_puff              :: RGBi
peach_puff              = RGBi 0xff 0xda 0xb9

peru                    :: RGBi
peru                    = RGBi 0xcd 0x85 0x3f

pink                    :: RGBi
pink                    = RGBi 0xff 0xc0 0xcb

plum                    :: RGBi
plum                    = RGBi 0xdd 0xa0 0xdd

powder_blue             :: RGBi
powder_blue             = RGBi 0xb0 0xe0 0xe6

purple                  :: RGBi
purple                  = RGBi 0x80 0x00 0x80

red                     :: RGBi
red                     = RGBi 0xff 0x00 0x00

rosy_brown              :: RGBi
rosy_brown              = RGBi 0xbc 0x8f 0x8f

royal_blue              :: RGBi
royal_blue              = RGBi 0x41 0x69 0xe1

saddle_brown            :: RGBi
saddle_brown            = RGBi 0x8b 0x45 0x13

salmon                  :: RGBi
salmon                  = RGBi 0xfa 0x80 0x72

sandy_brown             :: RGBi
sandy_brown             = RGBi 0xf4 0xa4 0x60

sea_green               :: RGBi
sea_green               = RGBi 0x2e 0x8b 0x57

seashell                :: RGBi
seashell                = RGBi 0xff 0xf5 0xee

sienna                  :: RGBi
sienna                  = RGBi 0xa0 0x52 0x2d

silver                  :: RGBi
silver                  = RGBi 0xc0 0xc0 0xc0

sky_blue                :: RGBi
sky_blue                = RGBi 0x87 0xce 0xeb

slate_blue              :: RGBi
slate_blue              = RGBi 0x6a 0x5a 0xcd

slate_gray              :: RGBi
slate_gray              = RGBi 0x70 0x80 0x90

slate_grey              :: RGBi
slate_grey              = RGBi 0x70 0x80 0x90

snow                    :: RGBi
snow                    = RGBi 0xff 0xfa 0xfa

spring_green            :: RGBi
spring_green            = RGBi 0x00 0xff 0x7f

steel_blue              :: RGBi
steel_blue              = RGBi 0x46 0x82 0xb4

tan                     :: RGBi
tan                     = RGBi 0xd2 0xb4 0x8c

teal                    :: RGBi
teal                    = RGBi 0x00 0x80 0x80

thistle                 :: RGBi
thistle                 = RGBi 0xd8 0xbf 0xd8

tomato                  :: RGBi
tomato                  = RGBi 0xff 0x63 0x47

turquoise               :: RGBi
turquoise               = RGBi 0x40 0xe0 0xd0

violet                  :: RGBi
violet                  = RGBi 0xee 0x82 0xee

wheat                   :: RGBi
wheat                   = RGBi 0xf5 0xde 0xb3

white                   :: RGBi
white                   = RGBi 0xff 0xff 0xff

whitesmoke              :: RGBi
whitesmoke              = RGBi 0xf5 0xf5 0xf5

yellow                  :: RGBi
yellow                  = RGBi 0xff 0xff 0x00

yellow_green            :: RGBi
yellow_green            = RGBi 0x9a 0xcd 0x32





