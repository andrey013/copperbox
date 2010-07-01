{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.SVGColours
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

module Wumpus.Basic.SVGColours 
  (
    
  -- * Named colours
    aliceBlue
  , antiqueWhite
  , aqua
  , aquamarine
  , azure
  , beige
  , bisque
  , black
  , blanchedAlmond
  , blue
  , blueViolet
  , brown
  , burlywood
  , cadetBlue
  , chartreuse
  , chocolate
  , coral
  , cornflowerBlue
  , cornsilk
  , crimson
  , cyan
  , darkBlue
  , darkCyan
  , darkGoldenrod
  , darkGray
  , darkGreen
  , darkGrey
  , darkKhaki
  , darkMagenta
  , darkOliveGreen
  , darkOrange
  , darkOrchid
  , darkRed
  , darkSalmon
  , darkSeaGreen
  , darkSlateBlue
  , darkSlateGray
  , darkSlateGrey
  , darkTurquoise
  , darkViolet
  , deepPink
  , deepSkyBlue
  , dimGray
  , dimGrey
  , dodgerBlue
  , firebrick
  , floralWhite
  , forestGreen
  , fuchsia
  , gainsboro
  , ghostWhite
  , gold
  , goldenrod
  , gray
  , grey
  , green
  , greenYellow
  , honeydew
  , hotPink
  , indianRed
  , indigo
  , ivory
  , khaki
  , lavender
  , lavenderBlush
  , lawnGreen
  , lemonChiffon
  , lightBlue
  , lightCoral
  , lightCyan
  , lightGoldenrodYellow
  , lightGray
  , lightGreen
  , lightGrey
  , lightPink
  , lightSalmon
  , lightSeaGreen
  , lightSkyBlue
  , lightSlateGray
  , lightSlateGrey
  , lightSteelBlue
  , lightYellow
  , lime
  , limeGreen
  , linen
  , magenta
  , maroon
  , mediumAquamarine
  , mediumBlue
  , mediumOrchid
  , mediumPurple
  , mediumSeaGreen
  , mediumSlateBlue
  , mediumSpringGreen
  , mediumTurquoise
  , mediumVioletRed
  , midnightBlue
  , mintcream
  , mistyrose
  , moccasin
  , navajoWhite
  , navy
  , oldlace
  , olive
  , oliveDrab
  , orange
  , orangeRed
  , orchid
  , paleGoldenrod
  , paleGreen
  , paleTurquoise
  , paleVioletRed
  , papayawhip
  , peachpuff
  , peru
  , pink
  , plum
  , powderBlue
  , purple
  , red
  , rosyBrown
  , royalBlue
  , saddleBrown
  , salmon
  , sandyBrown
  , seaGreen
  , seashell
  , sienna
  , silver
  , skyBlue
  , slateBlue
  , slateGray
  , slateGrey
  , snow
  , springGreen
  , steelBlue
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
  , yellowGreen
  
  ) where


import Wumpus.Core.Colour ( RGB3(..), DRGB )


import Prelude ( )
  



--------------------------------------------------------------------------------
  
aliceBlue           :: DRGB
aliceBlue           = RGB3 0.941  0.973  1.0

antiqueWhite        :: DRGB
antiqueWhite        = RGB3 0.980  0.922  0.843

aqua                :: DRGB
aqua                = RGB3 0.0  1.0  1.0

aquamarine          :: DRGB
aquamarine          = RGB3 0.498  1.0  0.831

azure               :: DRGB
azure               = RGB3 0.941  1.0  1.0

beige               :: DRGB  
beige               = RGB3 0.961  0.961  0.863

bisque              :: DRGB
bisque              = RGB3 1.0  0.894  0.769

black               :: DRGB
black               = RGB3 0.0  0.0  0.0

blanchedAlmond      :: DRGB
blanchedAlmond      = RGB3 1.0  0.922  0.804

blue                :: DRGB
blue                = RGB3 0.0  0.0  1.0

blueViolet          :: DRGB
blueViolet          = RGB3 0.541  0.169  0.886

brown               :: DRGB
brown               = RGB3 0.647  0.165  0.165

burlywood           :: DRGB
burlywood           = RGB3 0.871  0.722  0.529

cadetBlue           :: DRGB
cadetBlue           = RGB3 0.373  0.620  0.627

chartreuse          :: DRGB
chartreuse          = RGB3 0.498  1.0  0.0

chocolate           :: DRGB
chocolate           = RGB3 0.824  0.412  0.118

coral               :: DRGB
coral               = RGB3 1.0  0.498  0.314

cornflowerBlue      :: DRGB
cornflowerBlue      = RGB3 0.392  0.584  0.929

cornsilk            :: DRGB
cornsilk            = RGB3 1.0  0.973  0.863

crimson             :: DRGB
crimson             = RGB3 0.863  0.078  0.235

cyan                :: DRGB
cyan                = RGB3 0.0  1.0  1.0

darkBlue            :: DRGB
darkBlue            = RGB3 0.0  0.0  0.545

darkCyan            :: DRGB
darkCyan            = RGB3 0.0  0.545  0.545

darkGoldenrod       :: DRGB
darkGoldenrod       = RGB3 0.722  0.545  0.043

darkGray            :: DRGB
darkGray            = RGB3 0.663  0.663  0.663

darkGreen           :: DRGB
darkGreen           = RGB3 0.0  0.392  0.0

darkGrey            :: DRGB
darkGrey            = RGB3 0.663  0.663  0.663

darkKhaki           :: DRGB
darkKhaki           = RGB3 0.741  0.718  0.420

darkMagenta         :: DRGB
darkMagenta         = RGB3 0.545  0.0  0.545

darkOliveGreen      :: DRGB
darkOliveGreen      = RGB3 0.333  0.420  0.184

darkOrange          :: DRGB
darkOrange          = RGB3 1.0  0.549  0.0

darkOrchid          :: DRGB
darkOrchid          = RGB3 0.600  0.196  0.800

darkRed             :: DRGB
darkRed             = RGB3 0.545  0.0  0.0

darkSalmon          :: DRGB
darkSalmon          = RGB3 0.914  0.588  0.478

darkSeaGreen        :: DRGB
darkSeaGreen        = RGB3 0.561  0.737  0.561

darkSlateBlue       :: DRGB
darkSlateBlue       = RGB3 0.282  0.239  0.545

darkSlateGray       :: DRGB
darkSlateGray       = RGB3 0.184  0.310  0.310

darkSlateGrey       :: DRGB
darkSlateGrey       = RGB3 0.184  0.310  0.310

darkTurquoise       :: DRGB
darkTurquoise       = RGB3 0.0  0.808  0.820

darkViolet          :: DRGB
darkViolet          = RGB3 0.580  0.0  0.827

deepPink            :: DRGB
deepPink            = RGB3 1.0  0.078  0.576

deepSkyBlue         :: DRGB
deepSkyBlue         = RGB3 0.0  0.750  1.0

dimGray             :: DRGB
dimGray             = RGB3 0.412  0.412  0.412

dimGrey             :: DRGB
dimGrey             = RGB3 0.412  0.412  0.412

dodgerBlue          :: DRGB
dodgerBlue          = RGB3 0.118  0.565  1.0

firebrick           :: DRGB
firebrick           = RGB3 0.698  0.133  0.133

floralWhite         :: DRGB
floralWhite         = RGB3 1.0  0.980  0.941

forestGreen         :: DRGB
forestGreen         = RGB3 0.133  0.545  0.133

fuchsia             :: DRGB
fuchsia             = RGB3 1.0  0.0  1.0

gainsboro           :: DRGB
gainsboro           = RGB3 0.863  0.863  0.863

ghostWhite          :: DRGB
ghostWhite          = RGB3 0.973  0.973  1.0

gold                :: DRGB
gold                = RGB3 1.0  0.843  0.0

goldenrod           :: DRGB
goldenrod           = RGB3 0.855  0.647  0.125

gray                :: DRGB
gray                = RGB3 0.5  0.5  0.5

grey                :: DRGB
grey                = RGB3 0.5  0.5  0.5

green               :: DRGB
green               = RGB3 0.0  0.5  0.0

greenYellow         :: DRGB
greenYellow         = RGB3 0.678  1.0  0.184

honeydew            :: DRGB
honeydew            = RGB3 0.941  1.0  0.941

hotPink             :: DRGB
hotPink             = RGB3 1.0  0.412  0.706

indianRed           :: DRGB
indianRed           = RGB3 0.804  0.361  0.361

indigo              :: DRGB
indigo              = RGB3 0.294  0.0  0.510

ivory               :: DRGB
ivory               = RGB3 1.0  1.0  0.941

khaki               :: DRGB
khaki               = RGB3 0.941  0.902  0.549

lavender            :: DRGB
lavender            = RGB3 0.902  0.902  0.980

lavenderBlush       :: DRGB
lavenderBlush       = RGB3 1.0  0.941  0.961

lawnGreen           :: DRGB
lawnGreen           = RGB3 0.486  0.988  0.0

lemonChiffon        :: DRGB
lemonChiffon        = RGB3 1.0  0.980  0.804

lightBlue           :: DRGB
lightBlue           = RGB3 0.678  0.847  0.902

lightCoral          :: DRGB
lightCoral          = RGB3 0.941  0.5  0.5

lightCyan           :: DRGB
lightCyan           = RGB3 0.878  1.0  1.0

lightGoldenrodYellow  :: DRGB
lightGoldenrodYellow  = RGB3 0.980  0.980  0.824

lightGray           :: DRGB
lightGray           = RGB3 0.827  0.827  0.827

lightGreen          :: DRGB
lightGreen          = RGB3 0.565  0.933  0.565

lightGrey           :: DRGB
lightGrey           = RGB3 0.827  0.827  0.827

lightPink           :: DRGB
lightPink           = RGB3 1.0  0.714  0.757

lightSalmon         :: DRGB
lightSalmon         = RGB3 1.0  0.627  0.478

lightSeaGreen       :: DRGB
lightSeaGreen       = RGB3 0.125  0.698  0.666

lightSkyBlue        :: DRGB
lightSkyBlue        = RGB3 0.529  0.808  0.980

lightSlateGray      :: DRGB
lightSlateGray      = RGB3 0.466  0.533  0.600

lightSlateGrey      :: DRGB
lightSlateGrey      = RGB3 0.466  0.533  0.600

lightSteelBlue      :: DRGB
lightSteelBlue      = RGB3 0.690  0.769  0.871

lightYellow         :: DRGB
lightYellow         = RGB3 1.0  1.0  0.878

lime                :: DRGB
lime                = RGB3 0.0  1.0  0.0

limeGreen           :: DRGB
limeGreen           = RGB3 0.196  0.804  0.196

linen               :: DRGB
linen               = RGB3 0.980  0.941  0.902

magenta             :: DRGB
magenta             = RGB3 1.0  0.0  1.0

maroon              :: DRGB
maroon              = RGB3 0.5  0.0  0.0

mediumAquamarine    :: DRGB
mediumAquamarine    = RGB3 0.4  0.804  0.666

mediumBlue          :: DRGB
mediumBlue          = RGB3 0.0  0.0  0.804

mediumOrchid        :: DRGB
mediumOrchid        = RGB3 0.729  0.333  0.827

mediumPurple        :: DRGB
mediumPurple        = RGB3 0.576  0.439  0.859

mediumSeaGreen      :: DRGB
mediumSeaGreen      = RGB3 0.235  0.702  0.443

mediumSlateBlue     :: DRGB
mediumSlateBlue     = RGB3 0.482  0.408  0.933

mediumSpringGreen   :: DRGB
mediumSpringGreen   = RGB3 0.0  0.980  0.604

mediumTurquoise     :: DRGB
mediumTurquoise     = RGB3 0.282  0.820  0.800

mediumVioletRed     :: DRGB
mediumVioletRed     = RGB3 0.780  0.082  0.522

midnightBlue        :: DRGB
midnightBlue        = RGB3 0.098  0.098  0.439

mintcream           :: DRGB
mintcream           = RGB3 0.961  1.0  0.980

mistyrose           :: DRGB
mistyrose           = RGB3 1.0  0.894  0.882

moccasin            :: DRGB
moccasin            = RGB3 1.0  0.894  0.710

navajoWhite         :: DRGB
navajoWhite         = RGB3 1.0  0.871  0.678

navy                :: DRGB
navy                = RGB3 0.0  0.0  0.5

oldlace             :: DRGB
oldlace             = RGB3 0.992  0.961  0.902

olive               :: DRGB
olive               = RGB3 0.5  0.5  0.0

oliveDrab           :: DRGB
oliveDrab           = RGB3 0.420  0.557  0.137

orange              :: DRGB
orange              = RGB3 1.0  0.647  0.0

orangeRed           :: DRGB
orangeRed           = RGB3 1.0  0.271  0.0

orchid              :: DRGB
orchid              = RGB3 0.855  0.439  0.839

paleGoldenrod       :: DRGB
paleGoldenrod       = RGB3 0.933  0.910  0.666

paleGreen           :: DRGB
paleGreen           = RGB3 0.596  0.984  0.596

paleTurquoise       :: DRGB
paleTurquoise       = RGB3 0.686  0.933  0.933

paleVioletRed       :: DRGB
paleVioletRed       = RGB3 0.859  0.439  0.576

papayawhip          :: DRGB
papayawhip          = RGB3 1.0  0.937  0.835

peachpuff           :: DRGB
peachpuff           = RGB3 1.0  0.855  0.725

peru                :: DRGB
peru                = RGB3 0.804  0.522  0.247

pink                :: DRGB
pink                = RGB3 1.0  0.753  0.796

plum                :: DRGB
plum                = RGB3 0.867  0.627  0.867

powderBlue          :: DRGB
powderBlue          = RGB3 0.690  0.878  0.902

purple              :: DRGB
purple              = RGB3 0.5  0.0  0.5

red                 :: DRGB
red                 = RGB3 1.0  0.0  0.0

rosyBrown           :: DRGB
rosyBrown           = RGB3 0.737  0.561  0.561

royalBlue           :: DRGB
royalBlue           = RGB3 0.255  0.412  0.882

saddleBrown         :: DRGB
saddleBrown         = RGB3 0.545  0.271  0.075

salmon              :: DRGB
salmon              = RGB3 0.980  0.5  0.447

sandyBrown          :: DRGB
sandyBrown          = RGB3 0.957  0.643  0.376

seaGreen            :: DRGB
seaGreen            = RGB3 0.180  0.545  0.341

seashell            :: DRGB
seashell            = RGB3 1.0  0.961  0.933

sienna              :: DRGB
sienna              = RGB3 0.627  0.322  0.176

silver              :: DRGB
silver              = RGB3 0.753  0.753  0.753

skyBlue             :: DRGB
skyBlue             = RGB3 0.529  0.808  0.922

slateBlue           :: DRGB
slateBlue           = RGB3 0.416  0.353  0.804

slateGray           :: DRGB
slateGray           = RGB3 0.439  0.5  0.565

slateGrey           :: DRGB
slateGrey           = RGB3 0.439  0.5  0.565

snow                :: DRGB
snow                = RGB3 1.0  0.980  0.980

springGreen         :: DRGB
springGreen         = RGB3 0.0  1.0  0.498

steelBlue           :: DRGB
steelBlue           = RGB3 0.275  0.510  0.706

tan                 :: DRGB
tan                 = RGB3 0.824  0.706  0.549

teal                :: DRGB
teal                = RGB3 0.0  0.5  0.5

thistle             :: DRGB
thistle             = RGB3 0.847  0.750  0.847

tomato              :: DRGB
tomato              = RGB3 1.0  0.388  0.278

turquoise           :: DRGB
turquoise           = RGB3 0.251  0.878  0.816

violet              :: DRGB
violet              = RGB3 0.933  0.510  0.933

wheat               :: DRGB
wheat               = RGB3 0.961  0.871  0.702

white               :: DRGB
white               = RGB3 1.0  1.0  1.0

whitesmoke          :: DRGB
whitesmoke          = RGB3 0.961  0.961  0.961

yellow              :: DRGB
yellow              = RGB3 1.0  1.0  0.0

yellowGreen         :: DRGB
yellowGreen         = RGB3 0.604  0.804  0.196





