--------------------------------------------------------------------------------
-- |
-- Module      :  HNotate.Marks
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined
--
-- Common annotations for LilyPond and Abc 
--
--------------------------------------------------------------------------------

module HNotate.Marks where

import HNotate.Document
import HNotate.Duration
import HNotate.NoteListDatatypes


makeLyDrum :: String -> (Duration -> Tile)
makeLyDrum name = \d -> Singleton $ RhythmicMark name d (M fs ())
  where 
    fs = (\() -> MarkF { _ly_output = \() -> text name,
                         _abc_output = \() -> emptyDoc })
                          
acousticbassdrum      :: Duration -> Tile
acousticbassdrum      = makeLyDrum "acousticbassdrum"
                         
bassdrum              :: Duration -> Tile
bassdrum              = makeLyDrum "bassdrum" 

hisidestick           :: Duration -> Tile
hisidestick           = makeLyDrum "hisidestick" 

sidestick             :: Duration -> Tile
sidestick             = makeLyDrum "sidestick"

losidestick           :: Duration -> Tile
losidestick           = makeLyDrum "losidestick"

acousticsnare         :: Duration -> Tile
acousticsnare         = makeLyDrum "acousticsnare"

snare                 :: Duration -> Tile
snare                 = makeLyDrum "snare"

handclap              :: Duration -> Tile
handclap              = makeLyDrum "handclap"

electricsnare         :: Duration -> Tile
electricsnare         = makeLyDrum "electricsnare"

lowfloortom           :: Duration -> Tile
lowfloortom           = makeLyDrum "lowfloortom"

closedhihat           :: Duration -> Tile
closedhihat           = makeLyDrum "closedhihat"

hihat                 :: Duration -> Tile
hihat                 = makeLyDrum "hihat"

highfloortom          :: Duration -> Tile
highfloortom          = makeLyDrum "highfloortom"

pedalhihat            :: Duration -> Tile
pedalhihat            = makeLyDrum "pedalhihat"

lowtom                :: Duration -> Tile
lowtom                = makeLyDrum "lowtom"

openhihat             :: Duration -> Tile
openhihat             = makeLyDrum "openhihat"

halfopenhihat         :: Duration -> Tile
halfopenhihat         = makeLyDrum "halfopenhihat"

lowmidtom             :: Duration -> Tile
lowmidtom             = makeLyDrum "lowmidtom"

himidtom              :: Duration -> Tile
himidtom              = makeLyDrum "himidtom"

crashcymbala          :: Duration -> Tile
crashcymbala          = makeLyDrum "crashcymbala"

crashcymbal           :: Duration -> Tile
crashcymbal           = makeLyDrum "crashcymbal"

hightom               :: Duration -> Tile
hightom               = makeLyDrum "hightom"

ridecymbala           :: Duration -> Tile
ridecymbala           = makeLyDrum "ridecymbala"

ridecymbal            :: Duration -> Tile
ridecymbal            = makeLyDrum "ridecymbal"

chinesecymbal         :: Duration -> Tile
chinesecymbal         = makeLyDrum "chinesecymbal"

ridebell              :: Duration -> Tile
ridebell              = makeLyDrum "ridebell"

tambourine            :: Duration -> Tile
tambourine            = makeLyDrum "tambourine"

splashcymbal          :: Duration -> Tile
splashcymbal          = makeLyDrum "splashcymbal"

cowbell               :: Duration -> Tile
cowbell               = makeLyDrum "cowbell"

crashcymbalb          :: Duration -> Tile
crashcymbalb          = makeLyDrum "crashcymbalb"

vibraslap             :: Duration -> Tile
vibraslap             = makeLyDrum "vibraslap"

ridecymbalb           :: Duration -> Tile
ridecymbalb           = makeLyDrum "ridecymbalb"

mutehibongo           :: Duration -> Tile
mutehibongo           = makeLyDrum "mutehibongo"

hibongo               :: Duration -> Tile
hibongo               = makeLyDrum "hibongo"

openhibongo           :: Duration -> Tile
openhibongo           = makeLyDrum "openhibongo"

mutelobongo           :: Duration -> Tile
mutelobongo           = makeLyDrum "mutelobongo"

lobongo               :: Duration -> Tile
lobongo               = makeLyDrum "lobongo"

openlobongo           :: Duration -> Tile
openlobongo           = makeLyDrum "openlobongo"

mutehiconga           :: Duration -> Tile
mutehiconga           = makeLyDrum "mutehiconga"

muteloconga           :: Duration -> Tile
muteloconga           = makeLyDrum "muteloconga"

openhiconga           :: Duration -> Tile
openhiconga           = makeLyDrum "openhiconga"

hiconga               :: Duration -> Tile
hiconga               = makeLyDrum "hiconga"

openloconga           :: Duration -> Tile
openloconga           = makeLyDrum "openloconga"

loconga               :: Duration -> Tile
loconga               = makeLyDrum "loconga"

hitimbale             :: Duration -> Tile
hitimbale             = makeLyDrum "hitimbale"

lotimbale             :: Duration -> Tile
lotimbale             = makeLyDrum "lotimbale"

hiagogo               :: Duration -> Tile
hiagogo               = makeLyDrum "hiagogo"

loagogo               :: Duration -> Tile
loagogo               = makeLyDrum "loagogo"

cabasa                :: Duration -> Tile
cabasa                = makeLyDrum "cabasa"

maracas               :: Duration -> Tile
maracas               = makeLyDrum "maracas"

shortwhistle          :: Duration -> Tile
shortwhistle          = makeLyDrum "shortwhistle"

longwhistle           :: Duration -> Tile
longwhistle           = makeLyDrum "longwhistle"

shortguiro            :: Duration -> Tile
shortguiro            = makeLyDrum "shortguiro"

longguiro             :: Duration -> Tile
longguiro             = makeLyDrum "longguiro"

guiro                 :: Duration -> Tile
guiro                 = makeLyDrum "guiro"

claves                :: Duration -> Tile
claves                = makeLyDrum "claves"

hiwoodblock           :: Duration -> Tile
hiwoodblock           = makeLyDrum "hiwoodblock"

lowoodblock           :: Duration -> Tile
lowoodblock           = makeLyDrum "lowoodblock"

mutecuica             :: Duration -> Tile
mutecuica             = makeLyDrum "mutecuica"

opencuica             :: Duration -> Tile
opencuica             = makeLyDrum "opencuica"

mutetriangle          :: Duration -> Tile
mutetriangle          = makeLyDrum "mutetriangle"

triangle              :: Duration -> Tile
triangle              = makeLyDrum "triangle"

opentriangle          :: Duration -> Tile
opentriangle          = makeLyDrum "opentriangle"

oneup                 :: Duration -> Tile
oneup                 = makeLyDrum "oneup"

twoup                 :: Duration -> Tile
twoup                 = makeLyDrum "twoup"

threeup               :: Duration -> Tile
threeup               = makeLyDrum "threeup"

fourup                :: Duration -> Tile
fourup                = makeLyDrum "fourup"

fiveup                :: Duration -> Tile
fiveup                = makeLyDrum "fiveup"

onedown               :: Duration -> Tile
onedown               = makeLyDrum "onedown"

twodown               :: Duration -> Tile
twodown               = makeLyDrum "twodown"

threedown             :: Duration -> Tile
threedown             = makeLyDrum "threedown"

fourdown              :: Duration -> Tile
fourdown              = makeLyDrum "fourdown"

fivedown              :: Duration -> Tile
fivedown              = makeLyDrum "fivedown"


