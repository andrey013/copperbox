{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Neume.Extra.DrumPitches
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Named musical elements e.g. notes, rests, LilyPond drum pitches...
--
--------------------------------------------------------------------------------

module Neume.Extra.DrumPitches
  (
  
  -- * Drum pitches
  -- $drumpitchdoc 
    acousticbassdrum
  , bassdrum                        
  , hisidestick
  , sidestick
  , losidestick
  , acousticsnare
  , snare                 
  , handclap              
  , electricsnare         
  , lowfloortom           
  , closedhihat           
  , hihat                 
  , highfloortom          
  , pedalhihat            
  , lowtom                
  , openhihat             
  , halfopenhihat         
  , lowmidtom             
  , himidtom              
  , crashcymbala
  , crashcymbal           
  , hightom               
  , ridecymbala           
  , ridecymbal            
  , chinesecymbal         
  , ridebell
  , tambourine
  , splashcymbal
  , cowbell               
  , crashcymbalb
  , vibraslap
  , ridecymbalb
  , mutehibongo           
  , hibongo               
  , openhibongo           
  , mutelobongo           
  , lobongo               
  , openlobongo           
  , mutehiconga           
  , muteloconga           
  , openhiconga           
  , hiconga               
  , openloconga
  , loconga               
  , hitimbale             
  , lotimbale             
  , hiagogo               
  , loagogo               
  , cabasa                
  , maracas               
  , shortwhistle          
  , longwhistle           
  , shortguiro            
  , longguiro             
  , guiro                 
  , claves                
  , hiwoodblock           
  , lowoodblock           
  , mutecuica             
  , opencuica             
  , mutetriangle          
  , triangle              
  , opentriangle          
  , oneup                 
  , twoup                 
  , threeup               
  , fourup                
  , fiveup                
  , onedown               
  , twodown               
  , threedown             
  , fourdown              
  , fivedown

 ) where


import Neume.Extra.Percussion

--------------------------------------------------------------------------------
-- Drum pitches
-- $drumpitchdoc 
-- LilyPond percussion pitches. 

acousticbassdrum      :: DrumPitch
acousticbassdrum      = DrumPitch "acousticbassdrum"   "bda"

bassdrum              :: DrumPitch
bassdrum              = DrumPitch "bassdrum"           "bd"

hisidestick           :: DrumPitch
hisidestick           = DrumPitch "hisidestick"        "ssh"

sidestick             :: DrumPitch
sidestick             = DrumPitch "sidestick"          "ss"

losidestick           :: DrumPitch
losidestick           = DrumPitch "losidestick"        "ssl"

acousticsnare         :: DrumPitch
acousticsnare         = DrumPitch "acousticsnare"      "sna"

snare                 :: DrumPitch
snare                 = DrumPitch "snare"              "sn"

handclap              :: DrumPitch
handclap              = DrumPitch "handclap"           "hc"

electricsnare         :: DrumPitch
electricsnare         = DrumPitch "electricsnare"      "sne"

lowfloortom           :: DrumPitch
lowfloortom           = DrumPitch "lowfloortom"        "tomfl"

closedhihat           :: DrumPitch
closedhihat           = DrumPitch "closedhihat"        "hhc"

hihat                 :: DrumPitch
hihat                 = DrumPitch "hihat"              "hh"

highfloortom          :: DrumPitch
highfloortom          = DrumPitch "highfloortom"       "tomfh"

pedalhihat            :: DrumPitch
pedalhihat            = DrumPitch "pedalhihat"         "hhp"

lowtom                :: DrumPitch
lowtom                = DrumPitch "lowtom"             "toml"

openhihat             :: DrumPitch
openhihat             = DrumPitch "openhihat"          "hho"

halfopenhihat         :: DrumPitch
halfopenhihat         = DrumPitch "halfopenhihat"      "hhho"

lowmidtom             :: DrumPitch
lowmidtom             = DrumPitch "lowmidtom"          "tomml"

himidtom              :: DrumPitch
himidtom              = DrumPitch "himidtom"           "tommh"

crashcymbala          :: DrumPitch
crashcymbala          = DrumPitch "crashcymbala"       "cymca"

crashcymbal           :: DrumPitch
crashcymbal           = DrumPitch "crashcymbal"        "cymc"

hightom               :: DrumPitch
hightom               = DrumPitch "hightom"            "tomh"

ridecymbala           :: DrumPitch
ridecymbala           = DrumPitch "ridecymbala"        "cymra"

ridecymbal            :: DrumPitch
ridecymbal            = DrumPitch "ridecymbal"         "cymr"

chinesecymbal         :: DrumPitch
chinesecymbal         = DrumPitch "chinesecymbal"      "cymch"

ridebell              :: DrumPitch
ridebell              = DrumPitch "ridebell"           "rb"

tambourine            :: DrumPitch
tambourine            = DrumPitch "tambourine"         "tamb"

splashcymbal          :: DrumPitch
splashcymbal          = DrumPitch "splashcymbal"       "cyms"

cowbell               :: DrumPitch
cowbell               = DrumPitch "cowbell"            "cb"

crashcymbalb          :: DrumPitch
crashcymbalb          = DrumPitch "crashcymbalb"       "cymcb"

vibraslap             :: DrumPitch
vibraslap             = DrumPitch "vibraslap"          "vibs"

ridecymbalb           :: DrumPitch
ridecymbalb           = DrumPitch "ridecymbalb"        "cymrb"

mutehibongo           :: DrumPitch
mutehibongo           = DrumPitch "mutehibongo"        "bohm"

hibongo               :: DrumPitch
hibongo               = DrumPitch "hibongo"            "boh"

openhibongo           :: DrumPitch
openhibongo           = DrumPitch "openhibongo"        "boho"

mutelobongo           :: DrumPitch
mutelobongo           = DrumPitch "mutelobongo"        "bolm"

lobongo               :: DrumPitch
lobongo               = DrumPitch "lobongo"            "bol"

openlobongo           :: DrumPitch
openlobongo           = DrumPitch "openlobongo"        "bolo"

mutehiconga           :: DrumPitch
mutehiconga           = DrumPitch "mutehiconga"        "cghm"

muteloconga           :: DrumPitch
muteloconga           = DrumPitch "muteloconga"        "cglm"

openhiconga           :: DrumPitch
openhiconga           = DrumPitch "openhiconga"        "cgho"

hiconga               :: DrumPitch
hiconga               = DrumPitch "hiconga"            "cgh"

openloconga           :: DrumPitch
openloconga           = DrumPitch "openloconga"        "cglo"

loconga               :: DrumPitch
loconga               = DrumPitch "loconga"            "cgl"

hitimbale             :: DrumPitch
hitimbale             = DrumPitch "hitimbale"          "timh"

lotimbale             :: DrumPitch
lotimbale             = DrumPitch "lotimbale"          "timl"

hiagogo               :: DrumPitch
hiagogo               = DrumPitch "hiagogo"            "agh"

loagogo               :: DrumPitch
loagogo               = DrumPitch "loagogo"            "agl"

cabasa                :: DrumPitch
cabasa                = DrumPitch "cabasa"             "cab"

maracas               :: DrumPitch
maracas               = DrumPitch "maracas"            "mar"

shortwhistle          :: DrumPitch
shortwhistle          = DrumPitch "shortwhistle"       "whs"

longwhistle           :: DrumPitch
longwhistle           = DrumPitch "longwhistle"        "whl"

shortguiro            :: DrumPitch
shortguiro            = DrumPitch "shortguiro"         "guis"

longguiro             :: DrumPitch
longguiro             = DrumPitch "longguiro"          "guil"

guiro                 :: DrumPitch
guiro                 = DrumPitch "guiro"              "gui"

claves                :: DrumPitch
claves                = DrumPitch "claves"             "cl"

hiwoodblock           :: DrumPitch
hiwoodblock           = DrumPitch "hiwoodblock"        "whi"

lowoodblock           :: DrumPitch
lowoodblock           = DrumPitch "lowoodblock"        "wbl"

mutecuica             :: DrumPitch
mutecuica             = DrumPitch "mutecuica"          "cuim"

opencuica             :: DrumPitch
opencuica             = DrumPitch "opencuica"          "cuio"

mutetriangle          :: DrumPitch
mutetriangle          = DrumPitch "mutetriangle"       "trim"

triangle              :: DrumPitch
triangle              = DrumPitch "triangle"           "tri"

opentriangle          :: DrumPitch
opentriangle          = DrumPitch "opentriangle"       "trio"

oneup                 :: DrumPitch
oneup                 = DrumPitch "oneup"              "ua"

twoup                 :: DrumPitch
twoup                 = DrumPitch "twoup"              "ub"

threeup               :: DrumPitch
threeup               = DrumPitch "threeup"            "uc"

fourup                :: DrumPitch
fourup                = DrumPitch "fourup"             "ud"

fiveup                :: DrumPitch
fiveup                = DrumPitch "fiveup"             "ue"

onedown               :: DrumPitch
onedown               = DrumPitch "onedown"            "da"

twodown               :: DrumPitch
twodown               = DrumPitch "twodown"            "db"

threedown             :: DrumPitch
threedown             = DrumPitch "threedown"          "dc"

fourdown              :: DrumPitch
fourdown              = DrumPitch "fourdown"           "dd"

fivedown              :: DrumPitch
fivedown              = DrumPitch "fivedown"           "de"



