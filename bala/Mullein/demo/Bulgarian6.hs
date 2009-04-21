
-- This tune is `Bulgarian (?) 6` from the Exotic ABC songbook

-- ghci ...
-- :set -i../src

module Bulgarian6 where

import Mullein.AbcConvert
import qualified Mullein.AbcOutput as Abc
import Mullein.Bracket
import Mullein.Core
import Mullein.CoreTypes
import Mullein.Duration
import Mullein.LabelSet
import Mullein.NamedElements
import Mullein.Pitch
import Mullein.RS
import Mullein.Score
import Mullein.Utils

import qualified Mullein.LilyPondConvert as Ly
import qualified Mullein.LilyPondOutput as Ly

import Data.Ratio
import Text.PrettyPrint.Leijen ( putDoc )


main = putDoc $ Abc.output a_major (TimeSig 2 4) (repeat 4) bulgarian6

ly = putDoc $ Ly.output a_major bulg6 where
  bulg6 = Ly.convertToLy c4 part1_8


bulgarian6 :: Part
bulgarian6 = convertToAbc lset sixteenth part1_8 where
  lset = maybe (error "lset missing") id $ makeLabelSet a_major

-- tune1 = evaluatePart a_major twoFourTime part1_8

amMotif = motif a_major twoFourTime

twoFourTime :: MetricalSpec
twoFourTime = metricalSpec 2 4


part1_8 :: Part
part1_8 = part [repeated m1_4, repeated m5_8]



m1_4 :: Motif
m1_4 = amMotif $  primary bars1_4

m5_8 ::  Motif
m5_8 = amMotif $ primary bars5_8



bars1_4 :: [Element]
bars1_4 =  
  [ a4 # n16, b4 # n16, cis5 # n16, cis5 # n16, cis5 # n16, a4 # n16
            , cis5 # n16, cis5 # n16
  -- bar 2
  , cis5 # n16, a4 # n16, b4 # n16, cis5 # n16, b4 # n16, a4 # n16, a4 # n16
              , rest du16
  -- bar 3
  , e5 # n16, d5 # n16, cis5 # n16, b4 # n16, cis5 # n16, a4 # n16, b4 # n16
            , cis5 # n16
  -- bar 4
  , a4 # n16, b4 # n16, b4 # n16, a4 # n16, a4 # n8, rest du8
  ]

bars5_8 :: [Element]
bars5_8 =   
  [ c5 # n8, b4 # n16, a4 # n16, b4 # n8, a4 # n16, gis4 # n16
  -- bar 6
  , fis4 # n16, e4 # n16, fis4 # n16, gis4 # n16, a4 # n8, b4 # n8
  -- bar7
  , cis5 # n8, b4 # n16, a4 # n16, b4 # n8, a4 # n16, gis4 # n16
  -- bar 8
  , fis4 # n16, e4 # n16, fis4 # n8, rest du8
  ]

n16 :: Pitch -> Element
n16 p = note p sixteenth

n8 :: Pitch -> Element
n8  p = note p eighth



    