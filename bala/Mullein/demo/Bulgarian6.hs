
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
import Mullein.Score
import Mullein.Utils

import qualified Mullein.LilyPondConvert as Ly
import qualified Mullein.LilyPondOutput as Ly

import Data.Ratio
import Text.PrettyPrint.Leijen ( putDoc )


main = putDoc $ Abc.outputAbc a_major (fst twoFourTime) (repeat 4) bulgarian6

ly = putDoc $ Ly.outputLy a_major (fst twoFourTime) bulg6 where
  bulg6 = Ly.convertToLy c4 part1_8


bulgarian6 :: Part
bulgarian6 = convertToAbc lset sn part1_8 where
  lset = maybe (error "lset missing") id $ makeLabelSet a_major


amMotif = motif a_major twoFourTime

twoFourTime :: MetricalSpec
twoFourTime = metricalSpec 2 4


part1_8 :: Part
part1_8 = part [repeated m1_4, repeated m5_8]



m1_4 :: Motif
m1_4 = amMotif $ primary bars1_4

m5_8 ::  Motif
m5_8 = amMotif $ primary bars5_8



bars1_4 :: [Element]
bars1_4 =  
  [ a 4 sn, b 4 sn, cs 5 sn, cs 5 sn, cs 5 sn, a 4 sn, cs 5 sn, cs 5 sn
  -- bar 2
  , cs 5 sn, a 4 sn, b 4 sn, cs 5 sn, b 4 sn, a 4 sn, a 4 sn, snr
  -- bar 3
  , e 5 sn, d 5 sn, cs 5 sn, b 4 sn, cs 5 sn, a 4 sn, b 4 sn, cs 5 sn
  -- bar 4
  , a 4 sn, b 4 sn, b 4 sn, a 4 sn, a 4 en, enr
  ]

bars5_8 :: [Element]
bars5_8 =   
  [ c 5 qn, b 4 sn, a 4 sn, b 4 en, a 4 sn, gs 4 sn
  -- bar 6
  , fs 4 sn, e 4 sn, fs 4 sn, gs 4 sn, a 4 en, b 4 en
  -- bar7
  , cs 5 en, b 4 sn, a 4 sn, b 4 en, a 4 sn, gs 4 sn
  -- bar 8
  , fs 4 sn, e 4 sn, fs 4 en, enr
  ]



    