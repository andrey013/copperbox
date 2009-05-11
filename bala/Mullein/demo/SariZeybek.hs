
-- This tune is `Sari Zybek` from the Tunebook ABC songbook

-- ghci ...
-- :set -i../src

module SariZeybek where

import Mullein.Bracket
import Mullein.Core
import Mullein.Duration
import Mullein.NamedElements
import Mullein.Pitch
import Mullein.Score
import Mullein.Utils


import Data.Ratio


abcTune _ = undefined



-- score = keyChange d_minor |>> repeated bars1_3 |>> repeated bars4_6


nineEightTime :: MetricalSpec
nineEightTime = metricalSpec 9 8



notes1_3 :: [Element]
notes1_3 = 
  [ d 4 dqn [], a 4 en [], a 4 en [], g 4 en [], f 4 qn [], e 4 en []
  -- bar 2
  , f 4 qn [], g 4 qn [], a 4 en [], g 4 en [], f 4 en [], e 4 en [], d 4 en []
  -- bar 3      
  , e 4 qn [], f 4 en [], e 4 en [], d 4 qn [], d 4 dqn []
  ]
                     



notes4_6 :: [Element]
notes4_6 =   
  [ d 4 dqn [], f 4 en [], e 4 en [], d 4 en [], c 4 qn [], b 3 en []
  -- bar 5
  , c 4 qn [], e 4 qn [], e 4 en [], g 4 en [], f 4 en [], e 4 en [], d 4 en []
  -- bar 6
  , e 4 qn [], f 4 en [], e 4 en [], d 4 qn [], d 4 dqn []
  ]          
    