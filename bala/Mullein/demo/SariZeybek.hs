
-- This tune is `Sari Zybek` from the Tunebook ABC songbook

-- ghci ...
-- :set -i../src

module SariZeybek where

import Mullein.Bracket
import Mullein.Core
import Mullein.CoreTypes
import Mullein.Duration hiding (dot)
import Mullein.NamedElements
import Mullein.Pitch
import Mullein.Score
import Mullein.Utils


import Data.Ratio


abcTune _ = undefined



-- score = keyChange d_minor |>> repeated bars1_3 |>> repeated bars4_6


nineEightTime :: MetricalSpec
nineEightTime = metricalSpec 9 8



notes1_3 :: NoteCtx [Element]
notes1_3 = notelist $
    [ d4 & du4 & dot,  a4 & du8, a4, g4, f4 & du4, e4 & du8
    -- bar 2
    , f4 & du4, g4, a4 & du8, g4, f4, e4, d4
    -- bar 3      
    , e4 & du4, f4 & du8, e4, d4 & du4, d4 & du4 & dot
    ]
                     



notes4_6 :: NoteCtx [Element]
notes4_6 = notelist $  
    [ d4 & du4 & dot, f4 & du8, e4, d4, c4 & du4, b3 & du8
    -- bar 5
    , c4 & du4, e4, e4 & du8, g4, f4, e4, d4 
    -- bar 6
    , e4 & du4, f4 & du8, e4, d4 & du4, d4 & du4 & dot
    ]          
    