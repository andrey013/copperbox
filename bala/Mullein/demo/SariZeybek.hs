
-- This tune is `Sari Zybek` from the Tunebook ABC songbook

-- ghci ...
-- :set -i../src

module SariZeybek where


import Mullein.Core
import Mullein.Duration
import Mullein.NamedElements
import Mullein.Section
import Mullein.Utils


import Data.Ratio


abcTune _ = undefined

{-


-- demo = runAbc sari_zeybek_abc

-- a major

sari_zeybek_abc :: AbcOutput
sari_zeybek_abc = tune $  tune_number 1 
              <$> title "Sari Zeybek"
              <$> origin "Turkey"
              <$> key d_minor 
              <$> meter 9 8 
              <$> sari_zeybek

generateAbc :: AbcOutput -> String
generateAbc doc = pprender $ printf doc

runAbc = putStr . generateAbc

-}

score = keyChange d_minor |>> repeated bars1_3 |>> repeated bars4_6


nineEightTime :: MetricalSpec
nineEightTime = metricalSpec 9 8

bars1_3 :: Section Element
bars1_3 = section nineEightTime notes1_3


notes1_3 :: NoteList
notes1_3 = 
    root  # note d4 (dot du4) # note a4 du8 # note a4 du8 # note g4 du8 
              # note f4 du4 # note e4 du8
          -- bar 2
          # note f4 du4 # note g4 du4 
              # note a4 du8 # note g4 du8 # note f4 du8 # note e4 du8 
              # note d4 du8
          -- bar 3      
          # note e4 du4 # note f4 du8 # note e4 du8
              # note d4 du4 # note d4 (dot du4)
                     

bars4_6 :: Section Element
bars4_6 = section nineEightTime notes4_6 

notes4_6 :: NoteList
notes4_6 = 
    root  # note d4 (dot du4) # note f4 du8 # note e4 du8 # note d4 du8
              # note c4 du4  # note b3 du8
          -- bar 5
          # note c4 du4 # note e4 du4      
              # note e4 du8 # note g4 du8 # note f4 du8 # note e4 du8 
              # note d4 du8 
          -- bar 6
          # note e4 du4 # note f4 du8 # note e4 du8
              # note d4 du4 # note d4 (dot du4)
          
    