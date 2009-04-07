
-- This tune is `Sari Zybek` from the Tunebook ABC songbook

-- ghci ...
-- :set -i../src

module SariZeybek where

import HNotate.Data
import HNotate.DocAbc
import HNotate.DocBase
import HNotate.Duration
import HNotate.EvalAbc
import HNotate.MusicRepDatatypes
import HNotate.NamedElements
import HNotate.NoteList
import HNotate.Pitch
import HNotate.StructuralDatatypes
import HNotate.Utils

demo = runAbc sari_zeybek_abc

-- a major

sari_zeybek_abc :: AbcOutput
sari_zeybek_abc = tune $  tune_number 1 
              <$> title "Sari Zeybek"
              <$> origin "Turkey"
              <$> key d_minor 
              <$> meter 9 8 
              <$> current (stdInterp bars1_3)

generateAbc :: AbcOutput -> String
generateAbc doc = pprender $ printf doc

runAbc = putStr . generateAbc

-- sari_zeybek :: Compo Element -- This would realize the full tune, with correct repeat marks
sari_zeybek = repeated (lit bars1_3) ->- repeated (lit bars4_6)

lit x = Literal $ \env -> stdInterp x env


bars1_3 :: NoteList
bars1_3 = 
    root  # note d4 (dot du4) # note a4 du8 # note a4 du8 # note g4 du8 
              # note f4 du4 # note e4 du8
          -- bar 2
          # note f4 du4 # note g4 du4 
              # note a4 du8 # note g4 du8 # note f4 du8 # note e4 du8 
              # note d4 du8
          -- bar 3      
          # note e4 du4 # note f4 du8 # note e4 du8
              # note d4 du4 # note d4 (dot du4)
                     

bars4_6 :: NoteList
bars4_6 = 
    root  # note d4 (dot du4) # note f4 du8 # note e4 du8 # note d4 du8
              # note c4 du4  # note b3 du8
          -- bar 5
          # note c4 du4 # note e4 du4      
              # note e4 du8 # note g4 du8 # note f4 du8 # note e4 du8 
              # note d4 du8 
          -- bar 6
          # note e4 du4 # note f4 du8 # note e4 du8
              # note d4 du4 # note d4 (dot du4)
          
    