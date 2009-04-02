
-- This tune is `Bulgarian (?) 6` from the Exotic ABC songbook

-- ghci ...
-- :set -i../src

module Bulgarian6 where

import HNotate.Data
import HNotate.DocAbc
import HNotate.DocBase
import HNotate.Duration
import HNotate.EvalAbc
import HNotate.MusicRepDatatypes
import HNotate.NoteList
import HNotate.Pitch
import HNotate.Utils

import qualified Text.PrettyPrint.Leijen as PP


    

-- a major

bulgarian6 :: AbcOutput
bulgarian6 = tune $  tune_number 1 
                 <$> title "Bulgarian (?) 6"
                 <$> composer "Unknown"
                 <$> key a_major 
                 <$> meter 2 4 
                 <$> current (stdInterp bars1_4)

generateAbc :: AbcOutput -> String
generateAbc doc = pprender $ printf doc

runAbc = putStr . generateAbc

demo1 = runAbc bulgarian6

bars1_4 :: NoteList
bars1_4 = 
    root # note a4 du16   # note b4 du16 # note cis5 du16 # note cis5 du16 
         # note cis5 du16 # note a4 du16 # note cis5 du16 # note cis5 du16
        
         # note cis5 du16 # note a4 du16 # note b4 du16   # note cis5 du16
         # note b4 du16   # note a4 du16 # note a4 du16   # rest du16
        
         # note e5 du16   # note d5 du16 # note cis5 du16 # note b4 du16
         # note cis5 du16 # note a4 du16 # note b4 du16   # note cis5 du16
        
         # note a4 du16   # note b4 du16 # note b4 du16   # note a4 du16
         # note a4 du8    # rest du8       

        
 


    