
-- ghci ...
-- :set -Wall
-- :set -i../../HNotate



module Nplet where

import HNotate
import HNotate.NoteListDatatypes
import HNotate.Duration
import HNotate.DocLilyPond


import Data.Sequence


notes :: EventList
notes = 
    root # note c4 du4 # nplet 2 eighth (fromList [d4,e4,f4])
         # note c4 du4 # note d4 du8
         # note c4 du8 

tuplet_sys :: System
tuplet_sys = system1 "tuplet" notes   
  
ly_template, ly_output :: FilePath
ly_template   = "templates/ly0-tuplet.ly"
ly_output     = "out/ly-tuplet.ly" 

abc_template, abc_output :: FilePath
abc_template  = "templates/abc0-tuplet.abc"
abc_output    = "out/abc-tuplet.abc"  


main :: IO ()
main = do 
    outputLilyPond DebugOn tuplet_sys   ly_template   ly_output
    outputAbc      DebugOn tuplet_sys   abc_template  abc_output




 

