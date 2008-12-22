
-- ghci ...
-- :set -Wall
-- :set -i../../HNotate



module StringNumberScale where

import HNotate
import HNotate.NoteListDatatypes
import HNotate.Duration
import HNotate.DocLilyPond
import HNotate.TemplateDatatypes

import HNotate.FPList

cmajor_notes = root # note' c3 quarter (stringNumber 5)
                    # note' d3 quarter (stringNumber 5) 
                    # note' e3 quarter (stringNumber 4) 
                    # note' f3 quarter (stringNumber 4) 
                    # note' g3 quarter (stringNumber 4) 
                    # note' a3 quarter (stringNumber 3) 
                    # note' b3 quarter (stringNumber 3) 
                    # note' c4 quarter (stringNumber 3) 
                    



scales_sys = systemL $ 
    [ ("cmajor", cmajor_notes)
    ]

scales_doc :: LilyPondTemplate    
scales_doc = 
  lilypond 
    [  version
    
    ,  header
     . title "C Major - with string numbering"               
    
    ,  definition "cmajor"
     . expression          
     . key c_nat major 
     . time 4 4
     . outputAbsolute "cmajor"  
    
      
    , book . score . new "TabStaff" . invocation  "cmajor"
    ]
    
output_ly_scales :: IO ()
output_ly_scales = 
  outputLilyPondDocu' 5 scales_sys 
                        evalStringNumAnno 
                        scales_doc 
                        "./out/fingered_scale.ly"

    
main = output_ly_scales

