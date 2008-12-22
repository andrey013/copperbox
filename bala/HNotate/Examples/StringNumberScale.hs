
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

cmajor_notes = root # note' c4 quarter (stringNumber 5)
                    # note' d4 quarter (stringNumber 5) 
                    # note' e4 quarter (stringNumber 4) 
                    # note' f4 quarter (stringNumber 4) 
                    # note' g4 quarter (stringNumber 4) 
                    # note' a4 quarter (stringNumber 3) 
                    # note' b4 quarter (stringNumber 3) 
                    # note' c5 quarter (stringNumber 3) 
                    



scales_sys = systemL' $ 
    [ ("cmajor",     cmajor_notes, noAnnoEval)
    , ("cmajorfret", cmajor_notes, evalStringNumAnno)
    ]

scales_doc :: LilyPondTemplate    
scales_doc = 
  lilypond 
    [  version
    
    ,  header
     . title "C Major - with string numbering"               

    ,  definition "cmajorfret"
     . expression          
     . key c_nat major 
     . time 4 4
     . outputAbsolute "cmajorfret"  
     
    ,  definition "cmajor"          
     . relative c4
     . key c_nat major 
     . time 4 4
     . outputRelative "cmajor"  
         

    
      
    ,  book . score . doubleAngles  
      . new "Staff"    . invocation  "cmajor"
      . new "TabStaff" . invocation  "cmajorfret"
    ]
    
output_ly_scales :: IO ()
output_ly_scales = 
  outputLilyPondDocu  5 scales_sys scales_doc "./out/fingered_scale.ly"

    
main = output_ly_scales

