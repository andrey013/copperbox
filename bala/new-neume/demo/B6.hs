{-# OPTIONS -Wall #-}

module B6 where

import Neume.Core.Duration
import Neume.Core.NamedElements
import Neume.Core.Pitch
import Neume.Core.LilyPondMonad

import Control.Category
import System.Cmd

main = do
    writeScore "bulgarian6.ly" ly_score
    system     "lilypond bulgarian6.ly"
  


notelist = relative middle_c $ 
        key  a_nat "major"
    $+$ time (2,4)     
    $+$ clef "treble"
    $+$ repeat_volta 2 (bar1 $+$ bar2 $+$ bar3 $+$ bar4)

ly_score :: Doc 
ly_score = execLyScore $ 
   version "2.12.2" $+$ score notelist
   
bar1 :: LyNoteListM ()
bar1 = beam (map sn [ a_ 4, b_ 4, cs_ 5, cs_ 5 ])
   <+> beam (map sn [ cs_ 5, a_ 4, cs_ 5, cs_ 5 ])

bar2 :: LyNoteListM ()
bar2 = beam (map sn [ cs_ 5, a_ 4, b_ 4, cs_ 5 ])
   <+> beam (map sn [ b_  4, a_ 4, a_ 4])
   <+> snr


bar3 :: LyNoteListM ()
bar3 = beam (map sn [ e_ 5, d_ 5, cs_ 5, b_ 4 ])
   <+> beam (map sn [ cs_ 5, a_ 4, b_ 4, cs_ 5 ])

bar4 :: LyNoteListM ()
bar4 = beam (map sn [ a_ 4, b_ 4, b_ 4, a_ 4 ])
   <+> en (a_ 4) <+> enr