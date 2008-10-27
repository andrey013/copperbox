

module Examples where

import HNotate


-- starts with an anacrusis
adeste_fidelis :: System
adeste_fidelis = system1 "adeste_fidelis" $ 
    root  # chord [e4,    a4] du4
    
          # chord [e4,    a4] du2
          # note   e4         du4 
          # chord [cis4,  a4] du4 
       
          # chord [e4,    b4] du2 
          # note   e4         du2 

          # chord [e4,  cis5] du4 
          # chord [e4,    b4] du4 
          # chord [e4,  cis5] du4 
          # chord [fis4,  d5] du4 

          # chord [e4,  cis5] du2 
          # chord [e4,    b4] du4 
          # chord [cis4,  a4] du4
 

-- unmetered plain song
te_laudamus_domine :: System
te_laudamus_domine = system1 "te_laudamus_domine" $
    root  # note d4 du4   # note d4 du4 {- inclinatum -}  
                          # note d4 du4 {- inclinatum -}
          # note b3 du4
          # note c4 du4
          # note d4 du4
          # note d4 du4
          # note e4 du4
          # note d4 du4
          # note c4 du4
          # note c4 du4
          # note c4 du4   # note d4 du4 {- pes -}
          # note d4 du4   # note c4 du4 {- inclinatum -}
                          # note b3 du4 {- inclinatum -}
                          # note a3 du4 {- inclinatum -} {- augmentum -}
          # note g3 du4   # note a3 du4 {- pes -} 
                          # note g3 du4 {- inclinatum -}
                          # note f3 du4 {- inclinatum -} {- augmentum -}
          # note a3 du4
          # note c4 du4
          # note c4 du4
          # note b3 du4
          # note c4 du4
          # note d4 du4
          # note a3 du4
          # note b3 du4
          # note a3 du4
          # note g3 du4
          # note f3 du4   # note g3 du4 {- pes -}
          # note g3 du4   {- augmentum -}                        
          

-- key signature  Bb Eb F#
hijaz_scale :: System
hijaz_scale = system1 "hijaz" $
    root  # note d3   du4
          # note a3   du8
          # note bes3 du8
          # note c4   du8
          # note d4   du8
          # note e4   du8
          # note fis4 du8
          
          # note g4   du8
          # note a4   du8
          # note bes4 du8
          # note c5   du8
          # note d5   du2
          
          
          
             