\version "2.10.3"

\header {
  title = \markup \typewriter "Polyphony"
  }
  
\book {
  
  \markup { 
    \typewriter "example7a :: Performance NrEvent                             " }
  \markup {
    \typewriter "example7a = perf1 $                                          " }
  \markup { 
    \typewriter "  root  # event (c4 # du2)                                   " }
  \markup {
    \typewriter "        # poly [ root # event (c5 # du4) # event (c5 # du4)  " }
  \markup { 
    \typewriter "                      # event (d5 # du4) # event (e5 # du4)  " }
  \markup { 
    \typewriter "               , root # event (g4 # du2) # event (e4 # du2)  " }
  \markup { 
    \typewriter "               ]                                             " }
  \markup { 
    \typewriter "        # event (c4 # du2)                                   " }
  \markup { 
    \typewriter "        # event (c4 # du2)                                   " } 
  
  \markup { \typewriter "----------" }
  
  \markup { 
    \typewriter ":part 1 " }
    
  \markup { 
    \typewriter "|:1 [1-2] C4/1%2 C4/1%2 " }
    
  \markup { 
    \typewriter "|:2 [] C4/1%2 S/1%2 " }
    
  \markup { 
    \typewriter "  #1 |:1 [] S/1%2 C5/1%4 C5/1%4 |:2 [] D5/1%4 E5/1%4 S/1%2 " }
    
  \markup { 
    \typewriter "  #2 |:1 [] S/1%2 G4/1%2 |:2 [] E4/1%2 S/1%2 "}
    

  \markup { \typewriter "----------" }
               
  \markup \typewriter 
    { Vanilla transliteration to LilyPond, note the c4 half notes aren't
      in series: } 
      
  \score {
    \new Voice {
      \relative c' { 
        \key c \major
        \clef treble
       
        
        c2 << { c'4 c d e } \\ { g,2 e2 } >> c c 
        
        }
      }
    }  %{ end score %}
    
  
  \markup \typewriter
    { Making the primary line part of the poly group, the c4 half notes are 
      now in series... }     
      
  \score {
    \new Voice {
      \relative c' { 
        \key c \major
        \clef treble
        
        << { c2 c c } \\ { s2 c'4 c d e } \\ { s2 g, e } >> 
        
        }
      }
    }  %{ end score %}
    
  \markup \typewriter
    { The second choice is most attractive. But how far do we lookahead in the primary line? } 
  

            
  \markup \typewriter
    { Also, the second choice is quite like Abc, which would divide like this... }     
    
  \score {
    \new Voice {
      \relative c' { 
        \key c \major
        \clef treble
        
        << { c2 c } \\ { s2 c'4 c }  \\ { s2 g2 } >> 
        << { c,2 s2 } \\ { d'4 e s2 } \\ { e,2 s2 } >>
        
        }
      }
    }  %{ end score %}
    

  
              
                          
  } %{ end book %}  
        
          