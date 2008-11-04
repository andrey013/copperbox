\version "2.10.33"

\header {
  title = "Drums!"
}



drumsOne = \drummode { 
  \time 4/4
  
  %{# output: drums1 #%}
}


drumsTwo = \drummode { 
  \time 4/4
  
  %{# output: drums2 #%}
  
}


\book {
  \score {
    \new DrumStaff <<
      \new DrumVoice { \voiceOne \drumsOne }
    >>  
  }
  \score {
    \new DrumStaff <<
      \new DrumVoice { \voiceOne \drumsTwo }
    >>    
  }  
}