
\version "2.10.33"
\header {
  
  title = "Bulgarian 6"
}


melody = \relative c' {
  \key a \major
  \time 2/4 
  \clef treble 
   %{# bulgarian6:relative #%}
}      

\book {
  \score {
    \melody
  }
}
