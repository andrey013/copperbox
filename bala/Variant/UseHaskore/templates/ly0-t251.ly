\version "2.10.33"

\header {
  title = "t251"
}


melody = \relative c' {
  \key c \major
  \time 4/4 
  \clef bass 
   %{# output: \relative AcousticGrandPiano #%}
}      

\book {
  \score {
    \melody
  }
}
