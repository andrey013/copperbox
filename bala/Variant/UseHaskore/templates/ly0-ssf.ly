\version "2.10.33"

\header {
  title = "Stars and Stripes Forever"
}


melody = \relative c'' {
  \key c \major
  \time 4/4 
  \clef treble 
   %{# output: \relative Flute #%}
}      

\book {
  \score {
    \melody
  }
}
