\version "2.10.33"

\header {
  title = "Sim1"
}


melody = \relative c'' {
  \key c \major
  \time 4/4 
  \clef treble 
  \cadenzaOn
   %{# output: \relative AcousticBass #%}
  \cadenzaOff 
}      

\book {
  \score {
    \melody
  }
}
