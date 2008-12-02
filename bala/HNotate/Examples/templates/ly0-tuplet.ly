\version "2.10.33"

\header {
  title = "Tuplets"
  tagline = " - "
}

tupletOutput = \relative c' {
  \clef treble
  \key c \major
  \time 4/4

  %{# output: \relative tuplet #%}
}


\book {

  \markup { Tuplet ouput }
  \score {
    \new Staff { \tupletOutput }

  }
}
