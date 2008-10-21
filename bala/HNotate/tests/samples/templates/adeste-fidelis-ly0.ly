\version "2.10.33"

\header {
  title = "Adeste Fidelis"
  composer = "John F. Wade"
}

adesteFidelis = \relative c' {
  \key a \major
  \time 4/4
  \clef treble
  \partial 4 %{# output: \relative adeste_fidelis #%}
}

\book {
  \score {
    \adesteFidelis
  }
}
