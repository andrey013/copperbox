\version "2.10.33"

\header {
  title = "Hijaz scale"
}


hijaz = \relative c' {
  \set Staff.keySignature = #`(((0 .  10) . ,SHARP) ((0 . 9) . ,FLAT) ((0 . 6) . ,FLAT))
  \clef treble
  \time 4/4
  %{# output: \relative hijaz #%}
}

\book {
  \score {
    \hijaz
  }
}
