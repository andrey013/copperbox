\version "2.10.33"

\header {
  title = "Major scales"
  tagline = " - "
}

cmajor = \relative c' {
  \clef treble
  \key c \major
  \time 4/4

  %{# output: \relative cmajor #%}
}

gmajor = \relative c' {
  \clef treble
  \key g \major
  \time 4/4

  %{# output: \relative gmajor #%}
}


\book {

  \markup { C major }
  \score {
    \new Staff { \cmajor }

    \layout { }
    \midi {
      \context {
        \Score
          tempoWholesPerMinute = #(ly:make-moment 110 4)
      }
    }
  } %{ end - c major %}

  \markup { G major }
  \score {
    \new Staff { \gmajor }

    \layout { }
    \midi {
      \context {
        \Score
          tempoWholesPerMinute = #(ly:make-moment 110 4)
      }
    }
  } %{ end - g major %}

}
