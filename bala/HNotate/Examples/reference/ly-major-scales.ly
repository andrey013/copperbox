\version "2.10.33"


\header {
  title = "Major scales"
  tagline = " - "
}

cmajor = \relative c' {
  \clef treble
  \key c \major
  \time 4/4
  
  c4 d e f |
  g4 a b c |
  d4 e f g |
  a4 b c2 \bar "||"
} 

gmajor = \relative c' {
  \clef treble
  \key g \major
  \time 4/4
  
  g4 a b c |
  d4 e fis g |
  a4 b c d |
  e4 fis g2 \bar "||"
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
