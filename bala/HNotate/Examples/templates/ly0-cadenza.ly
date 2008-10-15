\version "2.10.33"

\header {
  title = "Cadenza"
}

melody = \relative c' {
	\key c \major
	\time 4/4
	\clef treble
	%{# output: \relative intro #%}
	\cadenzaOn
	%{# output: \relative cadenza1 #%}
	\cadenzaOff
	\barNumberCheck #3
	c1
}

\book {
  \score {
    \melody
  }
}
