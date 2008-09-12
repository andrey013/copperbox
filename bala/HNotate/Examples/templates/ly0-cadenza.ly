\version "2.10.33"

\header {
  title = "Cadenza"
}

melody = \relative c' {
	\key c \major
	\time 4/4
	\clef treble
	%{# intro:relative #%}
	\cadenzaOn
	%{# cadenza1:relative #%}
	\cadenzaOff
	\barNumberCheck #3
	c1
}	

\book {
  \score {
    \melody
  }
}