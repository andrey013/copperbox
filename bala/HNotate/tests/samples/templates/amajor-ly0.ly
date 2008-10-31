\version "2.10.33"

\header {
  title = "A Major (twice)"
}

amajorRel = \relative c' {
  \key a \major
  \time 4/4
  \clef treble
  \partial 4 
  %{# output: \relative amajor_scale #%}
}

amajorAbs = {
  \key a \major
  \time 4/4
  \clef treble
  \partial 4 
  %{# output: amajor_scale #%}
}


\book {
  \score {
  	<<
    	\new Staff { \amajorRel }
  
  		\new Staff { \amajorAbs }
    >>
  }
}
