
\version "2.10.33"
\header {
  
  title = "Bulgarian 6"
}

melody = \relative c' {
  \key a \major
  \time 2/4 
  \clef treble 
   a'16 [b cis cis] cis [a cis cis] |
   cis16 [a b cis] b [a a] r |   
   e'16 [d cis b] cis [a b cis] |
   a16 [b b a] a8 r \bar "||"
}      

\book {
  \score {
    \melody
  }
}
