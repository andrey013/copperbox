\version "2.10.33"

\header {
  title ="Cha Cha"
}

chaCha = \drummode {
           \time 2/4
           <ssh cb>16 ssl16 <ssh cb>16 <ssh timl>16 <ssh cb>16 ssl16 <ssh cb
           timl>16 ssh16 |
         }

\book {
  \score {
    \new DrumStaff \with {
          drumStyleTable = #timbales-style
          \override StaffSymbol #'line-count = #2
        } <<
          \set Staff.instrumentName = #"timbales"
          \chaCha
        >>
  }
}
