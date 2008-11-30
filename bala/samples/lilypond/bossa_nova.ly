\version "2.10.33"

\header {
  title ="Bossa Nova"
}

  
bossaNova = \drummode {
              \time 2/4
              \bar "|:"
              <cymr ss bd>16 cymr cymr <cymr ss bd>
              <cymr bd> cymr <cymr ss> <cymr bd> |
              
              <cymr bd>16 cymr <cymr ss> <cymr bd>
              <cymr bd> <cymr ss> cymr   <cymr bd> |
              \bar ":|"

}

\book {
  \score {
    \new DrumStaff <<
      \set DrumStaff.drumStyleTable = #drums-style
      \new DrumVoice { \voiceOne \bossaNova }

    >>
  }
}
