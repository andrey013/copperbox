\version "2.10.33"

\header {
  title = "Tuplets"
}


melody = \relative c {
  \key c \major
  \clef treble
  \time 4/4
   c'4 \times 2/3 { d e f } c  | 
   
   %{ This doesn't do what I'd like ... %}
   c8 \times 2/3 { d e f } c8 d4 d4 | 
   
   %{ This is better the first note in the tuplet has a duration %}
   %{ Note that the c after the tuplet is an eighth - the tuplet 
      duration annotation changes the 'global' relative duration %} 
   c4 \times 2/3 { d8 e f } c d8 c4 | 
  

}

\book {
  \score {
    \melody
  }
}
