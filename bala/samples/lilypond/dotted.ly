


\version "2.10.3"


{ 
  \relative c' {
    \key c \major 
    \clef treble

    %{ Can we dot a note with no duration (i.e. default duration) ? %}
    
    %{ No we get a parse error at e. %}
    c4. e. f2. g4.
    
    
  }
    
}


