
\version "2.10.3"


{ 
  \relative c' {
    \key c \major 
    \clef treble

    %{ Can we dot a \longa or a \breve ? %}
    
    %{ No we get a parse error at e. %}
    c d\breve. e\longa..
    
    
  }
    
}