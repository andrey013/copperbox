%{ How EventTrees should be rendered in LilyPond %}

\version "2.10.3"

\header {
  tagline = "- spt -"
}

\book {
  \markup { \bold { Trees } }
  \markup \line { }
  
  \markup { All these examples must be renderable as an EventTree or [EventTree]. }
  \markup \line { } 
  
  \markup { 1. Simple successors }
  
  \score {
    \new Staff \relative c' {
        \key c \major
        \clef treble
      
        c c d e
    }
  }
  
  \markup { 2. Simple parallel - a chord }
  \score {
    \new Staff \relative c' {
        \key c \major
        \clef treble
      
        <c e g>1
    }
  }
  
  \markup { 3. Successors & grace }
  \score {
    \new Staff \relative c' {
        \key c \major
        \clef treble
      
        c4 \grace { a'16 [b16] } c,4 d4 e4
    }
  }
  
  \markup { 4. Successors & parallel (chord) }
  \score {
    \new Staff \relative c' {
        \key c \major
        \clef treble
      
        c4 <c e g> d e
    }
  }
  
  \markup { 5. Independent parallelism - two staves [EventTree, EventTree] }
  \score {
    <<
      \new Staff \relative c' {
          \key c \major
          \clef treble
      
          c4 c d e
      }
      
      \new Staff \relative c {
          \key c \major
          \clef bass
      
          c2 d2
      }
    >>    
  }
  
  \markup { 6. Independent parallelism - plus chord, grace  [EventTree, EventTree] }
  \score {
    <<
      \new Staff \relative c' {
          \key c \major
          \clef treble
      
          c4 \grace { a'16 [b16] } <c, e g>4 d e
      }
      
      \new Staff \relative c {
          \key c \major
          \clef bass
      
          c2 d2
      }
    >>    
  } 
  
  \markup { 7. Same 'channel' parallelism }
  \score {
    \new Staff \relative c'' {
      \key c \major
      \clef treble

       <<
        { c4 d e c }
        \\
        { g2 e }
      >>
    }
  }      
}   %{ end book %}
