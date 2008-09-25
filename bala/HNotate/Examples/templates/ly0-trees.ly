%{ How note lists should be rendered in LilyPond %}

\version "2.10.3"

\header {
  tagline = "- spt -"
}

\book {
  \markup { \bold { Trees } }
  \markup \line { }
  
  \markup { All these examples must be renderable with the System/NoteList
            datatypes. 
  }
  \markup \line { } 
  
  \markup { 1. Simple successors }
  
  \score {
    \new Staff \relative c' {
        \key c \major
        \time 4/4
        \clef treble
      
        %{# relative:example1 #%} 
    }
  }
  
  \markup { 2. Simple parallel - a chord }
  \score {
    \new Staff \relative c' {
        \key c \major
        \time 4/4
        \clef treble
      
        %{# relative:example2 #%} 
    }
  }
  
  \markup { 3. Successors & grace }
  \score {
    \new Staff \relative c' {
        \key c \major
        \time 4/4
        \clef treble
      
        %{# relative:example3 #%} 
    }
  }
  
  \markup { 4. Successors & parallel (chord) }
  \score {
    \new Staff \relative c' {
        \key c \major
        \time 4/4
        \clef treble
      
        %{# relative:example4 #%} 
    }
  }
  
  \markup { 5. Independent parallelism - just use two note lists }
  \score {
    <<
      \new Staff \relative c' {
          \key c \major
          \time 4/4
          \clef treble
      
          %{# relative:example5a #%} 
      }
      
      \new Staff \relative c {
          \key c \major
          \time 4/4
          \clef bass
      
          %{# relative:example5b #%}
      }
    >>    
  }
  
  \markup { 6. Independent parallelism - a complex note list and a simple one }
  \score {
    <<
      \new Staff \relative c' {
          \key c \major
          \time 4/4
          \clef treble
      
          %{# relative:example6a #%}
      }
      
      \new Staff \relative c {
          \key c \major
          \time 4/4
          \clef bass
      
          %{# relative:example6b #%}
      }
    >>    
  } 
  
  \markup { 7. Same 'channel' parallelism }
  \score {
    \new Staff \relative c'' {
      \key c \major
      \time 4/4
      \clef treble

       %{# relative:example7 #%}
    }
  }      
}   %{ end book %}
