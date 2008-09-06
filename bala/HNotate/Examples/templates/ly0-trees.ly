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
      
        %{# example1:relative #%} 
    }
  }
  
  \markup { 2. Simple parallel - a chord }
  \score {
    \new Staff \relative c' {
        \key c \major
        \time 4/4
        \clef treble
      
        %{# example2:relative #%} 
    }
  }
  
  \markup { 3. Successors & grace }
  \score {
    \new Staff \relative c' {
        \key c \major
        \time 4/4
        \clef treble
      
        %{# example3:relative #%} 
    }
  }
  
  \markup { 4. Successors & parallel (chord) }
  \score {
    \new Staff \relative c' {
        \key c \major
        \time 4/4
        \clef treble
      
        %{# example4:relative #%} 
    }
  }
  
  \markup { 5. Independent parallelism - just use two note lists }
  \score {
    <<
      \new Staff \relative c' {
          \key c \major
          \time 4/4
          \clef treble
      
          %{# example5a:relative #%} 
      }
      
      \new Staff \relative c {
          \key c \major
          \time 4/4
          \clef bass
      
          %{# example5b:relative #%}
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
      
          %{# example6a:relative #%}
      }
      
      \new Staff \relative c {
          \key c \major
          \time 4/4
          \clef bass
      
          %{# example6b:relative #%}
      }
    >>    
  } 
  
  \markup { 7. Same 'channel' parallelism }
  \score {
    \new Staff \relative c'' {
      \key c \major
      \time 4/4
      \clef treble

       %{# example7:relative #%}
    }
  }      
}   %{ end book %}
