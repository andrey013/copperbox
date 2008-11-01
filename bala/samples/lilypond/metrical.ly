%{ Displaying barred notes - metrical grouping %}

\version "2.10.3"

\header {
  tagline = "- spt -"
}

\book {
  \markup { \bold { Metrical grouping / beams } }
  \markup \line { }

    \markup { 2/8 simple duple }
    \score {   
      \new Staff \relative c' {
        \key c \major
        \clef treble
      	\time 2/8
      	c4
      	c8 c8 
      	c16 c16 c16 c16 
        c32 c32 c32 c32 c32 c32 c32 c32
      }
     
    } %{ end score %}
    
    
    \markup { 2/4 simple duple }
    \score {   
      \new Staff \relative c' {
        \key c \major
        \clef treble
      	\time 2/4
      	c2 
        c4 c4 
        c8 c8 c8 c8 
        c16 c16 c16 c16 c16 c16 c16 c16
      }
     
    } %{ end score %}
    
    \markup { 2/2 simple duple }
    \score {   
      \new Staff \relative c' {
        \key c \major
        \clef treble
      	\time 2/2
      	c1
      	c2 c2 
        c4 c4 c4 c4
        c8 c8 c8 c8 c8 c8 c8 c8
        c16 c16 c16 c16 c16 c16 c16 c16 c16 c16 c16 c16 c16 c16 c16 c16
      }
     
    } %{ end score %}
    
    
	\markup { 3/8 simple triple }
    \score {   
      \new Staff \relative c' {
        \key c \major
        \clef treble
      	\time 3/8
      	c4.
        c8 c8 c8
        c16 c16 c16 c16 c16 c16 
        c32 c32 c32 c32 c32 c32 c32 c32 c32 c32 c32 c32
      }
     
    } %{ end score %}  
    
	\markup { 3/4 simple triple }
    \score {   
      \new Staff \relative c' {
        \key c \major
        \clef treble
      	\time 3/4
      	c2.
        c4 c4 c4
        c8 c8 c8 c8 c8 c8 
        c16 c16 c16 c16 c16 c16 c16 c16 c16 c16 c16 c16
      }
     
    } %{ end score %}

	\markup { 3/2 simple triple }
    \score {   
      \new Staff \relative c' {
        \key c \major
        \clef treble
      	\time 3/2
      	c1.
        c2 c2 c2
        c8 c8 c8 c8 c8 c8 c8 c8 c8 c8 c8 c8 
        c16 c16 c16 c16 c16 c16 c16 c16 c16 c16 c16 c16 
        	c16 c16 c16 c16 c16 c16 c16 c16 c16 c16 c16 c16
      }
     
    } %{ end score %}
    
    
  	\markup { 4/4 simple quadruple }
    \score {
      \new Staff \relative c' {
        \key c \major
        \clef treble
      	\time 4/4
      	c1
      	c2 c2
        c4 c4 c4 c4 
        c8 c8 c8 c8 c8 c8 c8 c8
        c16 c16 c16 c16 c16 c16 c16 c16 c16 c16 c16 c16 c16 c16 c16 c16
      }
    }  %{ end score %}  


  	\markup { 4/2 simple quadruple }
    \score {
      \new Staff \relative c' {
        \key c \major
        \clef treble
      	\time 4/2
      	c\breve
      	c1 c1
        c8 c8 c8 c8 c8 c8 c8 c8 c8 c8 c8 c8 c8 c8 c8 c8
        c16 c16 c16 c16 c16 c16 c16 c16 c16 c16 c16 c16 c16 c16 c16 c16 
        	c16 c16 c16 c16 c16 c16 c16 c16 c16 c16 c16 c16 c16 c16 c16 c16
      }
    }  %{ end score %}  
    
        
} %{ end book %}
  