
\version "2.10.3"

%{ enumerate dashHat, dashBar... etc from script-init.ly  %}

{ 
  \relative c' {
    \key c \major 
    \clef treble
    
    c\marcato^"long no placing" d\stopped e\tenuto f\staccatissimo g\accent a\staccato b\portato r4
    
    c,-^^"short dash" d-+ e-- f-| g-> a-. b-_ r4
    
    c,_^_"short below" d_+ e_- f_| g_> a_. b__ r4
    
    c,^^^"short above" d^+ e^- f^| g^> a^. b^_ r4
    
    
    c,^\marcato^"long above" d^\stopped e^\tenuto f^\staccatissimo g^\accent a^\staccato b^\portato r4
    
    
  }
}