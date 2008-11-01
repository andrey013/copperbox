
\version "2.10.3"

\drums {
  hihat hh bassdrum bd
}

%{ this doesn't do what you expect %}
\drummode {
  hihat hh bassdrum bd
}


%{ drummode needs to be inside a DrumStaff context %}
\new DrumStaff \drummode {
    hihat hh bassdrum bd
  }
  
  