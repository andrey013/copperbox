\version "2.10.33"

\header {
  title = "Te Laudamus Domine"
}

teLaudamusDomine = \relative c' {
  \key c \major
  \clef bass
  \cadenzaOn
  %{# output: \relative te_laudamus_domine #%}
}

\book {
  \score {
    \teLaudamusDomine
  }
}
