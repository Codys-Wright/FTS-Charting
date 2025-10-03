\version "2.24.0"

\score {
  <<
    \new ChordNames {
      \chordmode {
        c1 f1 g1 c1
      }
    }
    \new Staff {
      \clef treble
      \time 4/4
      \key c \major
      
      <c e g>4 <f a c'>4 <g b d'>4 <c e g>4 |
      <c e g>4 <f a c'>4 <g b d'>4 <c e g>4 |
    }
  >>
  \layout {}
  \midi {}
}