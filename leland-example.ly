\version "2.24.0"

ekmFont = "Bravura"

\include "/home/cody/Documents/Development/reference/esmuflily/ly/cosmufl.ily"

music = \relative c' {
  \clef treble
  \time 4/4
  c4 d e f |
  g a b c |
  d e f g |
  a b c d |
}

slashMusic = \relative c' {
  \clef treble
  \time 4/4
  \hide Stem
  \override NoteHead.style = #'slash
  b4 b4 b4 b4 |
  b4 b4 b4 b4 |
}

  \layout {
    \context {
      \Score
      % Enable SMuFL for most elements but exclude noteheads
      \ekmSmuflOn #'(clef time-signature bar-line accidental articulation notehead)
    }
  }

\score {
  \new Staff \music
  \layout {}
  \midi {}
}

\score {
  \new Staff \slashMusic
  \layout {}
  \midi {}
}

\markup \column {
  \line { "Leland Font Characters:" }
  \vspace #1




  \line { "Slash with Vertical Ends:" }
  \ekm-chars #'(#xE100) 
  \vspace #0.5

}
