\version "2.24.0"

\include "english.ly"
\include "utils/custom-rhythmic-notation.ly"

\score {
  <<
  \new Staff {
    \clef treble
    \time 4/4
    \key c \major
    \mark ch

    \rh {4 4 4. 8 | s1 | s1 | s1 | s1 | s1}  
    \break
    \mark vs
    \rh {s1*4}
  }
  
  >>
  \layout {
    \context {
      \Score
      \ekmSmuflOn #'all
    }
  }
}
