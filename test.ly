


\include "utils/paper-setup.ly"
\include "utils/measure-position-detection.ly"
\include "utils/capsule-utils.ly"
\include "utils/section-engraver.ly"





% Test the dynamic functions
#(ly:message "Dynamic line break detection functions loaded from utility file")

\layout {
  \context {
    \Score
    \override RehearsalMark.after-line-breaking = #red-first-in-line-callback
  }
}




% ****************************************************************
% ly snippet:
% ****************************************************************
\version "2.24.0"
\header {

}

chs = \transpose c' c' {
 s1*48 
}

marks = {
  \mark \markup "M1"
 s1*4 |
 \mark \markup "M5"
 s1*2 |
 \mark \markup "M7"
 s1*2 |
 \mark \markup "M9"
 s1*3 |
 \mark \markup "M12"
 s1*44
}

breaks = {
  s1*4 |
  \break
  s1*2 
  \break
}

\score {
  <<
  \chords { \chs }
  \new Staff \transpose c c' { 
    <<
      \marks
      \breaks
    >>
  }
  >>
  \layout {
    \context {
      \Score
      \remove Bar_number_engraver
    }
  }
}




% ****************************************************************
% end ly snippet
% ****************************************************************