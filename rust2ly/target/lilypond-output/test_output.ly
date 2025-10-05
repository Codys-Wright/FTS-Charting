\version "2.24.0"

% Set the font to Bravura for SMuFL support

\include "english.ly"
\include "../../lilypond-utils/fonts.ly"
\include "../../lilypond-utils/chord-display.ly"
\include "../../lilypond-utils/key-changes.ly"
\include "../../lilypond-utils/note-placement.ly"
\include "../../lilypond-utils/paper-setup.ly"
\include "../../lilypond-utils/capsules/measure-position-detection.ly"
\include "../../lilypond-utils/capsules/capsule-utils.ly"
\include "../../lilypond-utils/capsules/new-capsule-utils.ly"
\include "../../lilypond-utils/rehearsal-marks/rehearsal-mark-positioning.ly"
\include "../../lilypond-utils/breaks/auto-four-measure-breaks.ly"
\include "../../lilypond-utils/breaks/pseudo-indents.ly"
\include "../../lilypond-utils/breaks/auto-pseudo-indents.ly"
\include "../../lilypond-utils/layout/spacing.ly"
\include "../../lilypond-utils/layout/score-layout.ly"
\include "../../lilypond-utils/header-template.ly"
\include "../../lilypond-utils/testing.ly"

% Replicate the marks variable from test.ly
marks = {
 s1*2 |
 \mark \markup "Intro"
 s1*8 |
 \mark \markup "VS 1"
 s1*16 |
 \mark \markup "CH 1"
 s1*8  | 
 \mark \markup "VS 2"
 s1*16 |
 \mark \markup "CH 2"
 s1*8 |
 \mark \markup "KEYS"
 s1*8 |
 \mark \markup "GTR"
 s1*8 |
 \mark \markup "CH 3"
 s1*8 |
 \mark \markup "CH 4"
 s1*8 |
 \mark \markup "Outro"
 s1*6 |
}

% Global settings including key signature
global = { \time 4/4  }

\score {
  \new Staff {
    \clef treble
    \time 4/4
    \key c \major
    
    \marks
  }
  \layout {
    \context {
      \Score
      \override RehearsalMark.self-alignment-X = #LEFT
      \override RehearsalMark.font-size = #2
    }
  }
  \midi {}
}