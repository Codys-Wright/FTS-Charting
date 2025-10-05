\version "2.24.0"

\include "english.ly"
\include "utils/fonts.ly"
\include "utils/chord-display.ly"
\include "utils/key-changes.ly"
\include "utils/note-placement.ly"
\include "utils/paper-setup.ly"
\include "utils/capsules/measure-position-detection.ly"
\include "utils/capsules/capsule-utils.ly"
\include "utils/capsules/new-capsule-utils.ly"
\include "utils/rehearsal-marks/rehearsal-mark-positioning.ly"
\include "utils/breaks/auto-four-measure-breaks.ly"
\include "utils/breaks/pseudo-indents.ly"
\include "utils/breaks/auto-pseudo-indents.ly"
\include "utils/layout/spacing.ly"
\include "utils/layout/score-layout.ly"
\include "utils/custom-rhythmic-notation.ly"
\include "utils/header-template.ly"

\header {
  title = "Chart Title"
  composer = "Composer Name"
  % Add any other header fields you need
}

\score {
  <<
  \new ChordNames = "chordProgression" {
    \set chordChanges = ##t
    \set chordNameExceptions = #chordExceptions
    \set chordRootNamer = #musejazz-chord-name->markup
    \override ChordName.font-size = #3
    \override ChordName.font-name = #"MuseJazz Text"
    \override VerticalAxisGroup.nonstaff-relatedstaff-spacing.padding = #0.9
    
    % Add your chord progression here
    \chordmode {
      c1:maj7 | d:m7 | e:m7 | f:maj7 |
      g:7 | a:m7 | b:m7.5- | c:maj7 |
    }
  }

  \new Staff \with {
    \consists #key-change-tracker-engraver
  } {
    \clef treble
    \time 4/4
    \key c \major
    \set Staff.printKeyCancellation = ##f
    \override Score.Script.font-size = #2
    \override Score.Script.color = #red
    
    \autoInlineBreaks {
        \rh{

      \mark "CH 1"
      s1*6
      \mark "VS 2"
      s1*7
      \mark "CH 2"
      s1*5
      \mark "VS 3"
      s1*4
      \mark "CH 3"
        }
    }
  }
  >>
  
  \layout {
    \context {
      \Score
      \ekmSmuflOn #'all
    }
  }
}
