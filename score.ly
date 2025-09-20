% Main score content for LilyPond charting workflow
% This file contains the actual music and score definition

\version "2.24.0"
\include "english.ly"

% Include utility files
\include "utils/fonts.ly"
\include "utils/fitbox-utils.ly"
\include "utils/music-definitions.ly"

% Define header variables
#(define songTitle "Song Title")
#(define masterRhythm "Master Rhythm")
#(define transcribedBy "Transcribed By")
#(define transcriberName "Cody Wright")

% Include header template
\include "utils/header-template.ly"

% Include paper setup AFTER header is defined
\include "utils/paper-setup.ly"

\layout {
  % Use default indentation for rehearsal marks
  \context {
    \Staff
    \override InstrumentName.self-alignment-X = #LEFT
    \override StaffSymbol.line-count = #5
    \override StaffSymbol.thickness = #0.05
    \override TimeSignature.style = #'numbered
  }
  \context {
    \ChordNames
    \override ChordName.font-size = #5
    \override ChordName.Y-offset = #3
  }
}

% ==== Music ====
global = { \time 4/4 \key g \major }

oneLineChords = \chordmode { g1 c1 e1:m d1 }
oneLineSlashes = {
  \override NoteHead.style = #'slash
  \hide Stem
  b'4 b'4 b'4 b'4 |
  b'4 b'4 b'4 b'4 |
  b'4 b'4 b'4 b'4 |
  b'4 b'4 b'4 b'4
}

\score {
  <<
    % Chord symbols — mirror the same line breaks
    \new ChordNames {
      \oneLineChords \break
      \oneLineChords \break
      \oneLineChords \break
      \oneLineChords \break
      \oneLineChords \break
      \oneLineChords \break
      \oneLineChords \break
      \oneLineChords \break
      \oneLineChords \break
      \oneLineChords
    }

    % Staff with left-margin labels
    \new Staff \with {
      instrumentName = #(markup #:fitBoxNoScale "INTRO")          % first system - no scaling
      shortInstrumentName = #(markup #:fitBoxNoScale "INTRO")     % default for subsequent systems until changed
    } {
      \global \clef treble

      % Line 1  (uses instrumentName/shortInstrumentName = "INTRO")
      \oneLineSlashes
      \set Staff.shortInstrumentName = #(markup #:fitBox "VS 1")
      \break

      % Line 2
      \oneLineSlashes
      \set Staff.shortInstrumentName = #(markup #:fitBox "CH 1")
      \break

      % Line 3
      \oneLineSlashes
      \set Staff.shortInstrumentName = #(markup #:fitBox "VS 2")
      \break

      % Line 4
      \oneLineSlashes
      \set Staff.shortInstrumentName = #(markup #:fitBox "CH 2")
      \break

      % Line 5
      \oneLineSlashes
      \set Staff.shortInstrumentName = #(markup #:fitBox "BR 1")
      \break

      % Line 6
      \oneLineSlashes
      \set Staff.shortInstrumentName = #(markup #:fitBox "CH 3")
      \break

      % Line 7
      \oneLineSlashes
      \set Staff.shortInstrumentName = #(markup #:fitBox "SOLO\nGTR")
      \override Staff.InstrumentName.extra-offset = #'(0 . -1.5)
      \break

      % Line 8
      \oneLineSlashes
      \revert Staff.InstrumentName.extra-offset
      \set Staff.shortInstrumentName = #(markup #:fitBox "CH 4")
      \break

      % Line 9
      \oneLineSlashes
      \set Staff.shortInstrumentName = #(markup #:fitBox "OUTRO")
      \break

      % Line 10
      \oneLineSlashes
    }
  >>
  \layout { }
}
