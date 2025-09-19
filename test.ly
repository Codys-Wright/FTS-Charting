\version "2.24.0"
\include "english.ly"

#(set-global-staff-size 18)

\paper {
  #(set-paper-size "a4")
  top-margin = 12\mm
  bottom-margin = 12\mm
  left-margin = 16\mm
  right-margin = 16\mm
  indent = 5.00\sp
  system-system-spacing = #'(
    (basic-distance . 16)
    (minimum-distance . 12)
    (padding . 6)
    (stretchability . 20))
  ragged-last-bottom = ##f
  
  % Disable default headers and page numbers
  print-page-number = ##f
  print-first-page-number = ##f
  bookTitleMarkup = ##f
  
  % Custom title layout with part type in top left
  scoreTitleMarkup = \markup \fill-line {
    \column {
      \line { \fontsize #1 \bold "MASTER" }
      \line { \fontsize #1 \bold "RHYTHM" }
      \line { \fontsize #0.5 \with-color #grey "V1" }
    }
    \center-column {
      \fontsize #10 \bold \fromproperty #'header:title
      \fontsize #-4 \line { \italic "Transcribed by: " \fromproperty #'header:subtitle }
    }
    \column {
      \line { \fontsize #-1 \fromproperty #'header:composer }
      \line { \fontsize #-1 \fromproperty #'header:arranger }
    }
  }
  
  % Use MuseScore fonts
  % Leland for music notation, MuseJazz Text for chord names and text
  #(define fonts
    (make-pango-font-tree "Leland"
                          "MuseJazz Text"
                          "MuseJazz Text"
                          (/ staff-height pt 20)))
  
}

\layout {
  % Use default indentation for rehearsal marks
  \context {
    \Staff
    \override InstrumentName.self-alignment-X = #LEFT
    \override StaffSymbol.line-count = #5
    \override StaffSymbol.thickness = #0.05
  }
}

\header {
  title = "Song Title"
  subtitle = "Cody Wright"
  composer = "Artist Name #1"
  arranger = "Composer Name #2"
  tagline = ""
}

% Custom part type markup
#(define (part-type-markup)
  (markup
    #:column
    (#:line (#:bold "MASTER")
     #:line (#:bold "RHYTHM")
     #:line (#:with-color "gray" #:fontsize -2 "V1"))))

% ==== Music ====
global = { \time 4/4 \key g \major }

oneLineChords = \chordmode { g1 c1 e1:m d1 }
oneLineSlashes = {
  \improvisationOn c'1 \improvisationOff |
  \improvisationOn c'1 \improvisationOff |
  \improvisationOn c'1 \improvisationOff |
  \improvisationOn c'1 \improvisationOff
}

% Convenience macro for a boxed red label
#(define (red-box text)
  (markup #:with-color "red" #:box #:bold #:pad-around 0.5 #:fontsize 3 text))

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
      instrumentName = #(red-box "INTRO")          % first system
      shortInstrumentName = #(red-box "INTRO")     % default for subsequent systems until changed
    } {
      \global \clef treble

      % Line 1  (uses instrumentName/shortInstrumentName = "INTRO")
      \oneLineSlashes
      \set Staff.shortInstrumentName = #(red-box "VS 1")
      \break

      % Line 2
      \oneLineSlashes
      \set Staff.shortInstrumentName = #(red-box "CH 1")
      \break

      % Line 3
      \oneLineSlashes
      \set Staff.shortInstrumentName = #(red-box "VS 2")
      \break

      % Line 4
      \oneLineSlashes
      \set Staff.shortInstrumentName = #(red-box "CH 2")
      \break

      % Line 5
      \oneLineSlashes
      \set Staff.shortInstrumentName = #(red-box "BR 1")
      \break

      % Line 6
      \oneLineSlashes
      \set Staff.shortInstrumentName = #(red-box "CH 3")
      \break

      % Line 7
      \oneLineSlashes
      \set Staff.shortInstrumentName = #(red-box "SOLO")
      \break

      % Line 8
      \oneLineSlashes
      \set Staff.shortInstrumentName = #(red-box "CH 4")
      \break

      % Line 9
      \oneLineSlashes
      \set Staff.shortInstrumentName = #(red-box "OUT")
      \break

      % Line 10
      \oneLineSlashes
    }
  >>
  \layout { }
}
