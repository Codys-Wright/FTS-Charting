\version "2.24.0"
\include "english.ly"

#(set-global-staff-size 18)

% Add fonts from local directories
#(ly:font-config-add-directory "fonts/San-Francisco-Pro-Fonts-master/")
#(ly:font-config-add-directory "fonts/musescore/fonts/leeland/")
#(ly:font-config-add-directory "fonts/musescore/fonts/musejazz/")

\paper {
  #(set-paper-size "a4")
  top-margin = 12\mm
  bottom-margin = 12\mm
  left-margin = 16\mm
  right-margin = 16\mm
  indent = 5.00
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
      \line { \fontsize #1 \override #'(font-family . "SF Pro Display Bold") "MASTER" }
      \line { \fontsize #1 \override #'(font-family . "SF Pro Display Bold") "RHYTHM" }
      \line { \fontsize #0.5 \with-color #grey \override #'(font-family . "SF Pro Text") "V1" }
    }
    \center-column {
      \fontsize #10 \override #'(font-family . "SF Pro Display Bold") \fromproperty #'header:title
      \fontsize #-4 \line { \override #'(font-family . "SF Pro Display Thin Italic") "Transcribed by: " \override #'(font-family . "SF Pro Display Medium") \fromproperty #'header:subtitle }
    }
    \column {
      \line { \fontsize #-1 \override #'(font-family . "SF Pro Text") \fromproperty #'header:composer }
      \line { \fontsize #-1 \override #'(font-family . "SF Pro Text") \fromproperty #'header:arranger }
    }
  }
  
  % Use mixed fonts
  % Default Emmentaler for music notation, MuseJazz Text for chords and text, SF Pro Display for titles and fitBoxes
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
    \override TimeSignature.style = #'numbered
  }
  \context {
    \ChordNames
    \override ChordName.font-size = #5
    \override ChordName.Y-offset = #3
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
  \override NoteHead.style = #'slash
  \hide Stem
  b'4 b'4 b'4 b'4 |
  b'4 b'4 b'4 b'4 |
  b'4 b'4 b'4 b'4 |
  b'4 b'4 b'4 b'4
}

% Auto-fitting functionality integrated into fitBox command

% Enhanced fitBox command with auto-fitting and manual word wrapping
#(define-markup-command (fitBox layout props content) (markup?)
  "Auto-fitting fitBox markup command with manual word wrapping"
  (let* ((target-width 20)  ; Target width slightly larger than left margin
         (text (if (string? content) content (format #f "~a" content)))
         (text-length (string-length text))
         (scale-factor (cond
                        ((> text-length 8) 0.7)    ; Very long text - scale down more
                        ((> text-length 6) 0.7)    ; Long text - scale down moderately
                        ((> text-length 4) 0.7)    ; Medium text - scale down slightly
                        (else 1.0))))              ; Short text - no scaling
    (ly:message "AUTO-FIT: Text: ~s, Length: ~a, Scale: ~a" text text-length scale-factor)
    (interpret-markup layout props
      (markup #:with-color "red" #:box #:bold #:fontsize 3 #:pad-around 0.5 #:scale (cons scale-factor scale-factor) #:override #'(font-family . "SF Pro Display") content))))

#(define-markup-command (fitBoxNoScale layout props content) (markup?)
  "Auto-fitting fitBox markup command with no scaling"
  (let* ((target-width 20)  ; Target width slightly larger than left margin
         (text (if (string? content) content (format #f "~a" content)))
         (text-length (string-length text))
         (scale-factor 1.0))  ; No scaling
    (ly:message "AUTO-FIT: Text: ~s, Length: ~a, Scale: ~a" text text-length scale-factor)
    (interpret-markup layout props
      (markup #:with-color "red" #:box #:bold #:fontsize 3 #:pad-around 0.5 #:scale (cons scale-factor scale-factor) #:override #'(font-family . "SF Pro Display") content))))

% Simple red box (back to working version)
#(define (red-box text)
  (markup #:with-color "red" #:box #:bold #:fontsize 3 text))

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
      \set Staff.shortInstrumentName = #(markup #:fitBox "SOLO")
      \break

      % Line 8
      \oneLineSlashes
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
