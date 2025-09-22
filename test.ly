


\include "utils/paper-setup.ly"
\include "utils/measure-position-detection.ly"
\include "utils/capsule-utils.ly"
\include "utils/new-capsule-utils.ly"
\include "utils/rehearsal-mark-positioning.ly"
\include "utils/auto-four-measure-breaks.ly"
\include "utils/auto-pseudo-indent.ly"
\include "utils/pseudo-indents.ly"





% Test the dynamic functions
%  #(ly:message "Dynamic line break detection functions loaded from utility file")

% Test the section measure counting
%  #(ly:message "Testing section measure counting:")

% Function to create a capsule marker only for marks at the beginning of a line
#(define (capsule-marker grob)
   "Create a capsule marker with red text only for marks at the beginning of a line"
   (let* ((original-text (ly:grob-property grob 'text))
          ;; Check if this mark is at the beginning of a line
          (is-line-start (is-grob-at-line-start? grob)))
     ;; Print the text being processed
     ;; (ly:message "Creating capsule marker for: ~a (line-start: ~a)" original-text is-line-start)
     ;; Apply different capsule styling based on line position
     (if is-line-start
         ;; For marks at the beginning of a line: use full-width capsule
         (let* ((capsule-stencil (make-rehearsal-mark-capsule grob)))
           ;; Set the capsule stencil as the stencil property
           (ly:grob-set-property! grob 'stencil capsule-stencil)
           ;; Use extra-offset for positioning (applied after all other positioning)
           (ly:grob-set-property! grob 'extra-offset (cons -0.5 -4.4))  ; Move right 0.5 and down 4.4 staff spaces
           grob)
         ;; For marks in the middle of a line: use optimal-width capsule (no extra positioning)
         (let* ((capsule-stencil (make-rehearsal-mark-capsule-optimal grob)))
           ;; Set the capsule stencil as the stencil property
           (ly:grob-set-property! grob 'stencil capsule-stencil)
           ;; No extra-offset for middle marks (they stay in their normal position)
           grob))))


\paper {
  
  % Remove ragged-right to use manual padding control instead
  % ragged-right = ##t
  
  % Display spacing dimensions graphically
  % annotate-spacing = ##t
  
  % Reduce system-to-system spacing
  system-system-spacing = #'((basic-distance . 6)
                             (minimum-distance . 4)
                             (padding . 1)
                             (stretchability . 6))
  
  % Reduce score-to-score spacing  
  score-system-spacing = #'((basic-distance . 8)
                            (minimum-distance . 6)
                            (padding . 1)
                            (stretchability . 8))
  
}

\layout {
  \context {
    \Score

   
    % Show 4/4 instead of common time
    \override TimeSignature.style = #'numbered
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

% Function to calculate exact padding to fill line width
#(define (calculate-line-padding grob)
   "Calculate exact padding needed to fill line width"
   (let* ((layout (ly:grob-layout grob))
          (line-width (ly:output-def-lookup layout 'line-width))
          (left-margin (ly:output-def-lookup layout 'left-margin))
          (right-margin (ly:output-def-lookup layout 'right-margin))
          (available-width (- line-width left-margin right-margin))
          (current-extent (ly:grob-extent grob grob X))
          (current-width (interval-length current-extent))
          (needed-padding (- available-width current-width)))
     (max 0 needed-padding)))

% Function to force ragged behavior by preventing stretching
#(define (force-ragged-behavior grob)
   "Force ragged behavior by preventing stretching, like page-spacing.cc does"
   (let* ((layout (ly:grob-layout grob))
          (line-width (ly:output-def-lookup layout 'line-width))
          (left-margin (ly:output-def-lookup layout 'left-margin))
          (right-margin (ly:output-def-lookup layout 'right-margin))
          (available-width (- line-width left-margin right-margin))
          (system (ly:grob-system grob)))
     
     ;; Log the attempt
     (ly:message "Force ragged behavior:")
     (ly:message "  available-width: ~a" available-width)
     (ly:message "  system: ~a" (if (ly:grob? system) "found" "not found"))
     
     ;; Force ragged behavior by setting spacing properties to prevent stretching
     ;; This is equivalent to setting force to 0 in the spring system
     (if (ly:grob? system)
         (begin
           ;; Set the system's spacing properties to use natural spacing (force = 0)
           ;; This prevents the stretching that normally happens with ragged-right = ##f
           (ly:grob-set-property! system 'spacing-increment 1.0)
           (ly:grob-set-property! system 'shortest-duration-space 1.0)
           ;; Also try to set the system to use ragged behavior
           (ly:grob-set-property! system 'ragged-right #t)
           (ly:message "  Forced ragged behavior: natural spacing (force = 0)"))))
     
     grob)

% Define the music content
slashBeatsContent = {
  c4 c c c c
  c c c
  \repeat unfold 32 {
  c16
  }
  c2 c c
}

% Debug function to show which lines need indents with detailed output
debugIndents = #(define-music-function (marks) (ly:music?)
   (let ((indent-lines (identify-lines-needing-indents marks)))
     (ly:message "=== DEBUG: Lines needing indents ===")
     (ly:message "Raw data: ~a" indent-lines)
     (ly:message "Number of lines needing indents: ~a" (length indent-lines))
     ;; Show which specific measures need indents
     (for-each (lambda (line-info)
                 (let ((position (car line-info))
                       (indent (cdr line-info)))
                   (ly:message "  -> Measure ~a needs indent of ~a" position indent)))
               indent-lines)
     (ly:message "=== END DEBUG ==="))
   marks)

% Apply pseudoIndents only to the first line
slashBeats = {
  % First line - make it narrower with right-indent
  \slashBeatsContent
  
  % Remaining lines - normal width
  \repeat unfold 3 {
    \slashBeatsContent
  }
}

marks = {
 s1*2 |
 \mark \markup "Intro"
 s1*4 |
 \mark \markup "VS 1"
 s1*16 |
 \mark \markup "CH 1"
 s1*8 |
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

indents = {

 s1*90 |
  \pseudoIndents 0 44
  s1*2
}



% Test the combined functionality (both section and four-measure breaks)
combinedBreaks = \autoSectionAndFourMeasureBreaks \marks

% Global settings including key signature
global = { \time 4/4 \key e \major }

% Chord progression for E major
chordProgression = \chordmode {
  e2. a4 | a1 | b1 | e1 |
  e1 | a1 | b1 | e1 |
  e1 | a1 | b1 | e1 |
  e1 | a1 | b1 | e1 |
  e1 | a1 | b1 | e1 |
  e1 | a1 | b1 | e1 |
  e1 | a1 | b1 | e1 |
  e1 | a1 | b1 | e1 |
  e1 | a1 | b1 | e1 |
  e1 | a1 | b1 | e1 |
  e1 | a1 | b1 | e1 |
  e1 | a1 | b1 | e1 |
}



\score {
  <<
  \chords { \chs }
  \new ChordNames {
    \chordProgression
  }
  \new Staff \transpose c c' { 
    \global
    <<
      \marks
      \slashBeats
      \combinedBreaks
      
     
    >>
  }
  >>
  \layout {
    \context {
      \Score
      \remove Bar_number_engraver
      \override RehearsalMark.break-align-symbols = #'(left-edge)
      \override RehearsalMark.self-alignment-X = #RIGHT
      \override RehearsalMark.self-alignment-Y = #UP
      \override RehearsalMark.before-line-breaking = #capsule-marker
      \override System.after-line-breaking = #auto-pseudo-indent-callback
    }
  }
}




