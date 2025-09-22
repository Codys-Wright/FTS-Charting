


\include "utils/paper-setup.ly"
\include "utils/measure-position-detection.ly"
\include "utils/capsule-utils.ly"
\include "utils/new-capsule-utils.ly"
\include "utils/rehearsal-mark-positioning.ly"
\include "utils/auto-breaks.ly"





% Test the dynamic functions
#(ly:message "Dynamic line break detection functions loaded from utility file")

% Function to create a capsule marker only for marks at the beginning of a line
#(define (capsule-marker grob)
   "Create a capsule marker with red text only for marks at the beginning of a line"
   (let* ((original-text (ly:grob-property grob 'text))
          ;; Check if this mark is at the beginning of a line
          (is-line-start (is-grob-at-line-start? grob)))
     ;; Print the text being processed
     (ly:message "Creating capsule marker for: ~a (line-start: ~a)" original-text is-line-start)
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
    % Reduce horizontal spacing
    \override SpacingSpanner.spacing-increment = 0.8
    \override SpacingSpanner.shortest-duration-space = 1.5
  }
  
  \context {
    \Staff
    % Reduce staff-to-staff spacing
    \override StaffGrouper.staff-staff-spacing = #'((basic-distance . 8)
                                                    (minimum-distance . 6)
                                                    (padding . 1)
                                                    (stretchability . 8))
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
 s1*2 |
 \mark \markup "INTRO"
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
 \mark \markup "OUTRO"
 s1*6 |
}



% Use the auto-break function that generates breaks from marks
breaks = \autoBreaks \marks



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
      \override RehearsalMark.break-align-symbols = #'(left-edge)
      \override RehearsalMark.self-alignment-X = #RIGHT
      \override RehearsalMark.self-alignment-Y = #UP
      \override RehearsalMark.before-line-breaking = #capsule-marker
    }
  }
}




