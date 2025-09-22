


\include "utils/paper-setup.ly"
\include "utils/measure-position-detection.ly"




% Custom rehearsal mark formatter that tests our dynamic functions
#(define (test-dynamic-mark-formatter mark context)
   "Test formatter that uses our dynamic line break detection"
   (ly:message "Formatter called with mark: ~a" mark)
   (let* ((grob (ly:context-property context 'currentRehearsalMark))
          (is-line-start (if grob (is-grob-at-line-start? grob) #f))
          (is-line-end (if grob (is-grob-at-line-end? grob) #f))
          (is-line-middle (if grob (is-grob-in-line-middle? grob) #f))
          (system (if grob (get-grob-system grob) #f))
          (break-dir (if grob (ly:item-break-dir grob) #f)))
     (ly:message "Mark: ~a, grob: ~a, break-dir: ~a, line-start: ~a, line-end: ~a, line-middle: ~a, system: ~a" 
                 mark grob break-dir is-line-start is-line-end is-line-middle system)
     mark))

% Test the dynamic functions
#(ly:message "Dynamic line break detection functions defined")

% Function to analyze measure positioning in the score
#(define (analyze-measure-positions)
   "Analyze the actual measure positions and line breaks in the score"
   (ly:message "=== MEASURE POSITION ANALYSIS ===")
   ;; This would need to be called after the score is processed
   ;; For now, let's create a test that shows how it would work
   (ly:message "Measure analysis function defined - ready to use with actual grobs"))

% Function to test our dynamic functions with a specific grob
#(define (test-grob-line-position grob)
   "Test our dynamic functions with a specific grob"
   (if grob
       (let* ((is-line-start (is-grob-at-line-start? grob))
              (is-line-end (is-grob-at-line-end? grob))
              (is-line-middle (is-grob-in-line-middle? grob))
              (system (get-grob-system grob))
              (break-dir (ly:item-break-dir grob)))
         (ly:message "GROB TEST: break-dir=~a, line-start=~a, line-end=~a, line-middle=~a, system=~a"
                     break-dir is-line-start is-line-end is-line-middle system)
         (list is-line-start is-line-end is-line-middle))
       (ly:message "GROB TEST: No grob provided")))

% Function to test line break detection using after-line-breaking callback
#(define (test-line-break-detection grob)
   "Test our dynamic line break detection using after-line-breaking callback"
   (let* ((break-dir (ly:item-break-dir grob))
          (is-line-start (is-grob-at-line-start? grob))
          (is-line-end (is-grob-at-line-end? grob))
          (is-line-middle (is-grob-in-line-middle? grob))
          (system (get-grob-system grob)))
     (ly:message "LINE BREAK TEST: break-dir=~a, line-start=~a, line-end=~a, line-middle=~a, system=~a"
                 break-dir is-line-start is-line-end is-line-middle system)
     grob))

% Simple test to verify our functions work
#(ly:message "Testing dynamic functions with dummy values...")
#(ly:message "This is just a test message to verify debug output works")

\layout {
  \context {
    \Score
    \override RehearsalMark.after-line-breaking = #test-line-break-detection
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
  \set Score.rehearsalMarkFormatter = #test-dynamic-mark-formatter
  \mark "M1"
 s1*4 |
 \mark "M5"
 s1*2 |
 \mark "M7"
 s1*2 |
 \mark "M9"
 s1*3 |
 \mark "M12"
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