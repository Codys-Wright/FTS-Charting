% Measure Position Detection Utilities for LilyPond
% Dynamic functions to detect line breaks and measure positions using LilyPond's internal grob system

% Dynamic scheme function to detect if a grob is at the start of a line
% This uses LilyPond's internal grob system to determine line breaks
#(define-public (is-grob-at-line-start? grob)
   "Check if a grob is at the start of its line using ly:item-break-dir"
   (and (ly:item? grob)
        (= 1 (ly:item-break-dir grob))))

% Dynamic scheme function to detect if a grob is at the end of a line  
#(define-public (is-grob-at-line-end? grob)
   "Check if a grob is at the end of its line using ly:item-break-dir"
   (and (ly:item? grob)
        (= -1 (ly:item-break-dir grob))))

% Dynamic scheme function to detect if a grob is in the middle of a line
#(define-public (is-grob-in-line-middle? grob)
   "Check if a grob is in the middle of its line using ly:item-break-dir"
   (and (ly:item? grob)
        (= 0 (ly:item-break-dir grob))))

% Function to get the system that a grob belongs to
#(define-public (get-grob-system grob)
   "Get the system that a grob belongs to"
   (ly:grob-system grob))

% Function to check if two grobs are on the same system
#(define-public (grobs-on-same-system? grob1 grob2)
   "Check if two grobs are on the same system"
   (let* ((system1 (get-grob-system grob1))
          (system2 (get-grob-system grob2)))
     (and system1 system2 (eq? system1 system2))))

% Function to test our dynamic functions with a specific grob
#(define-public (test-grob-line-position grob)
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
#(define-public (test-line-break-detection grob)
   "Test our dynamic line break detection using after-line-breaking callback"
   (let* ((break-dir (ly:item-break-dir grob))
          (is-line-start (is-grob-at-line-start? grob))
          (is-line-end (is-grob-at-line-end? grob))
          (is-line-middle (is-grob-in-line-middle? grob))
          (system (get-grob-system grob)))
     (ly:message "LINE BREAK TEST: break-dir=~a, line-start=~a, line-end=~a, line-middle=~a, system=~a"
                 break-dir is-line-start is-line-end is-line-middle system)
     grob))
