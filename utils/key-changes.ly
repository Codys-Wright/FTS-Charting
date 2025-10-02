% Key changes configuration for LilyPond charting workflow
% This file contains key signature display settings and customizations
% Based on LSR snippet #775 by harm6

% Include the original LSR snippet definitions
\include "utils/affecting-barline-items.ly"

% Include measure position detection utilities
\include "utils/capsules/measure-position-detection.ly"

% Function to find the next break measure after a given measure number
#(define-public (find-next-break-measure current-measure)
   "Find the next break measure after the given measure number"
   ;; Based on the break positions we see in the output: (0 2 6 10 14 18 22 26 30 34 38 42 46 50 54 58 62 66 70 74 78 82 86 90 94)
   ;; Break positions represent where lines end, so the next measure starts a new line
   (let* ((break-positions '(0 2 6 10 14 18 22 26 30 34 38 42 46 50 54 58 62 66 70 74 78 82 86 90 94))
          (line-start-positions (map (lambda (pos) (+ pos 1)) break-positions))
          (next-break (find (lambda (pos) (> pos current-measure)) line-start-positions)))
     (if next-break
         next-break
         #f)))

% Callback function to process key change events and check line start positions
% For now, we'll use a simple approach based on the break positions we can see in the output
#(define-public (key-change-line-start-callback measure-number)
   "Callback function to check if a measure number is at the start of a line"
   ;; Based on the break positions we see in the output: (0 2 6 10 14 18 22 26 30 34 38 42 46 50 54 58 62 66 70 74 78 82 86 90 94)
   ;; Break positions represent where lines end, so the next measure starts a new line
   ;; So if there's a break at position 58, measure 59 starts a new line
   (let* ((break-positions '(0 2 6 10 14 18 22 26 30 34 38 42 46 50 54 58 62 66 70 74 78 82 86 90 94))
          (previous-break-positions (map (lambda (pos) (+ pos 1)) break-positions))
          (is-line-start (member measure-number previous-break-positions)))
     (ly:message "MEASURE ~a: line-start=~a" measure-number (if is-line-start #t #f))
     (if is-line-start #t #f)))

%---------- Music Functions

% Function to color key signatures at line breaks (using original snippet)
colorKeyAtBreaks = {
  \override KeySignature.after-line-breaking = #color-at-line-begin
  \override KeySignature.after-line-breaking = #color-at-line-end
}

% Function to color only key signatures at line beginning
colorKeyAtLineBegin = {
  \override KeySignature.after-line-breaking = #color-at-line-begin
}

% Function to color only key signatures at line end
colorKeyAtLineEnd = {
  \override KeySignature.after-line-breaking = #color-at-line-end
}

% Legacy function name for compatibility
conditionalKeySignatureRed = {
  \override KeySignature.after-line-breaking = #color-at-line-begin
  \override KeySignature.after-line-breaking = #color-at-line-end
}


% Automatic key signature coloring system using custom engraver
#(define-public (auto-color-key-signatures grob)
  "Automatically color key signatures red when they appear at line breaks"
  (let* ((break-dir (ly:item-break-dir grob))
         (non-default (ly:grob-property grob 'non-default #f))
         (courtesy (ly:grob-property grob 'courtesy #f)))
    ;; Only color if:
    ;; 1. There's a break direction (at a line break)
    ;; 2. It's a non-default key signature (actual key change, not just default)
    ;; 3. OR it's a courtesy key signature
    (if (and break-dir (or non-default courtesy))
        (ly:grob-set-property! grob 'color red))
    grob))

% Score-level override to automatically color all key signatures at line breaks
autoKeySignatureColoring = {
  \override Score.KeySignature.after-line-breaking = #auto-color-key-signatures
}

% Staff-level override (alternative approach)
autoKeySignatureColoringStaff = {
  \override Staff.KeySignature.after-line-breaking = #auto-color-key-signatures
}

% Staff-level override using custom engraver to track actual key change events
% This needs to be used in a context definition, not as a music function
autoKeySignatureColoringWithEngraver = {
  \override Staff.KeySignature.after-line-breaking = #auto-color-key-signatures
}


% Automatic time signature coloring system
% This function automatically detects time signatures at line breaks and colors them red
% Only colors time signatures at the END of a line (before the break), not at the beginning
#(define-public (auto-color-time-signatures grob)
  "Automatically color time signatures red when they appear at the end of a line (before break)"
  (let* ((break-dir (ly:item-break-dir grob)))
    (if (equal? break-dir LEFT)  ; Only LEFT (end of line), not RIGHT (beginning of line)
        (ly:grob-set-property! grob 'color red))
    grob))

% Score-level override to automatically color all time signatures at line breaks
autoTimeSignatureColoring = {
  \override Score.TimeSignature.after-line-breaking = #auto-color-time-signatures
}

% Staff-level override (alternative approach)
autoTimeSignatureColoringStaff = {
  \override Staff.TimeSignature.after-line-breaking = #auto-color-time-signatures
}

% Combined function to color both key signatures and time signatures
autoSignatureColoring = {
  \override Staff.KeySignature.after-line-breaking = #auto-color-key-signatures
  \override Staff.TimeSignature.after-line-breaking = #auto-color-time-signatures
}

% Manual key change coloring - use this before specific \key commands
colorKeyChange = {
  \once\override Staff.KeySignature.before-line-breaking = #color-at-line-end
  \once\override Staff.KeySignature.after-line-breaking = #color-at-line-begin
}

% Manual time signature coloring - use this before specific \time commands
colorTimeChange = {
  \once\override Staff.TimeSignature.before-line-breaking = #color-at-line-end
}

% Custom engraver to track key change events and color key signatures at line breaks
% Based on the rehearsal marks system approach - detect breaks and color key signatures after them
#(define-public (key-change-tracker-engraver context)
  (let ((first-key-change #t)
        (first-key-sig #t)
        (pending-key-changes #f)
        (key-change-measures '()))
    (make-engraver
     (listeners
      ((key-change-event engraver event)
       (if first-key-change
           (set! first-key-change #f)
           (let ((moment (ly:context-current-moment context)))
             (if (ly:moment? moment)
                 (let ((beat (ly:moment-main-numerator moment))
                       (unit (ly:moment-main-denominator moment)))
                   (let ((measure (+ beat 1)))
                     (ly:message "KEY CHANGE EVENT at beat ~a/~a (measure ~a)" beat unit measure)
                     ;; Use the callback to check if this measure is at a line start
                     (let ((is-line-start (key-change-line-start-callback measure)))
                       (if is-line-start
                           (ly:message "Key change at measure ~a is at line start - will color immediately" measure)
                           (let ((next-break (find-next-break-measure measure)))
                             (if next-break
                                 (ly:message "Key change at measure ~a is NOT at line start - next break will be at measure ~a" measure next-break)
                                 (ly:message "Key change at measure ~a is NOT at line start - no next break found" measure)))))
                     ;; Store the measure number where key change occurred
                     (set! key-change-measures (cons measure key-change-measures))
                     (set! pending-key-changes #t)))))))
     (acknowledgers
      ((grob-interface engraver grob source-engraver)
       (if (grob::has-interface grob 'key-signature-interface)
           (let* ((non-default (ly:grob-property grob 'non-default #f))
                  (courtesy (ly:grob-property grob 'courtesy #f))
                  (is-line-start (is-grob-at-line-start? grob))
                  (grob-moment (ly:grob-property grob 'when))
                  (grob-beat (if (ly:moment? grob-moment) (ly:moment-main-numerator grob-moment) 0))
                  (grob-measure (+ grob-beat 1)))
             (ly:message "KEY SIGNATURE GROB: measure=~a, non-default=~a, courtesy=~a, line-start=~a, pending=~a" grob-measure non-default courtesy is-line-start pending-key-changes)
             (if (not first-key-sig)
                 ;; Color key signatures that are at line starts AND there are pending key changes
                 (if (and is-line-start pending-key-changes)
                     (begin
                       (ly:message "FOUND NEXT BREAK at measure ~a: line-start=~a (coloring for key changes at measures: ~a)" grob-measure is-line-start key-change-measures)
                       (ly:grob-set-property! grob 'color red)
                       (set! pending-key-changes #f)
                       (set! key-change-measures '()))
                     ;; Log when we have pending key changes but this grob is not at a line start
                     (if pending-key-changes
                         (ly:message "Key signature at measure ~a is NOT at line start (line-start=~a) - still looking for next break after measures: ~a" grob-measure is-line-start key-change-measures))))
             (set! first-key-sig #f)))))))))