% Key changes configuration for LilyPond charting workflow
% This file contains key signature display settings and customizations
% Based on LSR snippet #775 by harm6

% Include the original LSR snippet definitions
\include "utils/affecting-barline-items.ly"

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
% Based on the breaks utilities pattern
#(define-public (key-change-tracker-engraver context)
  (let ((key-change-positions '())
        (first-key-change #t)
        (first-key-sig #t))
    (make-engraver
     (listeners
      ((key-change-event engraver event)
       (let* ((moment (ly:context-current-moment context)))
         (if (ly:moment? moment)
             (let* ((beat (ly:moment-main-numerator moment))
                    (position (ly:moment-main-denominator moment)))
               (set! key-change-positions 
                     (cons (cons beat position) key-change-positions))
               (if first-key-change
                   (begin
                     (set! first-key-change #f)
                     (ly:message "KEY CHANGE EVENT at beat ~a/~a (SKIPPING - initial key)" beat position))
                   (ly:message "KEY CHANGE EVENT at beat ~a/~a" beat position)))
             (ly:message "KEY CHANGE EVENT - no valid moment")))))
     (acknowledgers
      ((key-signature-interface engraver grob source-engraver)
       (let* ((non-default (ly:grob-property grob 'non-default #f))
              (courtesy (ly:grob-property grob 'courtesy #f))
              (break-dir (ly:item-break-dir grob)))
         (ly:message "KEY SIG: non-default=~a, courtesy=~a, break-dir=~a, first-sig=~a" non-default courtesy break-dir first-key-sig)
         ;; Color red if it's a non-default key signature (actual key change) at a line break
         ;; BUT skip the very first key signature (initial key)
         (if (and (or non-default courtesy) break-dir (not first-key-sig))
             (begin
               (ly:message "COLORING KEY SIG - key change at line break")
               (ly:grob-set-property! grob 'color red))
             (if first-key-sig
                 (ly:message "SKIPPING first key signature")))
         ;; Mark that we've seen the first key signature
         (set! first-key-sig #f)))))))