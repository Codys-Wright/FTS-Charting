

% Set the font to Bravura for SMuFL support
ekmFont = "Bravura"

% Include esmuflily for SMuFL support
\include "/home/cody/Documents/Development/reference/esmuflily/ly/cosmufl.ily"

\include "utils/paper-setup.ly"
\include "utils/measure-position-detection.ly"
\include "utils/capsule-utils.ly"
\include "utils/new-capsule-utils.ly"
\include "utils/rehearsal-mark-positioning.ly"
\include "utils/auto-four-measure-breaks.ly"
\include "utils/pseudo-indents.ly"
\include "utils/auto-pseudo-indents.ly"





% Test the dynamic functions
%  #(ly:message "Dynamic line break detection functions loaded from utility file")

% Test the section measure counting
%  #(ly:message "Testing section measure counting:")

% Function to create thin staff lines
#(define (thin-staff-lines grob)
  (define (index-cell cell dir)
    (if (equal? dir RIGHT)
        (cdr cell)
        (car cell)))

  (define (index-set-cell! x dir val)
    (case dir
      ((-1) (set-car! x val))
      ((1) (set-cdr! x val))))

  (let* ((common (ly:grob-system grob))
         (span-points '(0 . 0))
         (thickness 0.05)  ; Very thin staff lines
         (width (ly:grob-property grob 'width))
         (line-positions (ly:grob-property grob 'line-positions))
         (staff-space (ly:grob-property grob 'staff-space 1))
         (line-stencil #f)
         (total-lines empty-stencil))

    ;; Calculate span points like the original example
    (for-each
     (lambda (dir)
       (if (and (= dir RIGHT)
                (number? width))
           (set-cdr! span-points width)
           (let* ((bound (ly:spanner-bound grob dir))
                  (bound-ext (ly:grob-extent bound bound X)))
             
             (index-set-cell! span-points dir
                              (ly:grob-relative-coordinate bound common X))
             (if (and (not (ly:item-break-dir bound))
                      (not (interval-empty? bound-ext)))
                 (index-set-cell! span-points dir 
                                  (+ (index-cell span-points dir)
                                     (index-cell bound-ext dir))))))
       (index-set-cell! span-points dir (- (index-cell span-points dir)
                                           (* dir thickness 0.5))))
     (list LEFT RIGHT))

    (set! span-points
          (coord-translate span-points
                           (- (ly:grob-relative-coordinate grob common X))))
    (set! line-stencil
          (make-line-stencil thickness (car span-points) 0 (cdr span-points) 0))

    (if (pair? line-positions)
        (for-each (lambda (position)
                    (set! total-lines
                          (ly:stencil-add
                           total-lines
                           (ly:stencil-translate-axis
                            line-stencil
                            (* position staff-space 0.5) Y))))
                  line-positions)       
        (let* ((line-count (ly:grob-property grob 'line-count 5))
               (height (* (1- line-count) (/ staff-space 2))))
          (do ((i 0 (1+ i)))                      
              ((= i line-count))
            (set! total-lines (ly:stencil-add
                               total-lines
                               (ly:stencil-translate-axis
                                line-stencil
                                (- height (* i staff-space)) Y))))))
    total-lines))

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
  
  % Make staff lines thinner (affects only staff lines, not stems/beams)
  % line-thickness = #0.05
  
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
  \repeat unfold 23 {
    \hide Stem
    \override NoteHead.style = #'slash
    b4 b4 b4 b4 |
  }
  
}

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

% Manual indents removed - now handled automatically by autoBreaksAndPseudoIndents



% Test the combined functionality (breaks and automatic pseudo-indents for short lines)
% First, let's see what positions need pseudo-indents
testPositions = \getShortLinePositions \marks

% Combined function that does both breaks and pseudo-indents in the right order
combinedBreaks = \autoBreaksAndPseudoIndents \marks

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
        % Enable SMuFL support for Leland font (excluding noteheads for now)
        \ekmSmuflOn #'all
        % Make staff lines thinner using custom stencil
        \override StaffSymbol.stencil = #thin-staff-lines
        \override BarNumber.font-size = -5  % Make bar numbers smaller
        \override BarNumber.Y-offset = #4.5  % Move bar numbers to the right
        \override BarNumber.X-offset = #0.4  % Move bar numbers to the right
      \override RehearsalMark.break-align-symbols = #'(left-edge)
      \override RehearsalMark.self-alignment-X = #RIGHT
      \override RehearsalMark.self-alignment-Y = #UP
      \override RehearsalMark.before-line-breaking = #capsule-marker
        % Position bar numbers to avoid conflict with rehearsal marks
    }
  }
}




