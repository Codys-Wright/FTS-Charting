\version "2.24.0"

\include "english.ly"

% Set font to Bravura for SMuFL glyphs
ekmFont = "Bravura"

\include "utils/third-party/esmuflily/ly/esmufl.ily"

detectClef = #(define-music-function () ()
  "Detect and print the current clef value"
  (make-apply-context
    (lambda (context)
      (let ((clefGlyph (ly:context-property context 'clefGlyph))
            (middleCPosition (ly:context-property context 'middleCPosition)))
        (ly:message "clefGlyph: ~s" clefGlyph)
        (ly:message "middleCPosition: ~s" middleCPosition)
        (make-music 'Music 'void #t)))))

#(define clef-pitch #f)

#(define (set-clef-pitch context)
  (let ((clefGlyph (ly:context-property context 'clefGlyph)))
    (ly:message "Detected clef: ~s" clefGlyph)
    (set! clef-pitch 
          (cond
           ((string=? clefGlyph "clefs.G")
            (ly:message "Setting pitch to B for treble clef")
            (ly:make-pitch 0 6 0))
           ((string=? clefGlyph "clefs.F")
            (ly:message "Setting pitch to D for bass clef")
            (ly:make-pitch -1 1 0))
           (else
            (ly:message "Setting pitch to C for unknown clef")
            (ly:make-pitch 0 0 0))))))

middleLineNote = #(define-music-function (music) (ly:music?)
  "Apply clef-aware pitch to rhythm notes"
  #{
    \applyContext #(lambda (context)
      (let ((clefGlyph (ly:context-property context 'clefGlyph)))
        (ly:message "Applying clef-aware pitch: ~s" clefGlyph)
        (let ((pitch (cond
                      ((string=? clefGlyph "clefs.G")
                       (ly:message "Using B for treble clef")
                       (ly:make-pitch 0 6 0))
                      ((string=? clefGlyph "clefs.F")
                       (ly:message "Using D for bass clef")
                       (ly:make-pitch -1 1 0))
                      (else
                       (ly:message "Using C for unknown clef")
                       (ly:make-pitch 0 0 0)))))
          (ly:context-set-property! context 'middleCPosition 0)
          (ly:context-set-property! context 'clefGlyph "clefs.G")
          (ly:context-set-property! context 'clefPosition 0))))
    #music
  #})

% Define custom notehead styles
#(define-public diamond-slash-style
  (lambda (grob)
    (let* ((duration-log (ly:grob-property grob 'duration-log)))
      (ly:message "diamond-slash-style called with duration-log: ~s" duration-log)
      (grob-interpret-markup grob (make-ekm-char-markup #xE104)))))

#(define-public regular-slash-style
  (lambda (grob)
    (grob-interpret-markup grob (make-ekm-char-markup #xE103))))

% Dynamic notehead based on duration
#(define (duration-notehead grob)
  (let* ((duration-log (ly:grob-property grob 'duration-log))
         (glyph (cond
                 ((<= duration-log -1) #xE104)  ; breve and longer - diamond slash
                 ((= duration-log 0) #xE104)    ; whole note - diamond slash
                 ((= duration-log 1) #xE104)    ; half note - diamond slash
                 ((= duration-log 2) #xE101)    ; quarter note - slash with horizontal ends
                 ((= duration-log 3) #xE101)    ; eighth note - slash with horizontal ends
                 ((= duration-log 4) #xE101)    ; sixteenth note - slash with horizontal ends
                 ((= duration-log 5) #xE101)    ; thirty-second note - slash with horizontal ends
                 ((= duration-log 6) #xE101)    ; sixty-fourth note - slash with horizontal ends
                 (else #xE101))))
    (ly:message "duration-notehead called with duration-log: ~s, glyph: ~s" duration-log glyph)
    ; Set stem properties based on notehead type
    (let ((stem (ly:grob-object grob 'stem)))
      (ly:message "Stem found: ~s" stem)
      (if stem
          (begin
            (ly:message "Setting stem properties for duration-log: ~s" duration-log)
            (if (or (<= duration-log -1) (= duration-log 0) (= duration-log 1))
                (begin
                  (ly:message "Setting diamond slash stem properties")
                  (ly:grob-set-property! stem 'Y-offset 0.4)  ; diamond slash noteheads
                  (ly:grob-set-property! stem 'neutral-direction down))
                (begin
                  (ly:message "Setting regular slash stem properties")
                  (ly:grob-set-property! stem 'Y-offset -0.7)  ; regular slash noteheads
                  (ly:grob-set-property! stem 'neutral-direction DOWN))))
          (ly:message "No stem found for this notehead")))
    (grob-interpret-markup grob (make-ekm-char-markup glyph))))

% Dynamic stem based on duration
#(define (duration-stem grob)
  (ly:stem::print grob))

% Slash function with dynamic noteheads and stems based on duration
sl = #(define-music-function (music) (ly:music?)
  "Create slash notes with dynamic noteheads and stems based on duration"
  #{
    \override NoteHead.stencil = #duration-notehead
    \override Stem.stencil = #duration-stem
    \override Accidental.stencil = ##f
    \override Stem.details.beamed-lengths = #'(3)
    \middleLineNote #music
    \revert NoteHead.stencil
    \revert Stem.stencil
    \revert Accidental.stencil
  #})

% Rhythm function with slash styling but keeps stems
rh = #(define-music-function (music) (ly:music?)
  "Create rhythm notes with slash styling and stems"
  #{
    \override NoteHead.style = #'slash
    \override Accidental.stencil = ##f
    \middleLineNote #music
    \revert NoteHead.style
    \revert Accidental.stencil
  #})

\score {
  <<
  \new Staff {
    \clef treble
    \time 4/4
    \key c \major
    \sl {16 16 8 8. 16 4 4. 8 2 2. 1}
  }
  
  \new Staff {
    \clef bass
    \time 4/4
    \key c \major
    \rh { 4 4  2 }
    g4
  }
  >>
  \layout {
    \context {
      \Score
      \ekmSmuflOn #'all
    }
  }
}