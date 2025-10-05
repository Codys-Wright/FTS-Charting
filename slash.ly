\version "2.24.0"

\include "english.ly"

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



\score {
  <<
  \new Staff {
    \clef treble
    \time 4/4
    \key c \major
    \middleLineNote { 4 4.~ 2}
  }
  
  \new Staff {
    \clef bass
    \time 4/4
    \key c \major
    \middleLineNote { 4 2 4 } 
  }
  >>
}