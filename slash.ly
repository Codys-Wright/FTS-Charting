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
  (let ((pitch-set #f))
    #{
      \applyContext #(lambda (context)
        (let* ((clefGlyph (ly:context-property context 'clefGlyph))
               (target-pitch (cond
                              ((string=? clefGlyph "clefs.G")
                               (ly:message "Using B for treble clef")
                               (ly:make-pitch 0 6 0))
                              ((string=? clefGlyph "clefs.F")
                               (ly:message "Using D for bass clef")
                               (ly:make-pitch -1 1 0))
                              (else
                               (ly:message "Using C for unknown clef")
                               (ly:make-pitch 0 0 0)))))
          (ly:message "Applying clef-aware pitch: ~s, target: ~s" clefGlyph target-pitch)
          ; Set the pitch on all note events in the music
          (music-map (lambda (m)
                      (if (music-is-of-type? m 'note-event)
                          (begin
                            (ly:music-set-property! m 'pitch target-pitch)
                            (ly:message "Set note pitch to: ~s" target-pitch)
                            m)
                          m))
                    music)))
      #music
    #}))

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
    ; Set stem properties based on notehead type
    (let ((stem (ly:grob-object grob 'stem)))
      (if stem
          (begin
            (if (or (<= duration-log -1) (= duration-log 0) (= duration-log 1))
                (begin
                  (ly:grob-set-property! stem 'Y-offset 0.4)  ; diamond slash noteheads
                  (ly:grob-set-property! stem 'neutral-direction down)
                  ; Extend stem length for half notes
                  (if (= duration-log 1)
                      (ly:grob-set-property! stem 'length 7.0)))
                (begin
                  (ly:grob-set-property! stem 'Y-offset -0.7)  ; regular slash noteheads
                  (ly:grob-set-property! stem 'neutral-direction DOWN))))))
    (grob-interpret-markup grob (make-ekm-char-markup glyph))))

% Analyze bar context to determine if quarter notes should be stemless
#(define (analyze-bar-context grob)
  (let* ((staff (ly:grob-parent grob X))
         (staff-elements (ly:grob-array->list (ly:grob-object staff 'elements)))
         (noteheads (filter (lambda (grob) (eq? (ly:grob-property grob 'name) 'NoteHead)) staff-elements))
         (current-moment (ly:grob-property grob 'when))
         (current-duration-log (ly:grob-property grob 'duration-log)))
    
    (ly:message "Analyzing bar context for note at moment ~s with duration-log ~s" current-moment current-duration-log)
    
    ; For quarter notes, check if they're in a sequence of quarter notes
    (if (= current-duration-log 2)  ; quarter note
        (let* ((sorted-notes (sort noteheads (lambda (a b) (< (ly:grob-property a 'when) (ly:grob-property b 'when)))))
               (quarter-notes (filter (lambda (n) (= (ly:grob-property n 'duration-log) 2)) sorted-notes))
               (current-index (list-index (lambda (n) (equal? n grob)) quarter-notes)))
          
          (ly:message "Found ~s quarter notes in bar" (length quarter-notes))
          
          ; If this is a quarter note, check if it's isolated or in a sequence
          (if (and current-index (> (length quarter-notes) 1))
              (let* ((prev-note (if (> current-index 0) (list-ref quarter-notes (- current-index 1)) #f))
                     (next-note (if (< current-index (- (length quarter-notes) 1)) (list-ref quarter-notes (+ current-index 1)) #f))
                     (prev-is-quarter (if prev-note (= (ly:grob-property prev-note 'duration-log) 2) #f))
                     (next-is-quarter (if next-note (= (ly:grob-property next-note 'duration-log) 2) #f)))
                
                (ly:message "Quarter note ~s: prev=~s, next=~s" current-index prev-is-quarter next-is-quarter)
                
                ; If both neighbors are quarter notes, this one should be stemless
                (and prev-is-quarter next-is-quarter))
              (ly:stencil-empty))  ; Single quarter note or first/last in sequence gets stem
          (ly:stencil-empty)))  ))

% Global variable to store bar analysis
#(define bar-analysis #f)

% Analyze quarter notes using music-map (inspired by the LSR example)
#(define current-bar-notes (list))
#(define note-counter 0)

#(define (analyze-quarter-notes music)
  (set! current-bar-notes (list))
  (set! note-counter 0)
  (let ((result (music-map (lambda (m)
                            (cond
                             ((music-is-of-type? m 'note-event)
                              (let* ((duration (ly:music-property m 'duration))
                                     (duration-log (ly:duration-log duration))
                                     (dots (ly:duration-dot-count duration))
                                     (is-quarter (and (= duration-log 2) (= dots 0))))
                                ; Store note info with reference to music object
                                (set! current-bar-notes (append current-bar-notes (list (list m duration-log dots is-quarter))))
                                (ly:message "Note: duration-log=~s, dots=~s, is-quarter=~s" duration-log dots is-quarter)
                                m))
                             ((eq? (ly:music-property m 'name) 'BarCheck)
                              (ly:message "BAR CHECK EVENT detected")
                              (ly:message "Bar has ~s notes" (length current-bar-notes))
                              ; Process this bar to mark consecutive quarter notes as stemless
                              (let ((consecutive-quarters (list)))
                                (for-each (lambda (note-info)
                                           (let ((music-obj (car note-info))
                                                 (duration-log (cadr note-info))
                                                 (dots (caddr note-info))
                                                 (is-quarter (cadddr note-info)))
                                             (if is-quarter
                                                 (set! consecutive-quarters (append consecutive-quarters (list music-obj)))
                                                 (begin
                                                   ; End of consecutive quarters, mark them if 2 or more
                                                   (if (>= (length consecutive-quarters) 2)
                                                       (for-each (lambda (m) 
                                                                  (ly:music-set-property! m 'stemless #t)
                                                                  (ly:message "Marking note as stemless"))
                                                                consecutive-quarters))
                                                   (set! consecutive-quarters (list))))))
                                         current-bar-notes)
                                ; Process any remaining consecutive quarters at end of bar
                                (if (>= (length consecutive-quarters) 2)
                                    (for-each (lambda (m) 
                                               (ly:music-set-property! m 'stemless #t)
                                               (ly:message "Marking note as stemless (end of bar)"))
                                             consecutive-quarters)))
                              (set! current-bar-notes (list))
                              m)
                             (else m)))
                          music)))
    ; Process any remaining notes after all music (final bar without bar check)
    (if (> (length current-bar-notes) 0)
        (begin
          (ly:message "Processing final bar with ~s notes" (length current-bar-notes))
          (let ((consecutive-quarters (list)))
            (for-each (lambda (note-info)
                       (let ((music-obj (car note-info))
                             (duration-log (cadr note-info))
                             (dots (caddr note-info))
                             (is-quarter (cadddr note-info)))
                         (if is-quarter
                             (set! consecutive-quarters (append consecutive-quarters (list music-obj)))
                             (begin
                               ; End of consecutive quarters, mark them if 2 or more
                               (if (>= (length consecutive-quarters) 2)
                                   (for-each (lambda (m) 
                                              (ly:music-set-property! m 'stemless #t)
                                              (ly:message "Marking note as stemless (final bar)"))
                                            consecutive-quarters))
                               (set! consecutive-quarters (list))))))
                     current-bar-notes)
            ; Process any remaining consecutive quarters at end of final bar
            (if (>= (length consecutive-quarters) 2)
                (for-each (lambda (m) 
                           (ly:music-set-property! m 'stemless #t)
                           (ly:message "Marking note as stemless (final bar end)"))
                         consecutive-quarters)))))
    result))

% Dynamic stem based on duration and bar context
#(define (duration-stem grob)
  (let* ((note-heads-array (ly:grob-object grob 'note-heads))
         (note-heads (if (ly:grob-array? note-heads-array)
                         (ly:grob-array->list note-heads-array)
                         '()))
         (note-head (if (pair? note-heads) (car note-heads) #f))
         (event (if note-head (ly:grob-property note-head 'cause) #f))
         (stemless (if (ly:stream-event? event)
                       (ly:event-property event 'stemless #f)
                       #f)))
    (if stemless
        (begin
          (ly:message "Hiding stem for stemless quarter note")
          (ly:make-stencil "" '(0 . 0) '(0 . 0)))
        (ly:stem::print grob))))

% Slash function with dynamic noteheads and stems based on duration
rh = #(define-music-function (music) (ly:music?)
  "Create slash notes with dynamic noteheads and stems based on duration"
  #{
    \override NoteHead.stencil = #duration-notehead
    \override Stem.stencil = #duration-stem
    \override Accidental.stencil = ##f
    \override Stem.details.beamed-lengths = #'(3)
    \applyMusic #analyze-quarter-notes
    \middleLineNote #music
    \revert NoteHead.stencil
    \revert Stem.stencil
    \revert Accidental.stencil
  #})


\score {
  <<
  \new Staff {
    \clef treble
    \time 4/4
    \key c \major
    \rh {4. 8 4 4 | 4 4 4 4 | 4 4 8 8 8 8 |  2 2 | 1} c'4 d' e' f' g' \rh {4 4 4 4} c'4
  }
  
  \new Staff {
    \clef bass
    \time 4/4
    \key c \major
    \rh {4. 8 4 4 | 4 4 4 4 | 4 4 8 8 8 8 |  2 2 | 1} c4 d e f g \rh {4 4 4 4} c4
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