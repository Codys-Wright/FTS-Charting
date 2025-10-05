\version "2.24.0"

% Custom Rhythmic Notation Utilities
% Provides intelligent slash notation with automatic stem hiding for consecutive quarter notes

% Requires esmuflily for SMuFL glyphs
ekmFont = "Bravura"
\include "third-party/esmuflily/ly/esmufl.ily"

% ===== CLEF DETECTION =====

detectClef = #(define-music-function () ()
  "Detect and print the current clef value"
  (make-apply-context
    (lambda (context)
      (let ((clefGlyph (ly:context-property context 'clefGlyph))
            (middleCPosition (ly:context-property context 'middleCPosition)))
        (make-music 'Music 'void #t)))))

#(define clef-pitch #f)

#(define (set-clef-pitch context)
  (let ((clefGlyph (ly:context-property context 'clefGlyph)))
    (set! clef-pitch 
          (cond
           ((string=? clefGlyph "clefs.G")
            (ly:make-pitch 0 6 0))
           ((string=? clefGlyph "clefs.F")
            (ly:make-pitch -1 1 0))
           (else
            (ly:make-pitch 0 0 0))))))

% ===== MIDDLE LINE NOTE FUNCTION =====

middleLineNote = #(define-music-function (music) (ly:music?)
  "Apply clef-aware pitch to rhythm notes"
  (let ((pitch-set #f))
    #{
      \applyContext #(lambda (context)
        (let* ((clefGlyph (ly:context-property context 'clefGlyph))
               (target-pitch (cond
                              ((string=? clefGlyph "clefs.G")
                               (ly:make-pitch 0 6 0))
                              ((string=? clefGlyph "clefs.F")
                               (ly:make-pitch -1 1 0))
                              (else
                               (ly:make-pitch 0 0 0)))))
          (music-map (lambda (m)
                      (if (music-is-of-type? m 'note-event)
                          (begin
                            (ly:music-set-property! m 'pitch target-pitch)
                            m)
                          m))
                    music)))
      #music
    #}))

% ===== CUSTOM NOTEHEAD STYLES =====

#(define-public diamond-slash-style
  (lambda (grob)
    (let* ((duration-log (ly:grob-property grob 'duration-log)))
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

% ===== QUARTER NOTE ANALYSIS =====

#(define bar-analysis #f)
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
                                m))
                             ((eq? (ly:music-property m 'name) 'BarCheck)
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
                                                                  (ly:music-set-property! m 'stemless #t))
                                                                consecutive-quarters))
                                                   (set! consecutive-quarters (list))))))
                                         current-bar-notes)
                                ; Process any remaining consecutive quarters at end of bar
                                (if (>= (length consecutive-quarters) 2)
                                    (for-each (lambda (m) 
                                               (ly:music-set-property! m 'stemless #t))
                                             consecutive-quarters)))
                              (set! current-bar-notes (list))
                              m)
                             (else m)))
                          music)))
    ; Process any remaining notes after all music (final bar without bar check)
    (if (> (length current-bar-notes) 0)
        (begin
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
                                              (ly:music-set-property! m 'stemless #t))
                                            consecutive-quarters))
                               (set! consecutive-quarters (list))))))
                     current-bar-notes)
            ; Process any remaining consecutive quarters at end of final bar
            (if (>= (length consecutive-quarters) 2)
                (for-each (lambda (m) 
                           (ly:music-set-property! m 'stemless #t))
                         consecutive-quarters)))))
    result))

% ===== DYNAMIC STEM FUNCTION =====

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
        (ly:make-stencil "" '(0 . 0) '(0 . 0))
        (ly:stem::print grob))))

% ===== SKIP EXPANSION =====

#(define (expand-skips music)
  "Expand skip events (s1, s2, etc.) into quarter notes"
  (music-map (lambda (m)
              (if (music-is-of-type? m 'skip-event)
                  (let* ((duration (ly:music-property m 'duration))
                         (length (ly:duration-length duration))
                         (quarter-length (ly:make-duration 2 0))
                         (num-quarters (inexact->exact (round (/ (ly:moment-main length) 1/4)))))
                    (make-sequential-music
                     (map (lambda (i)
                           (make-music 'NoteEvent
                                      'duration quarter-length
                                      'pitch (ly:make-pitch 0 0 0)))
                         (iota num-quarters))))
                  m))
            music))

% ===== MAIN RHYTHMIC NOTATION FUNCTION =====

rh = #(define-music-function (music) (ly:music?)
  "Create slash notes with dynamic noteheads, intelligent stem hiding, and skip expansion.
   
   Usage examples:
     \\rh { 4 4 4 4 }         % Four quarter notes (all stemless)
     \\rh { 4 4 8 8 4 }       % Mixed durations (stems appear as needed)
     \\rh { s1*4 }            % Expands to 16 quarter notes across 4 bars
     \\rh { 4. 8 2 }          % Dotted quarter, eighth, half note"
  (let ((expanded-music (expand-skips music)))
    #{
      \override NoteHead.stencil = #duration-notehead
      \override Stem.stencil = #duration-stem
      \override Accidental.stencil = ##f
      \override Stem.details.beamed-lengths = #'(3)
      \applyMusic #analyze-quarter-notes
      \middleLineNote #expanded-music
      \revert NoteHead.stencil
      \revert Stem.stencil
      \revert Accidental.stencil
    #}))

