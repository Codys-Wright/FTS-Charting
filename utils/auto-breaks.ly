% Auto-break utilities for generating line breaks based on measures and marks

#(define-public (analyze-marks-for-breaks marks-music)
   "Analyze marks music and return a list of break positions"
   (let* ((break-positions '())
          (measure-count 0))
     
     (define (process-element element)
       (cond
        ((eq? (ly:music-property element 'name) 'AdHocMarkEvent)
         (set! break-positions (append break-positions (list measure-count)))
         (set! measure-count 0))
        
        ((eq? (ly:music-property element 'name) 'SkipEvent)
         (let* ((duration (ly:music-property element 'duration))
                (measures (if duration (ly:duration-scale duration) 1)))
           (set! measure-count (+ measure-count measures)))))
       break-positions)
     
     (if (eq? (ly:music-property marks-music 'name) 'SequentialMusic)
         (for-each process-element (ly:music-property marks-music 'elements))
         (process-element marks-music))
     
     break-positions))

#(define-public (analyze-section-measures marks-music)
   "Analyze marks music and print measure counts for each section"
   (let ((section-measure-count 0)
         (section-number 1))
     
     (define (process-element element)
       (cond
        ((eq? (ly:music-property element 'name) 'AdHocMarkEvent)
         ;; Print current section info
         (ly:message "Section ~a: ~a measures" section-number section-measure-count)
         ;; Reset for new section
         (set! section-measure-count 0)
         (set! section-number (+ section-number 1)))
        
        ((eq? (ly:music-property element 'name) 'SkipEvent)
         (let* ((duration (ly:music-property element 'duration))
                (measures (if duration (ly:duration-scale duration) 1)))
           (set! section-measure-count (+ section-measure-count measures))))))
     
     (if (eq? (ly:music-property marks-music 'name) 'SequentialMusic)
         (for-each process-element (ly:music-property marks-music 'elements))
         (process-element marks-music))
     
     ;; Print final section
     (ly:message "Section ~a: ~a measures" section-number section-measure-count)))

printSectionMeasures = #(define-music-function (marks) (ly:music?)
   "Print measure counts for each section"
   (analyze-section-measures marks)
   marks)

autoBreaks = #(define-music-function (marks) (ly:music?)
   "Generate automatic line breaks based on marks structure"
   (let ((break-positions (analyze-marks-for-breaks marks)))
     (ly:message "Break positions: ~a" break-positions)
     ;; Generate breaks based on positions
     (let ((result '()))
       (for-each (lambda (pos)
                   ;; Add skip for the measures
                   (set! result (append result (list (make-music 'SkipEvent 'duration (ly:make-duration 0 0 pos)))))
                   ;; Add break with force permission
                   (set! result (append result (list (make-music 'LineBreakEvent 'break-permission 'force)))))
                 break-positions)
       ;; Create the result as sequential music
       (make-music 'SequentialMusic 'elements result))))