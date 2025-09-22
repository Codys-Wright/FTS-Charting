% Section Engraver for LilyPond
% Custom engraver that tracks section labels and detects first measures of lines

% Custom scheme function to detect if a measure is at the start of its line
#(define-public (is-measure-at-line-start? grob)
   "Check if a grob is at the start of its line using ly:item-break-dir"
   (and (ly:item? grob)
        (= 1 (ly:item-break-dir grob))))

% Custom scheme function to detect if a measure is at the end of its line  
#(define-public (is-measure-at-line-end? grob)
   "Check if a grob is at the end of its line using ly:item-break-dir"
   (and (ly:item? grob)
        (= -1 (ly:item-break-dir grob))))

% Custom scheme function to detect if a measure is in the middle of its line
#(define-public (is-measure-in-line-middle? grob)
   "Check if a grob is in the middle of its line using ly:item-break-dir"
   (and (ly:item? grob)
        (= 0 (ly:item-break-dir grob))))

% Custom Scheme engraver for section labels
#(define (create-section-label-engraver)
   "Create a custom engraver for section labels with measure tracking"
   (lambda (context)
     (let ((current-measure 0)
           (section-labels '()))
       
       (define (is-first-measure-of-line? measure)
         "Check if this measure is the first measure of a line"
         (= measure 0))  ; Only measure 0 is definitely first on line
       
       (make-translator
        ((start-translation-timestep translator)
         "Called at the start of each timestep"
         (set! current-measure (ly:context-property context 'currentBarNumber 0)))
        
        ((process-music translator)
         "Process music - called for each timestep"
         (set! current-measure (ly:context-property context 'currentBarNumber 0)))
        
        (listeners
         ((ad-hoc-mark-event translator event)
          "Listen to ad-hoc mark events and create section labels"
          (let* ((mark-text (ly:event-property event 'text #f))
                 (text (if mark-text 
                           (if (string? mark-text) mark-text (markup->string mark-text))
                           "MARK"))
                 (is-first-on-line (is-first-measure-of-line? current-measure)))
            (ly:message "Section label engraver: measure ~a, text: ~s, first-on-line: ~a" 
                        current-measure text is-first-on-line)
            ;; Create a custom grob for our section label
            (let ((grob (ly:engraver-make-grob translator 'TextScript event)))
              (ly:grob-set-property! grob 'text (markup #:capsule text))
              (ly:grob-set-property! grob 'direction UP)
              (ly:grob-set-property! grob 'self-alignment-X LEFT)
              (ly:grob-set-property! grob 'extra-offset (cons -20 0)) ; Position on left margin
              ;; Color red if it's first on line
              (when is-first-on-line
                (ly:grob-set-property! grob 'color red))
              (set! section-labels (cons grob section-labels)))))
         
         ((rehearsal-mark-event translator event)
          "Listen to rehearsal mark events and create section labels"
          (let* ((mark-label (ly:event-property event 'label #f))
                 (text (if mark-label (number->string mark-label) "MARK"))
                 (is-first-on-line (is-first-measure-of-line? current-measure)))
            (ly:message "Section label engraver: measure ~a, label: ~s, first-on-line: ~a" 
                        current-measure text is-first-on-line)
            ;; Create a custom grob for our section label
            (let ((grob (ly:engraver-make-grob translator 'TextScript event)))
              (ly:grob-set-property! grob 'text (markup #:capsule text))
              (ly:grob-set-property! grob 'direction UP)
              (ly:grob-set-property! grob 'self-alignment-X LEFT)
              (ly:grob-set-property! grob 'extra-offset (cons -20 0)) ; Position on left margin
              ;; Color red if it's first on line
              (when is-first-on-line
                (ly:grob-set-property! grob 'color red))
              (set! section-labels (cons grob section-labels))))))))))
