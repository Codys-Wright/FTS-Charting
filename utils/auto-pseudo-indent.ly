% Auto-pseudo-indent utilities for automatically applying pseudoIndents to lines with fewer than 4 measures

% Function to calculate indent amount based on number of measures
% For now, returns a fixed value, but can be enhanced later
#(define-public (calculate-indent-for-measures measure-count)
   "Calculate the right-indent amount based on the number of measures in a line"
   ;; For now, return a fixed value of 44
   ;; TODO: Calculate based on measure count and width of measures above
   44)

% Function to identify lines with fewer than 4 measures that need pseudoIndents
#(define-public (identify-lines-needing-indents marks-music)
   "Identify which lines will have fewer than 4 measures and need pseudoIndents"
   (let ((all-break-positions '())
         (current-position 0)
         (current-section-measures 0)
         (lines-needing-indents '()))
     
     ;; Use the same logic as autoSectionAndFourMeasureBreaks to get break positions
     (define (process-element element)
       (cond
        ((eq? (ly:music-property element 'name) 'AdHocMarkEvent)
         ;; Add section break at current position
         (set! all-break-positions (append all-break-positions (list current-position)))
         ;; Generate four-measure breaks for the current section
         (let ((section-four-measure-breaks '()))
           (do ((measure 4 (+ measure 4)))
               ((> measure current-section-measures))
             (set! section-four-measure-breaks (append section-four-measure-breaks (list (+ current-position measure)))))
           (set! all-break-positions (append all-break-positions section-four-measure-breaks)))
         ;; Reset for new section
         (set! current-position (+ current-position current-section-measures))
         (set! current-section-measures 0))
        
        ((eq? (ly:music-property element 'name) 'SkipEvent)
         (let* ((duration (ly:music-property element 'duration))
                (measures (if duration (ly:duration-scale duration) 1)))
           (set! current-section-measures (+ current-section-measures measures))))))
     
     ;; Process all elements to get break positions
     (if (eq? (ly:music-property marks-music 'name) 'SequentialMusic)
         (for-each process-element (ly:music-property marks-music 'elements))
         (process-element marks-music))
     
     ;; Add final section break and four-measure breaks
     (set! all-break-positions (append all-break-positions (list current-position)))
     (let ((final-section-four-measure-breaks '()))
       (do ((measure 4 (+ measure 4)))
           ((> measure current-section-measures))
         (set! final-section-four-measure-breaks (append final-section-four-measure-breaks (list (+ current-position measure)))))
       (set! all-break-positions (append all-break-positions final-section-four-measure-breaks)))
     
     ;; Remove duplicates and sort
     (set! all-break-positions (sort (delete-duplicates all-break-positions) <))
     
     ;; Identify lines with fewer than 4 measures
     (let ((previous-pos 0))
       (for-each (lambda (pos)
                   (let ((measures-in-line (- pos previous-pos)))
                     (if (and (> measures-in-line 0) (< measures-in-line 4))
                         (set! lines-needing-indents (append lines-needing-indents 
                                                           (list (cons pos (calculate-indent-for-measures measures-in-line)))))))
                   (set! previous-pos pos))
                 all-break-positions))
     
     lines-needing-indents))

% Global variable to track which systems have been processed
#(define processed-systems '())

% Global variable to track current measure number
#(define current-measure-number 0)

% Callback function to automatically apply pseudoIndents when lines have fewer than 4 measures
% This runs after line breaking when all information is available
#(define (auto-pseudo-indent-callback system)
   "Callback that checks if a line has fewer than 4 measures and applies pseudoIndents"
   (let* ((system-id (object-address system)))
     
     ;; Only process each system once
     (if (not (member system-id processed-systems))
         (begin
           (set! processed-systems (cons system-id processed-systems))
           (let* ((all-elements (ly:grob-array->list (ly:grob-object system 'all-elements)))
                  (measure-count 0))
             
             ;; Count measures in this system by looking for BarLine grobs
             (for-each (lambda (element)
                         (if (eq? (grob::name element) 'BarLine)
                             (set! measure-count (+ measure-count 1))))
                       all-elements)
             
             ;; If fewer than 4 measures, apply pseudoIndents and log
             (if (and (> measure-count 0) (< measure-count 4))
                 (let ((indent-amount (calculate-indent-for-measures measure-count)))
                   ;; Calculate absolute measure numbers for this system
                   (let ((start-measure current-measure-number)
                         (end-measure (+ current-measure-number measure-count)))
                     (ly:message "PSEUDOINDENT NEEDED: Measures ~a-~a (~a measures), indent ~a" 
                                 start-measure end-measure measure-count indent-amount)
                     ;; Apply pseudoIndents by modifying the system's line-break-system-details
                     (ly:grob-set-property! system 'line-break-system-details 
                                            (cons (cons 'ragged-right #t) 
                                                  (ly:grob-property system 'line-break-system-details))))
                   ;; Update current measure number for next system
                   (set! current-measure-number (+ current-measure-number measure-count)))
                 ;; Update current measure number even if no pseudoIndents needed
                 (set! current-measure-number (+ current-measure-number measure-count))))))))

% Debug function to show which lines need indents with detailed output
debugIndents = #(define-music-function (marks) (ly:music?)
   (let ((indent-lines (identify-lines-needing-indents marks)))
     (ly:message "=== DEBUG: Lines needing indents ===")
     (ly:message "Raw data: ~a" indent-lines)
     (ly:message "Number of lines needing indents: ~a" (length indent-lines))
     ;; Show which specific measures need indents
     (for-each (lambda (line-info)
                 (let ((position (car line-info))
                       (indent (cdr line-info)))
                   (ly:message "  -> Measure ~a needs indent of ~a" position indent)))
               indent-lines)
     (ly:message "=== END DEBUG ==="))
   marks)
