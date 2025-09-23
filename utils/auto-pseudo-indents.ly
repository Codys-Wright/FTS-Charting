% Auto-pseudo-indent utilities for automatically applying pseudo-indents to lines with < 4 measures

% Function to analyze marks and identify lines that need pseudo-indents
getShortLinePositions = #(define-music-function (marks) (ly:music?)
   "Analyze marks and identify positions where pseudo-indents should be applied"
   (let ((all-break-positions '())
         (current-position 0)
         (current-section-measures 0)
         (short-line-positions '())
         (total-measures 0))
     
     ;; First, analyze the marks to get all break positions (same logic as autoSectionAndFourMeasureBreaks)
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
     
     ;; Process all elements
     (if (eq? (ly:music-property marks 'name) 'SequentialMusic)
         (for-each process-element (ly:music-property marks 'elements))
         (process-element marks))
     
     ;; Add final section break and four-measure breaks
     (set! all-break-positions (append all-break-positions (list current-position)))
     (let ((final-section-four-measure-breaks '()))
       (do ((measure 4 (+ measure 4)))
           ((> measure current-section-measures))
         (set! final-section-four-measure-breaks (append final-section-four-measure-breaks (list (+ current-position measure)))))
       (set! all-break-positions (append all-break-positions final-section-four-measure-breaks)))
     
     ;; Remove duplicates and sort
     (set! all-break-positions (sort (delete-duplicates all-break-positions) <))
     
     ;; Now analyze which lines will be shorter than 4 measures
     ;; First, check lines between breaks - we apply pseudo-indents at the START of short lines
     (let ((previous-pos 0))
       (for-each (lambda (current-pos)
                   (let ((line-length (- current-pos previous-pos)))
                     ;; If line length is less than 4 measures, mark for pseudo-indent at the START of this line
                     (if (and (< line-length 4) (> current-pos 0))
                         (set! short-line-positions (append short-line-positions (list previous-pos)))))
                   (set! previous-pos current-pos))
                 all-break-positions))
     
     ;; Also check the final line from the last break to the end of the score
     ;; We need to get the total length of the marks to determine the final line length
     (define (count-total-measures element)
       (cond
        ((eq? (ly:music-property element 'name) 'SkipEvent)
         (let* ((duration (ly:music-property element 'duration))
                (measures (if duration (ly:duration-scale duration) 1)))
           (set! total-measures (+ total-measures measures))))))
     
     (if (eq? (ly:music-property marks 'name) 'SequentialMusic)
         (for-each count-total-measures (ly:music-property marks 'elements))
         (count-total-measures marks))
     
     ;; Check if the final line (from last break to end) is shorter than 4 measures
     (let ((last-break-pos (if (null? all-break-positions) 0 (list-ref all-break-positions (- (length all-break-positions) 1)))))
       (let ((final-line-length (- total-measures last-break-pos)))
         (if (< final-line-length 4)
             (set! short-line-positions (append short-line-positions (list last-break-pos))))))
     
     ;; Print the analysis results
     (ly:message "=== PSEUDO-INDENT ANALYSIS ===")
     (ly:message "All break positions: ~a" all-break-positions)
     (ly:message "Total measures in score: ~a" total-measures)
     (ly:message "Short line positions (need pseudo-indents): ~a" short-line-positions)
     (ly:message "Lines that need pseudo-indents (applied at start of line):")
     (for-each (lambda (pos)
                 (if (= pos (if (null? all-break-positions) 0 (list-ref all-break-positions (- (length all-break-positions) 1))))
                     (ly:message "  - Final line starting at measure ~a (from last break to end)" pos)
                     (ly:message "  - Line starting at measure ~a" pos)))
               short-line-positions)
     (ly:message "================================")
     
     marks))

% Function to create pseudo-indent music at specific positions
createPseudoIndentAtPosition = #(define-music-function (position) (number?)
   "Create pseudo-indent at a specific measure position"
   #{ \pseudoIndents 0 44 #})

% Function to automatically generate pseudo-indents at all short line positions
autoPseudoIndents = #(define-music-function (marks) (ly:music?)
   "Automatically generate pseudo-indents at all short line positions"
   (let ((all-break-positions '())
         (current-position 0)
         (current-section-measures 0)
         (total-measures 0)
         (short-line-positions '())
         (result '()))
     
     ;; First, analyze the marks to get all break positions (same logic as before)
     (define (process-element element)
       (cond
        ((eq? (ly:music-property element 'name) 'AdHocMarkEvent)
         (set! all-break-positions (append all-break-positions (list current-position)))
         (let ((section-four-measure-breaks '()))
           (do ((measure 4 (+ measure 4)))
               ((> measure current-section-measures))
             (set! section-four-measure-breaks (append section-four-measure-breaks (list (+ current-position measure)))))
           (set! all-break-positions (append all-break-positions section-four-measure-breaks)))
         (set! current-position (+ current-position current-section-measures))
         (set! current-section-measures 0))
        
        ((eq? (ly:music-property element 'name) 'SkipEvent)
         (let* ((duration (ly:music-property element 'duration))
                (measures (if duration (ly:duration-scale duration) 1)))
           (set! current-section-measures (+ current-section-measures measures))))))
     
     (if (eq? (ly:music-property marks 'name) 'SequentialMusic)
         (for-each process-element (ly:music-property marks 'elements))
         (process-element marks))
     
     (set! all-break-positions (append all-break-positions (list current-position)))
     (let ((final-section-four-measure-breaks '()))
       (do ((measure 4 (+ measure 4)))
           ((> measure current-section-measures))
         (set! final-section-four-measure-breaks (append final-section-four-measure-breaks (list (+ current-position measure)))))
       (set! all-break-positions (append all-break-positions final-section-four-measure-breaks)))
     
     (set! all-break-positions (sort (delete-duplicates all-break-positions) <))
     
     ;; Count total measures
     (define (count-total-measures element)
       (cond
        ((eq? (ly:music-property element 'name) 'SkipEvent)
         (let* ((duration (ly:music-property element 'duration))
                (measures (if duration (ly:duration-scale duration) 1)))
           (set! total-measures (+ total-measures measures))))))
     
     (if (eq? (ly:music-property marks 'name) 'SequentialMusic)
         (for-each count-total-measures (ly:music-property marks 'elements))
         (count-total-measures marks))
     
     ;; Find short line positions - we need to apply pseudo-indents at the START of short lines
     (let ((previous-pos 0))
       (for-each (lambda (current-pos)
                   (let ((line-length (- current-pos previous-pos)))
                     ;; If line length is less than 4 measures, apply pseudo-indent at the START of this line
                     (if (and (< line-length 4) (> current-pos 0))
                         (set! short-line-positions (append short-line-positions (list previous-pos)))))
                   (set! previous-pos current-pos))
                 all-break-positions))
     
     ;; Check the final line - if it's short, apply pseudo-indent at the start of that line
     (let ((last-break-pos (if (null? all-break-positions) 0 (list-ref all-break-positions (- (length all-break-positions) 1)))))
       (let ((final-line-length (- total-measures last-break-pos)))
         (if (< final-line-length 4)
             (set! short-line-positions (append short-line-positions (list last-break-pos))))))
     
     ;; Generate pseudo-indent music at each short line position
     (ly:message "=== APPLYING PSEUDO-INDENTS ===")
     (ly:message "Short line positions to apply: ~a" short-line-positions)
     (for-each (lambda (pos)
                 (ly:message "Applying pseudo-indent at position ~a" pos)
                 (set! result (append result (list #{ \pseudoIndents 0 44 #}))))
               short-line-positions)
     (ly:message "Applied ~a pseudo-indents total" (length short-line-positions))
     (ly:message "================================")
     
     ;; Return the result as sequential music
     (if (null? result)
         (make-music 'SequentialMusic 'elements '())
         (make-music 'SequentialMusic 'elements result))))

% Combined function that generates breaks AND applies pseudo-indents at the right positions
autoBreaksAndPseudoIndents = #(define-music-function (marks) (ly:music?)
   "Generate breaks and automatically apply pseudo-indents to short lines"
   (let ((all-break-positions '())
         (current-position 0)
         (current-section-measures 0)
         (total-measures 0)
         (short-line-positions '())
         (result '())
         (previous-pos 0))
     
     ;; First, analyze the marks to get all break positions and short line positions
     (define (process-element element)
       (cond
        ((eq? (ly:music-property element 'name) 'AdHocMarkEvent)
         (set! all-break-positions (append all-break-positions (list current-position)))
         (let ((section-four-measure-breaks '()))
           (do ((measure 4 (+ measure 4)))
               ((> measure current-section-measures))
             (set! section-four-measure-breaks (append section-four-measure-breaks (list (+ current-position measure)))))
           (set! all-break-positions (append all-break-positions section-four-measure-breaks)))
         (set! current-position (+ current-position current-section-measures))
         (set! current-section-measures 0))
        
        ((eq? (ly:music-property element 'name) 'SkipEvent)
         (let* ((duration (ly:music-property element 'duration))
                (measures (if duration (ly:duration-scale duration) 1)))
           (set! current-section-measures (+ current-section-measures measures))))))
     
     (if (eq? (ly:music-property marks 'name) 'SequentialMusic)
         (for-each process-element (ly:music-property marks 'elements))
         (process-element marks))
     
     (set! all-break-positions (append all-break-positions (list current-position)))
     (let ((final-section-four-measure-breaks '()))
       (do ((measure 4 (+ measure 4)))
           ((> measure current-section-measures))
         (set! final-section-four-measure-breaks (append final-section-four-measure-breaks (list (+ current-position measure)))))
       (set! all-break-positions (append all-break-positions final-section-four-measure-breaks)))
     
     (set! all-break-positions (sort (delete-duplicates all-break-positions) <))
     
     ;; Count total measures
     (define (count-total-measures element)
       (cond
        ((eq? (ly:music-property element 'name) 'SkipEvent)
         (let* ((duration (ly:music-property element 'duration))
                (measures (if duration (ly:duration-scale duration) 1)))
           (set! total-measures (+ total-measures measures))))))
     
     (if (eq? (ly:music-property marks 'name) 'SequentialMusic)
         (for-each count-total-measures (ly:music-property marks 'elements))
         (count-total-measures marks))
     
     ;; Find short line positions
     (let ((prev-pos 0))
       (for-each (lambda (current-pos)
                   (let ((line-length (- current-pos prev-pos)))
                     (if (and (< line-length 4) (> current-pos 0))
                         (set! short-line-positions (append short-line-positions (list prev-pos)))))
                   (set! prev-pos current-pos))
                 all-break-positions))
     
     (let ((last-break-pos (if (null? all-break-positions) 0 (list-ref all-break-positions (- (length all-break-positions) 1)))))
       (let ((final-line-length (- total-measures last-break-pos)))
         (if (< final-line-length 4)
             (set! short-line-positions (append short-line-positions (list last-break-pos))))))
     
     (ly:message "=== COMBINED BREAKS AND PSEUDO-INDENTS ===")
     (ly:message "Break positions: ~a" all-break-positions)
     (ly:message "Short line positions: ~a" short-line-positions)
     
     ;; Generate breaks and pseudo-indents in the right order
     (for-each (lambda (pos)
                 ;; Add skip for measures between previous position and current position
                 (let ((measures-to-skip (- pos previous-pos)))
                   (if (> measures-to-skip 0)
                       (set! result (append result (list (make-music 'SkipEvent 'duration (ly:make-duration 0 0 measures-to-skip)))))))
                 
                 ;; Check if this position needs a pseudo-indent (is start of a short line)
                 (if (member pos short-line-positions)
                     (begin
                       (ly:message "Adding pseudo-indent at position ~a" pos)
                       (set! result (append result (list #{ \pseudoIndents 0 44 #})))))
                 
                 ;; Add break
                 (set! result (append result (list (make-music 'LineBreakEvent 'break-permission 'force))))
                 (set! previous-pos pos))
               all-break-positions)
     
     (ly:message "================================")
     
     ;; Create the result as sequential music
     (make-music 'SequentialMusic 'elements result)))