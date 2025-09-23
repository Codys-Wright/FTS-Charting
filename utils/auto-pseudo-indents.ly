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
     ;; First, check lines between breaks (skip position 0 as it's the start, not a line end)
     (let ((previous-pos 0))
       (for-each (lambda (current-pos)
                   (let ((line-length (- current-pos previous-pos)))
                     ;; If line length is less than 4 measures, mark for pseudo-indent
                     ;; But skip position 0 as it's not a real line end
                     (if (and (< line-length 4) (> current-pos 0))
                         (set! short-line-positions (append short-line-positions (list current-pos)))))
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
             (set! short-line-positions (append short-line-positions (list total-measures))))))
     
     ;; Print the analysis results
     (ly:message "=== PSEUDO-INDENT ANALYSIS ===")
     (ly:message "All break positions: ~a" all-break-positions)
     (ly:message "Total measures in score: ~a" total-measures)
     (ly:message "Short line positions (need pseudo-indents): ~a" short-line-positions)
     (ly:message "Lines that need pseudo-indents:")
     (for-each (lambda (pos)
                 (if (= pos total-measures)
                     (ly:message "  - Final line ending at measure ~a (from last break to end)" pos)
                     (ly:message "  - Line ending at measure ~a" pos)))
               short-line-positions)
     (ly:message "================================")
     
     marks))

% Function to create pseudo-indent music at specific positions
createPseudoIndentAtPosition = #(define-music-function (position) (number?)
   "Create pseudo-indent at a specific measure position"
   #{ \pseudoIndents 0 44 #})