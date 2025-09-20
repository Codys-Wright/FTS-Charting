\paper { tagline = ##f }

%% http://lsr.di.unimi.it/LSR/Item?id=1092
%% Add by P.P.Schneider on Apr. 2019.

%%%% Defs:
#(define-public (make-capsule-stencil lgth radius thickness fill)
  "Make an capsule from four Bézier curves and two lines, of radius @var{radius},
   length @var{lgth}, and thickness @var{thickness} with fill
   defined by @code{fill}."
    (let*
        ((k 0)
         (top-offset 2)
         (side-offset 2)
         (r-max radius)
         (r-min (- r-max))
         (l-max (- lgth r-max)) 
         (l-min (- l-max))
         (command-list `(moveto ,lgth 0                    ; Start at bottom-right corner
                         lineto ,lgth ,(- r-max top-offset) ; Go up to top-right corner (inset down)
                         curveto ,lgth ,r-max ,(- lgth side-offset) ,r-max ,(- lgth side-offset) ,r-max ; Curve to top-right corner
                         lineto ,(+ (- lgth) side-offset) ,r-max ; Go left to top-left corner (inset right)
                         curveto ,(- lgth) ,r-max ,(- lgth) ,(- r-max top-offset) ,(- lgth) ,(- r-max top-offset) ; Curve to top-left corner
                         lineto ,(- lgth) ,(+ r-min top-offset) ; Go down to bottom-left corner (inset up)
                         curveto ,(- lgth) ,r-min ,(+ (- lgth) side-offset) ,r-min ,(+ (- lgth) side-offset) ,r-min ; Curve to bottom-left corner
                         lineto ,(- lgth side-offset) ,r-min ; Go right to bottom-right corner (inset left)
                         curveto ,lgth ,r-min ,lgth ,(+ r-min top-offset) ,lgth ,(+ r-min top-offset) ; Curve to bottom-right corner
                         lineto ,lgth 0                     ; Go up back to start
                         closepath)))                       ; Close the rectangle
      ;; after Harm:
      (make-path-stencil
       command-list
       thickness 1 1 fill)))

#(define-public (capsule-stencil stencil thickness x-padding y-padding)
  "Add a capsule around @code{stencil}, padded by the padding pair,
   producing a var stencil."
  (let* ((x-ext (ly:stencil-extent stencil X))
         (y-ext (ly:stencil-extent stencil Y))
         (x-length (+ (interval-length x-ext) x-padding thickness))
         (y-length (+ (interval-length y-ext) y-padding thickness))
         (x-radius (* 0.5 x-length) )
         (y-radius (* 0.52 y-length) )
         (capsule (make-capsule-stencil x-radius y-radius thickness #f)))
    (ly:stencil-add
     stencil
     (ly:stencil-translate capsule
                           (cons
                            (interval-center x-ext)
                            (interval-center y-ext))))))

#(define-markup-command (capsule layout props arg)
  (markup?)
  #:category graphic
  #:properties ((thickness 1)
                (font-size 0)
                (x-padding 1)
                (y-padding .4))
"
@cindex drawing capsule around text

Draw a capsule around @var{arg}. Use @code{thickness},
@code{x-padding}, @code{x-padding} and @code{font-size} properties to determine
line thickness and padding around the markup.

@lilypond[verbatim,quote]
\\markup {
  \\capsule {
    Hi
  }
}
@end lilypond"

  (let ((th (* (ly:output-def-lookup layout 'line-thickness)
               thickness))
        (pad-x (* (magstep font-size) x-padding))
        (pad-y (* (magstep font-size) y-padding))
        (m (interpret-markup layout props arg)))
    (capsule-stencil m th pad-x pad-y)))

%%%% Defs end %%%%%%%%%%


%%%% Tests:
\markup { \italic "Default:" \capsule "Hi" }
\markuplist {
  \vspace #1 \italic "With some overrides:" \vspace #.5
  \override #'(font-size . 3)
  \override #'(y-padding . 2)
  \override #'(x-padding . 2)
  \override #'(thickness . 5)
  \with-color #(rgb-color 1 0.3 0.2) 
  \capsule "This is an encapsulated text"
  
  \vspace #2 \italic "More experiments:" \vspace #.5
  
  % Different colors and sizes
  \override #'(font-size . 2)
  \override #'(thickness . 3)
  \with-color #(rgb-color 0.2 0.8 0.4)
  \capsule "Green capsule"
  
  \vspace #1
  \override #'(font-size . 1)
  \override #'(thickness . 2)
  \with-color #(rgb-color 0.1 0.3 0.9)
  \capsule "Blue capsule"
  
  \vspace #1
  % Very thin with lots of padding
  \override #'(font-size . 4)
  \override #'(y-padding . 3)
  \override #'(x-padding . 4)
  \override #'(thickness . 1)
  \with-color #(rgb-color 0.8 0.2 0.8)
  \capsule "Purple with lots of padding"
  
  \vspace #1
  % Thick border, minimal padding
  \override #'(font-size . 2)
  \override #'(y-padding . 0.2)
  \override #'(x-padding . 0.5)
  \override #'(thickness . 8)
  \with-color #(rgb-color 0.9 0.6 0.1)
  \capsule "Thick orange border"
  
  \vspace #2 \italic "Two-line text:" \vspace #.5
  
  % Two-line capsule
  \override #'(font-size . 2)
  \override #'(y-padding . 2.5)
  \override #'(x-padding . 3)
  \override #'(thickness . 3)
  \with-color #(rgb-color 0.2 0.4 0.8)
  \translate #'(15 . 0) \capsule \column { \center-align "SOLOISTIC" \center-align "GTR" }
  
  \vspace #3 \italic "Capsule next to staff:" \vspace #.5
  
  % Staff with capsule to the left
  \left-align \overlay {
    % Capsule positioned to the left
    \translate #'(-8 . 0) 
    \override #'(y-padding . 3)
    \override #'(x-padding . 4)
    \override #'(thickness . 2)
    \capsule \column { \center-align "SOLO" \center-align "GTR" }
    
    % Staff positioned to the right
    \translate #'(2 . 0) \score {
      {
        \time 4/4
        \clef treble
        c'4 d' e' f' | g'4 a' b' c'' |
      }
      \layout {
        indent = 0
        line-width = 60
      }
    }
  }
}
