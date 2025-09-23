% Paper setup for LilyPond charting workflow
% This file contains paper configuration

\paper {
  #(set-paper-size "a4")
  top-margin = 12\mm
  bottom-margin = 12\mm
  left-margin = 16\mm
  right-margin = 16\mm
  indent = 5.00

  % Disable default headers and page numbers
  print-page-number = ##f
  print-first-page-number = ##f
  bookTitleMarkup = ##f
  
  % Add space between header and score
  markup-system-spacing = #'((basic-distance . 25)
                            (minimum-distance . 8)
                            (padding . 4)
                            (stretchability . 6))
  
  % Use mixed fonts
  % Default Emmentaler for music notation, MuseJazz Text for chords and text, SF Pro Display for titles and fitBoxes
  #(define fonts
    (make-pango-font-tree "Leland"
                          "MuseJazz Text"
                          "MuseJazz Text"
                          (/ staff-height pt 20)))
  
}
