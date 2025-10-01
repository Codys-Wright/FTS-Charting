% Font configuration for LilyPond charting workflow
% This file contains font setup and configuration

% Set up custom fonts
#(ly:font-config-add-directory "fonts/San-Francisco-Pro-Fonts-master/")
#(ly:font-config-add-directory "fonts/musescore/fonts/musejazz/")
#(ly:font-config-add-directory "/nix/store/pa319y41n4hdkdv8p48mzxqpz5nkazzj-openlilypond-font-lilyjazz-8fa7d554/share/lilypond/2.24.4/fonts/otf/")

% Global font settings
\paper {
  #(define fonts
    (set-global-fonts
     #:roman "LilyJazz Text"
     #:sans "LilyJazz Text"
     #:typewriter "LilyJazz Text"
    ))
}
