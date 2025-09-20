% Font configuration for LilyPond charting workflow
% This file contains font setup and configuration

% Set up custom fonts
#(ly:font-config-add-directory "fonts/San-Francisco-Pro-Fonts-master/")
#(ly:font-config-add-directory "fonts/musescore/fonts/leeland/")
#(ly:font-config-add-directory "fonts/musescore/fonts/musejazz/")
#(ly:font-config-add-directory "/nix/store/pa319y41n4hdkdv8p48mzxqpz5nkazzj-openlilypond-font-lilyjazz-8fa7d554/share/lilypond/2.24.4/fonts/otf/")

% Set global staff size
#(set-global-staff-size 18)
