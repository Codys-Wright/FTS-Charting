

% Set the font to Bravura for SMuFL support
ekmFont = "Bravura"

% Include esmuflily for SMuFL support
\include "/home/cody/Documents/Development/reference/esmuflily/ly/cosmufl.ily"

\include "utils/paper-setup.ly"
\include "utils/capsules/measure-position-detection.ly"
\include "utils/capsules/capsule-utils.ly"
\include "utils/capsules/new-capsule-utils.ly"
\include "utils/rehearsal-marks/rehearsal-mark-positioning.ly"
\include "utils/breaks/auto-four-measure-breaks.ly"
\include "utils/breaks/pseudo-indents.ly"
\include "utils/breaks/auto-pseudo-indents.ly"
\include "utils/staff/thin-staff-lines.ly"
\include "utils/rehearsal-marks/capsule-markers.ly"
\include "utils/layout/spacing.ly"
\include "utils/music/slash-beats.ly"
\include "utils/music/rehearsal-marks.ly"
\include "utils/music/chord-progressions.ly"

\include "utils/header-template.ly"




% Test the dynamic functions
%  #(ly:message "Dynamic line break detection functions loaded from utility file")

% Test the section measure counting
%  #(ly:message "Testing section measure counting:")




\paper {
  
  % Remove ragged-right to use manual padding control instead
  % ragged-right = ##t
  
  % Display spacing dimensions graphically
  % annotate-spacing = ##t
  
  % Make staff lines thinner (affects only staff lines, not stems/beams)
  % line-thickness = #0.05
  
  % Reduce system-to-system spacing
  % system-system-spacing = #'((basic-distance . 6)
  %                            (minimum-distance . 4)
  %                            (padding . 1)
  %                            (stretchability . 6))
  
  % % Reduce score-to-score spacing  
  % score-system-spacing = #'((basic-distance . 8)
  %                           (minimum-distance . 6)
  %                           (padding . 1)
  %                           (stretchability . 8))
  
}

\layout {
  \context {
    \Score

   
    % Show 4/4 instead of common time
    \override TimeSignature.style = #'numbered
  }
  

}




% ****************************************************************
% ly snippet:
% ****************************************************************
\version "2.24.0"
\header {

}

chs = \transpose c' c' {
 s1*48 
}




% Manual indents removed - now handled automatically by autoBreaksAndPseudoIndents



% Test the combined functionality (breaks and automatic pseudo-indents for short lines)
% First, let's see what positions need pseudo-indents
testPositions = \getShortLinePositions \marks

% Combined function that does both breaks and pseudo-indents in the right order
combinedBreaks = \autoBreaksAndPseudoIndents \marks

% Global settings including key signature
global = { \time 4/4 \key e \major }




\score {
  <<
  \chords { \chs }
  \new ChordNames {
    \chordProgression
  }
  \new Staff \transpose c c' { 
    \global
    <<
    \marks
    \slashBeats
    \combinedBreaks
      
     
    >>
  }
  >>
  \layout {
    \context {
      \Score
        % Enable SMuFL support for Leland font (excluding noteheads for now)
        \ekmSmuflOn #'all
        % Make staff lines thinner using custom stencil
        \override StaffSymbol.stencil = #thin-staff-lines
        \override BarNumber.font-size = -5  % Make bar numbers smaller
        \override BarNumber.Y-offset = #4.5  % Move bar numbers to the right
        \override BarNumber.X-offset = #0.4  % Move bar numbers to the right
      \override RehearsalMark.break-align-symbols = #'(left-edge)
      \override RehearsalMark.self-alignment-X = #RIGHT
      \override RehearsalMark.self-alignment-Y = #UP
      \override RehearsalMark.before-line-breaking = #capsule-marker
        % Position bar numbers to avoid conflict with rehearsal marks
    }
  }
}




