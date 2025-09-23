% Slash Beats Music Content

% Define the music content
slashBeatsContent = {
  \repeat unfold 23 {
    \hide Stem
    \override NoteHead.style = #'slash
    b4 b4 b4 b4 |
  }
}

% Apply pseudoIndents only to the first line
slashBeats = {
  % First line - make it narrower with right-indent
  \slashBeatsContent
  
  % Remaining lines - normal width
  \repeat unfold 3 {
    \slashBeatsContent
  }
}
