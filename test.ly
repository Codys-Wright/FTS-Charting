

% Set the font to Bravura for SMuFL support
ekmFont = "Bravura"

\include "utils/paper-setup.ly"
\include "utils/capsules/measure-position-detection.ly"
\include "utils/capsules/capsule-utils.ly"
\include "utils/capsules/new-capsule-utils.ly"
\include "utils/rehearsal-marks/rehearsal-mark-positioning.ly"
\include "utils/breaks/auto-four-measure-breaks.ly"
\include "utils/breaks/pseudo-indents.ly"
\include "utils/breaks/auto-pseudo-indents.ly"
\include "utils/layout/spacing.ly"
\include "utils/layout/score-layout.ly"
\include "utils/music/rehearsal-marks.ly"

\include "utils/header-template.ly"








\version "2.24.0"


chs = \transpose c' c' {
 s1*48 
}



testPositions = \getShortLinePositions \marks

% Combined function that does both breaks and pseudo-indents in the right order
combinedBreaks = \autoBreaksAndPseudoIndents \marks

% Global settings including key signature
global = { \time 4/4 \key f \major }


 chordProgression =  \chordmode {
        | % 1
        s4 s4 s4 s4 | % 2
        s4 s4 s4 s4 | % 3
        g4:m9 s4 s4 s8. c16:13.11 | % 4
        s4 s4 c8.:13.11 des16:13.11 s8 d8:13.11 | % 5
        es4:13.11 s4 s4 d4:13.11 | % 6
        s4 s4 des4:13.11 s4 | % 7
        c4:13.11 s4 s4 s4 | % 8
        s4 c4:13.11 s8. es16:13.11 s8 as8:maj7 | % 9
        s4. des8:5 s2 | \barNumberCheck #10
        c16:13.11 s16 s8 s8 s16 s16 s4 ges4:dim7 | % 11
        g4:m9 s4 s4 s4 | % 12 | % 7
        c4:13.11 s4 s4 s4 | % 8
        s4 c4:13.11 s8. es16:13.11 s8 as8:maj7 | % 9
        s4. des8:5 s2 | \barNumberCheck #10
        c16:13.11 s16 s8 s8 s16 s16 s4 ges4:dim7 | % 11
        g4:m9 s4 s4 s4 | % 12
        c4:13.11 s4 s4 s4 | % 13
        f4:5.9 s4 s4 d4:7 | % 14
        s4 s4 s4 ges4:dim5 | % 15
        g4:m9 s4 s4 s4 | % 1
        c4:13.11 s4 s4 s4 | % 29
        f4:maj9 s4 s4 s4 | \barNumberCheck #30
        d4:7 s4 s4 ges4:dim5 | % 31
        g4:m9 s4 s4 s4 | % 32
        des4:dim5/+c s4 s4 s4 | % 33
        f4:maj9 s4 s4 s4 | % 34
        d4:7 s4 s4 ges4:dim5 | % 35
        g4:m9 s4 s4 s4 | % 36
        c4:13.11 s4 s4 s4 | % 37
        f4:5.9 s4 s4 s4 | % 38
        d4:7 s4 s4 ges4:dim5 | % 39
        g4:m9 s4 s4 s4 | \barNumberCheck #40
        c4:13.11 s4 s4 s4 | % 41
        f4:5.9 s4 s4 s4 | % 42
        d4:7 s4 s4 ges4:dim5 | % 43
        g4:m9 s4 s4 s4 | % 44
        c4:13.11 s4 s4 s4 | % 45
        f4:5.9 s4 s4 s4 | % 46
        d4:7 s4 s4 ges4:dim5 | % 47
        g4:m9 s4 s4 s4 | % 48
        c4:13.11 s4 s4 s4 | % 49
        f4:maj9 s4 s4 s4 | \barNumberCheck #50
        d8:7 s8 s8 s8 s8 ges8:dim7 s8. g16:m9 | % 51
        g4:m9 s4 s4 s4 | % 52
        c4:13.11 s4 s4 s4 | % 53
        f4:maj9 s4 s4 s8. d16:7 | % 54
        s4 s4 ges8.:dim5 s16 s8 g8:m9 | % 55
        g4:m9 s4 s4 s4 | % 56
        c4:13.11 s4 s4 s8. f16:maj9 | % 57
        s4 s4 s4 s4 | % 58
        d4:7 s4 s4 ges4:dim5 \bar "||"
        bes4:m9 s4 s4 s8. es16:13.11 | \barNumberCheck #60
        s4 s4 s4 s8. c16:m7 | % 61
        s4 s4 s4 s8. f16:m7 | % 62
        s4 s4 s4 s8. bes16:m9 | % 63
        bes4:m9 s4 s4 s8. es16:13.11 | % 64
        s4 s4 es8.:13.11 d16:13.11 s8 des8:13.11 | % 65
        c4:13.11 s4 s4 s4 | % 66
        c4:13.11 s4 s16 s16 s16 s16 s8. g16:m9 \bar "||"
        g4:m9 s4 s4 s8. c16:13.11 | % 68
        s4 s4 s4 s8. a16:m7 | % 69
        s4 s4 s4 s4 | \barNumberCheck #70
        d4:m7 c4:5 d4:m7 s16 ges8:dim7 g16:m9 | % 71
        g4:m9 s4 s4 s8. c16:13.11 | % 72
        s4 s4 s4. des8:5 | % 73
        s4. des8:6 s4. des8:7 | % 74
        s4. des8:5 s4 s4 | % 75
        s1 | % 76
        s4 s4 s8 s8 s8 ges8:maj9 | % 77
        s4. fes8:maj7 s4. es8:7 | % 78
        s4 es4:7 es8:7 s8 s8 as8:m9 | % 79
        as4:m9 s4 s4. f8:dim7 | \barNumberCheck #80
        s4. bes8:7 s2 | % 81
        es4.:m7 f8:dim5m7 s4. ges8:maj9 | % 82
        s4 s4 s4 s4 | % 83
        des4:11 s4 s4 s4 | % 84
        s4 s4 s4 s4 | % 85
        es4:11 s4 s4 s4 | % 86
        s4 s4 d8:11 s4 des8:11 | % 87
        des4:11 s4 s4 s4 | % 88
        s4 s4 s4 s4 | % 89
        es4:11 s4 s4 s4 | \barNumberCheck #90
        s4 s4 s4 s4 | % 91
        des4:11 s4 s4 s4 | % 92
        s4 s4 des8.:11 d16:11 s8 es8:11 | % 93
        s4 s4 s4 s4 | % 94
        s4 s4 es8.:11 d16:11 s8 des8:11 | % 95
        des4:11 s4 s4 s4 | % 96
        s4 s4 s4. s8 \bar "|."
        }

% Clean rhythm patterns extracted from High and Dry rhythm.ly
rhythmContent = {
  % Measures 1-2: Basic quarter notes (slash notes)
  \once \override NoteHead.style = #'slash bes4 \once \override NoteHead.style = #'slash bes4 \once \override NoteHead.style = #'slash bes4 \once \override NoteHead.style = #'slash bes4 | 
  \once \override NoteHead.style = #'slash bes4 \once \override NoteHead.style = #'slash bes4 \once \override NoteHead.style = #'slash bes4 \once \override NoteHead.style = #'slash bes4 |
  
  % Measure 3: Rest pattern
  s4 s4 s4 \once \override NoteHead.style = #'slash bes8. \once \override NoteHead.style = #'slash bes16 |
  
  % Measure 4: Rest pattern with dotted sixteenth
  s4 s4 \once \override NoteHead.style = #'slash bes8. \once \override NoteHead.style = #'slash bes16~ \once \override NoteHead.style = #'slash bes8 \once \override NoteHead.style = #'slash bes8 |
  
  % Measures 5-6: Rest patterns
  s4 s4 s4 s4 | s4 s4 s4 s4 |
  
  % Measure 7: Whole rest
  s1 |
  
  % Measure 8: Eighth note pattern with accents
  s4 s4 \once \override NoteHead.style = #'slash bes8.-> \once \override NoteHead.style = #'slash bes16~-> \once \override NoteHead.style = #'slash bes8 \once \override NoteHead.style = #'slash bes8~-> |
  
  % Measure 9: Fill pattern
  \once \override NoteHead.style = #'slash bes4. \once \override NoteHead.style = #'slash bes8~ \once \override NoteHead.style = #'slash bes2 |
  
  % Measure 10: Sixteenth note fill
  \once \override NoteHead.style = #'slash bes16-> \once \override NoteHead.style = #'slash bes16-> r8 r8 \once \override NoteHead.style = #'slash bes16-> \once \override NoteHead.style = #'slash bes16-> r4 b4 |
  
  % Measures 11-13: Whole rests
  s1 | s1 | s1 |
  
  % Measure 14: Bass line
  g4 fis4 g4 bes4 |
  
  % Measures 15-17: Whole rests
  s1 | s1 | s1 |
  
  % Measure 18: Bass line
  g4 fis4 g4 bes4 |
  
  % Measures 19-21: Whole rests
  s1 | s1 | s1 |
  
  % Measure 22: Bass line
  g4 fis4 g4 bes4 |
  
  % Measures 23-25: Whole rests
  s1 | s1 | s1 |
  
  % Measure 26: Eighth note with rest
  e8-> s8 s4 s2 |
  
  % Measures 27-29: Whole rests
  s1 | s1 | s1 |
  
  % Measure 30: Bass line
  g4 fis4 g4 a4 |
  
  % Measures 31-33: Whole rests
  s1 | s1 | s1 |
  
  % Measure 34: Bass line with accent
  g4 fis4 g4 a4 |
  
  % Measures 35-37: Whole rests
  s1 | s1 | s1 |
  
  % Measure 38: Bass line
  g4 fis4 g4 bes4 |
  
  % Measures 39-41: Whole rests
  s1 | s1 | s1 |
  
  % Measure 42: Bass line
  g4 fis4 g4 bes4 |
  
  % Measures 43-45: Whole rests
  s1 | s1 | s1 |
  
  % Measure 46: Bass line
  g4 fis4 g4 bes4 |
  
  % Measures 47-49: Whole rests
  s1 | s1 | s1 |
  
  % Measure 50: Accented eighth notes (slash notes)
  \once \override NoteHead.style = #'slash bes8-> \once \override NoteHead.style = #'slash bes8-> \once \override NoteHead.style = #'slash bes8-> \once \override NoteHead.style = #'slash bes8-> \once \override NoteHead.style = #'slash bes8-> \once \override NoteHead.style = #'slash bes8-> s8. \once \override NoteHead.style = #'slash bes16 |
  
  % Measures 51-52: Whole rests
  s1 | s1 |
  
  % Measure 53: Rest with dotted sixteenth
  s2 s4 \once \override NoteHead.style = #'slash bes8. g16~ g8 |
  
  % Measure 54: Bass line with accents
  g4-> fis4 g8.-> a16~-> a8 a8-> |
  
  % Measure 55: Whole rest
  s1 |
  
  % Measure 56: Rest with dotted sixteenth
  s2 s4 \once \override NoteHead.style = #'slash bes8. \once \override NoteHead.style = #'slash bes16-> |
  
  % Measure 57: Whole rest
  s1 |
  
  % Measure 58: Bass line with accent
  g4 fis4 g4 bes4 |
  
  % Measure 59: Rest with dotted sixteenth
  s2 s4 \once \override NoteHead.style = #'slash bes8. \once \override NoteHead.style = #'slash bes16 |
  
  % Measure 60: Rest with dotted sixteenth
  s2 s4 \once \override NoteHead.style = #'slash bes8. \once \override NoteHead.style = #'slash bes16 |
  
  % Measure 61: Rest with dotted sixteenth
  s2 s4 \once \override NoteHead.style = #'slash bes8. \once \override NoteHead.style = #'slash bes16 |
  
  % Measure 62: Rest with accented notes
  s2 \once \override NoteHead.style = #'slash bes4-> \once \override NoteHead.style = #'slash bes8.-> \once \override NoteHead.style = #'slash bes16-> |
  
  % Measure 63: Rest with dotted sixteenth
  s2 s4 \once \override NoteHead.style = #'slash bes8. b16 |
  
  % Measure 64: Rest with dotted sixteenth
  s4 s4 \once \override NoteHead.style = #'slash bes8. \once \override NoteHead.style = #'slash bes16~ \once \override NoteHead.style = #'slash bes8 \once \override NoteHead.style = #'slash bes8-> |
  
  % Measure 65: Whole rest
  s1 |
  
  % Measure 66: Rest with sixteenth notes
  s2 \once \override NoteHead.style = #'slash bes16-> \once \override NoteHead.style = #'slash bes16-> \once \override NoteHead.style = #'slash bes16-> s16 s8. \once \override NoteHead.style = #'slash bes16-> |
  
  % Measure 67: Rest with dotted sixteenth
  s4 s4 s4 \once \override NoteHead.style = #'slash bes8. b16 |
  
  % Measure 68: Rest with dotted sixteenth
  s2 s4 \once \override NoteHead.style = #'slash bes8. b16 |
  
  % Measure 69: Whole rest
  s1 |
  
  % Measure 70: Bass line with accents
  f4-> g4-> a4-> s16 c8-> c16-> |
  
  % Measure 71: Rest with dotted sixteenth
  s2 s4 \once \override NoteHead.style = #'slash bes8. b16 |
  
  % Measure 72: Rest with dotted eighth
  s2 \once \override NoteHead.style = #'slash bes4. \once \override NoteHead.style = #'slash bes8~-> |
  
  % Measure 73: Dotted eighth pattern
  \once \override NoteHead.style = #'slash bes4. \once \override NoteHead.style = #'slash bes8~-> \once \override NoteHead.style = #'slash bes4. \once \override NoteHead.style = #'slash bes8~-> |
  
  % Measure 74: Dotted eighth with rest
  \once \override NoteHead.style = #'slash bes4. \once \override NoteHead.style = #'slash bes8 s2 |
  
  % Measure 75: Whole rest
  s1 |
  
  % Measure 76: Rest with bass notes
  s4 bes4-> ces8-> des8-> s8 \once \override NoteHead.style = #'slash bes8~-> \once \override NoteHead.style = #'slash bes8 |
  
  % Measure 77: Dotted eighth pattern
  \once \override NoteHead.style = #'slash bes4. \once \override NoteHead.style = #'slash bes8~-> \once \override NoteHead.style = #'slash bes4. \once \override NoteHead.style = #'slash bes8~-> |
  
  % Measure 78: Bass line with accents
  \once \override NoteHead.style = #'slash bes4 b4-> f8-> ges8-> s8 \once \override NoteHead.style = #'slash bes8~ \once \override NoteHead.style = #'slash bes8 |
  
  % Measure 79: Bass line
  \once \override NoteHead.style = #'slash bes4 \once \override NoteHead.style = #'slash bes4 \once \override NoteHead.style = #'slash bes4. \once \override NoteHead.style = #'slash bes8~-> \once \override NoteHead.style = #'slash bes8 |
  
  % Measure 80: Dotted eighth with rest
  \once \override NoteHead.style = #'slash bes4. \once \override NoteHead.style = #'slash bes8~-> \once \override NoteHead.style = #'slash bes8 es2 |
  
  % Measure 81: Dotted eighth pattern
  \once \override NoteHead.style = #'slash bes4. \once \override NoteHead.style = #'slash bes8~ \once \override NoteHead.style = #'slash bes4. \once \override NoteHead.style = #'slash bes8~ |
  
  % Measure 82: Bass line with rest
  \once \override NoteHead.style = #'slash bes4 s4 s2 |
  
  % Measure 83: Bass line
  \once \override NoteHead.style = #'slash bes4 \once \override NoteHead.style = #'slash bes4 \once \override NoteHead.style = #'slash bes4 \once \override NoteHead.style = #'slash bes4 |
  
  % Measure 84: Bass line
  \once \override NoteHead.style = #'slash bes4 \once \override NoteHead.style = #'slash bes4 \once \override NoteHead.style = #'slash bes4 \once \override NoteHead.style = #'slash bes4 |
  
  % Measure 85: Whole rest
  s1 |
  
  % Measure 86: Rest with bass pattern
  s2 \once \override NoteHead.style = #'slash bes8~-> \once \override NoteHead.style = #'slash bes8 es4 |
  
  % Measures 87-90: Whole rests
  s1 | s1 | s1 | s1 |
  
  % Measure 91: Whole rest
  s1 |
  
  % Measure 92: Rest with dotted sixteenth
  s2 \once \override NoteHead.style = #'slash bes8.-> \once \override NoteHead.style = #'slash bes16~-> \once \override NoteHead.style = #'slash bes8 \once \override NoteHead.style = #'slash bes8-> |
  
  % Measure 93: Whole rest
  s1 |
  
  % Measure 94: Rest with dotted sixteenth
  s2 \once \override NoteHead.style = #'slash bes8.-> \once \override NoteHead.style = #'slash bes16~-> \once \override NoteHead.style = #'slash bes8 \once \override NoteHead.style = #'slash bes8-> |
  
  % Measure 95: Whole rest
  s1 |
  
  % Measure 96: Rest with dotted eighth
  s2 \once \override NoteHead.style = #'slash bes4. \once \override NoteHead.style = #'slash bes8 |
}

% Function to add slash marks to empty measures
#(define (add-slash-to-rests music)
   "Add slash marks to whole rests"
   (let* ((result '()))
     (for-each 
      (lambda (event)
        (if (and (ly:music? event)
                 (eq? (ly:music-property event 'name) 'RestEvent)
                 (= 1 (ly:moment-main-numerator (ly:music-property event 'duration))))
            ;; Add slash marks for whole rests
            (set! result (append result 
              (list (make-music 'NoteEvent
                    'duration (ly:music-property event 'duration)
                    'pitch (ly:make-pitch 0 1) ; b note
                    'articulations (list (make-music 'OverrideProperty
                                           'symbol 'NoteHead
                                           'grob-value 'stencil
                                           'grob-property-path '(style)
                                           'grob-property-value 'slash))))))
            ;; Keep other events as-is
            (set! result (append result (list event)))))
      (ly:music-property music 'elements))
     (make-music 'SequentialMusic 'elements result)))

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
    \rhythmContent
    \combinedBreaks
      
     
    >>
  }
  >>
}




