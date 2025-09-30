\version "2.24.0"

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

\include "utils/header-template.ly"



marks = {
 s1*2 |
 \mark \markup "Intro"
 s1*8 |
 \mark \markup "VS 1"
 s1*16 |
 \mark \markup "CH 1"
 s1*8 |
 \mark \markup "VS 2"
 s1*16 |
 \mark \markup "CH 2"
 s1*8 |
 \mark \markup "KEYS"
 s1*8 |
 \mark \markup "GTR"
 s1*8 |
 \mark \markup "CH 3"
 s1*8 |
 \mark \markup "CH 4"
 s1*8 |
 \mark \markup "Outro"
 s1*6 |
}

keys = {
  \key f \major
 s1*2 |
 %Intro
 s1*8 |
 %VS 1
 s1*16 |
 %CH 1
 s1*8 |
 %VS 2
 s1*16 |
 %CH 2
 s1*8 |
 \key aes \major
 %KEYS
 s1*8 |
 %GTR
 \key f \major
 s1*6 |
 \key ges \major
 s1*2
 %CH 3
 s1*8 |
 %CH 4
 s1*8 |
 %Outro
 s1*6 |

}







chs = \transpose c' c' {
 s1*48 
}



testPositions = \getShortLinePositions \marks

% Combined function that does both breaks and pseudo-indents in the right order
combinedBreaks = \autoBreaksAndPseudoIndents \marks

% Global settings including key signature
global = { \time 4/4  }



% 4 slash marks pattern
slashMarks = {
  \once \override NoteHead.style = #'slash 
  \once \override Stem.stencil = ##f
  d4
  \once \override NoteHead.style = #'slash 
  \once \override Stem.stencil = ##f
  d4 
  \once \override NoteHead.style = #'slash 
  \once \override Stem.stencil = ##f
  d4 
  \once \override NoteHead.style = #'slash 
  \once \override Stem.stencil = ##f
  d4
}



vsProgression = \chordmode {
  g1:m9  | c1 | f1 | d2. ges4 |

}

chProgression = \chordmode {
  g1:m9 | c1:sus13 | f1:maj9 | d2.:7  ges4:dim7
}

newChordProgression = \chordmode {
  s1 s1 | \barNumberCheck #3 %Drum Beginning
  \repeat unfold 4 { s1 } | \barNumberCheck #7
  \vsProgression
  \vsProgression
  \vsProgression
  g1:m9 c1 f1:maj7 ges8:dim7 \skip {8 2.} | \barNumberCheck #23
  \chProgression
  \chProgression | \barNumberCheck #31
 \vsProgression
  \vsProgression
  \vsProgression
  g1:m9 | c1 | f1:maj9 | cis2:7 ~8  ges8:dim7 s8. g16:m9 | \barNumberCheck #47
  g1:m9 | c1:sus13 | f1:maj9 | d2:7 ~8. ges16:dim~8 g8:m9  %RECHECK THIS RHYTHM LATER
  g1:m9 | c1:sus13 | f1:maj9 | d2.:7  ges4:dim | \barNumberCheck #55

  %KEYS
  bes2.:m9~8. es16:sus13~  |2. ~8. c16:m7~ |2. ~8. f16:m7~ |2. ~8. bes16:m9~ | \barNumberCheck #59
  bes2.:m9~8. es16:sus13~  |2~ 8. d16:sus13~ 8 des8:sus13 | \barNumberCheck #61
  c1:sus13 | c2:sus13  s8. s16 s8. g16:m9 | \barNumberCheck #63

}



rhythmContent = {
  \override NoteHead.style = #'slash
  \override Accidental.stencil = ##f
  \transpose d d, {

  s1*2

    \barNumberCheck #3 %INTRO
    s4  s4 s4 d8.  d16 | % 4
     s4  s4  d8. d16 ~ d8 d8 |
  s1*3
       s4  s4 
         d8. -> d16 -> d8 d8 ~ -> | % 9

   d4. d8 ~ d2 | \barNumberCheck #10
   d16 d16 r8 r8 d16 d16 r4 d4 | % 11
   
   \barNumberCheck #11 %VS 1
   s1  | % 12
   s1  | % 13
   s1  | % 14
   s1 | %melody here % 15
   s1  | % 16
   s1  | % 17
   s1  | % 18
   s1 | %melody here % 19
   s1  | \barNumberCheck #20
   s1  | % 21
   s1  | % 22
   s1 | %melody here % 23
   s1  | % 24
   s1  | % 25
   s1  | % 26
   s1 | % 27
   
   \barNumberCheck #27 %CH 1
   s1  | % 28
   s1  | % 29
   s1  | \barNumberCheck #30
   s1 | %melody here % 31
   s1  | % 32
   s1  | % 33
   s1  | % 34
   s1 | %melody here % 35
   
   \barNumberCheck #35 %VS 2
   r4 s2. | % 36
   s1  | % 37
   s1  | % 38
   s1 | %melody here % 39
   s1  | \barNumberCheck #40
   s1  | % 41
   s1  | % 42
   s1 | %melody here % 43
   s1  | % 44
   s1  | % 45
   s1  | % 46
   s1 | %melody here % 47
   s1  | % 48
   s1  | % 49
   s1  | \barNumberCheck #50
   d8 8 8 8 8 8 r8. f16 | % 51 MELODY NOTE HERE THAT HAS TO BE FIXED
   
   \barNumberCheck #51 %CH 2
   s1 | % 52
   s1 | % 53
   s1   | % 54
   s1 | %melody here % 55
   s1  | % 56
   s2.  d8. 16 | % 57
   s1  | % 58
   s1 | %melody here % 59
   
   \barNumberCheck #59 %KEYS
   s2. des8. 16 | % 60
   s2. 8. 16 | % 61
   s2. 8. 16 | % 62
   s2. 8. 16 | % 63
   s2. 8. 16 | % 64
   s2 8. 16 ~ 8 8 | % 65
   s1  | % 66
   s2  16 16 16 r16 r8. 16 | % 67
   
   \barNumberCheck #67 %GTR
    s2. d8. 16 | % 68
   s2. 8. 16 | % 69
   s1  | \barNumberCheck #70
   s1 | %melody here % 71
   s2. 8. 16 | % 72
   s2 4. des8 ~ | % 73
   des4. 8 ~ 4. 8 ~ | % 74
   4. 8 s2 | % 75
   
   \barNumberCheck #75 %CH 3
   R1 | % 76
   r4 s2 r8 8 ~ | % 77
   %r4 bes4 ces8 des8 r8 d8 ~ | % 77
   4. 8 ~ 4. 8 ~ | % 78
   4 s2 r8 8 ~ | % 79
   %4 4 f8 ges8 r8 d8 ~ | % 79
   4 s4 4. 8 ~ | \barNumberCheck #80
   4. 8 ~ 2 | % 81
   4. 8 ~ 4. 8 ~ | % 82
   4 s2. | % 83
   
   \barNumberCheck #83 %CH 4
   s1  | % 84
   s1  | % 85
   s1  | % 86
   s2 8 ~ 4 8 | % 87
   s1  | % 88
   s1  | % 89
   s1  | \barNumberCheck #90
   s1  | % 91
   
   \barNumberCheck #91 %OUTRO
   s1  | % 92
   s2 8. 16 ~ 8 8 | % 93
   s1  | % 94
   s2 8. 16 ~ 8 8 | % 95
   s1  | % 96
   s2 4. 8 | % 97

}}

slashContent = {
  \override NoteHead.style = #'slash
  \override Stem.stencil = ##f
  \override Accidental.stencil = ##f
  \transpose d d, {


  d4 d4 d4 d4 d4 d4 d4 d4

    \barNumberCheck #3 %INTRO
    d4  4 4 s8.  s16 | % 4
     4  4  s8. s16 ~ s8 s8 |
     4 4 4 4 4 4 4 4 4 4 4 4

       d4  d4 
         s2 |

   s1| \barNumberCheck #10
   s1| % 11
   
   \barNumberCheck #11 %VS 1
   d4 4 4 4 | % 12
   d4 4 4 4 | % 13
   d4 4 4 4 | % 14
   s1 | %melody here % 15
   d4 4 4 4 | % 16
   d4 4 4 4 | % 17
   d4 4 4 4 | % 18
   s1 | %melody here % 19
   d4 4 4 4 | \barNumberCheck #20
   d4 4 4 4 | % 21
   d4 4 4 4 | % 22
   s1 | %melody here % 23
   d4 4 4 4 | % 24
   d4 4 4 4 | % 25
   d4 4 4 4 | % 26
   e8 r8 r4 r2 | % 27
   
   \barNumberCheck #27 %CH 1
   d4 4 4 4 | % 28
   d4 4 4 4 | % 29
   d4 4 4 4 | \barNumberCheck #30
   s1 | %melody here % 31
   d4 4 4 4 | % 32
   d4 4 4 4 | % 33
   d4 4 4 4 | % 34
   s1 | %melody here % 35
   
   \barNumberCheck #35 %VS 2
   s4 4 4 4 | % 36
   4 4 4 4 | % 37
   4 4 4 4 | % 38
   s1 | %melody here % 39
   d4 4 4 4 | \barNumberCheck #40
   4 4 4 4 | % 41
   4 4 4 4 | % 42
   s1 | %melody here % 43
   d4 4 4 4 | % 44
   4 4 4 4 | % 45
   4 4 4 4 | % 46
   s1 | %melody here % 47
   d4 4 4 4 | % 48
   4 4 4 4 | % 49
   4 4 4 4 | \barNumberCheck #50
   s1| % 51 MELODY NOTE HERE THAT HAS TO BE FIXED
   
   \barNumberCheck #51 %CH 2
   d4 4 4 4 | % 52
   4 4 4 4 | % 53
   4 4 4 s4  | % 54
   s1 | %melody here % 55
   d4 4 4 4 | % 56
   4 4 4 s8. s16 | % 57
   4 4 4 4 | % 58
   s1 | %melody here % 59
   
   \barNumberCheck #59 %KEYS
   d4 4 4 s4 | % 60
   d4 4 4 s4 | % 61
   d4 4 4 s4 | % 62
   d4 4 4 s4 | % 63
   d4 4 4 s4 | % 64
   d4 4 s2| % 65
   d4 4 4 4 | % 66
   4 4 s2| % 67
   
   \barNumberCheck #67 %GTR
    d4 4 4 s4 | % 68
   4 4 4 s4 | % 69
   4 4 4 4 | \barNumberCheck #70
   s1 | %melody here % 71
   d4 4 4 s4 | % 72
   4 4 s2 | % 73
   s1| % 74
   s2 des4 4 | % 75
   
   \barNumberCheck #75 %CH 3
   R1 | % 76
   s1| % 77
   %r4 bes4 ces8 des8 r8 d8 ~ | % 77
   s1| % 78
   s1| % 79
   %4 4 f8 ges8 r8 d8 ~ | % 79
   s4 4 s2 | \barNumberCheck #80
   s1| % 81
   s1| % 82
   s4 4 4 4 | % 83
   
   \barNumberCheck #83 %CH 4
   4 4 4 4 | % 84
   4 4 4 4 | % 85
   4 4 4 4 | % 86
   4 4 s2 | % 87
   4 4 4 4 | % 88
   4 4 4 4 | % 89
   4 4 4 4 | \barNumberCheck #90
   4 4 4 4 | % 91
   
   \barNumberCheck #91 %OUTRO
   4 4 4 4 | % 92
   4 4 s2 | % 93
   4 4 4 4 | % 94
   4 4 s2 | % 95
   4 4 4 4 | % 96
   4 4 s2 | % 97

}}

melodyContent = {

  \transpose c c, {



  s1*13

  \barNumberCheck #14 %VS 1 Melody
  g4 fis4 g4 bes4 | % 15
  
  s1*3
  
  \barNumberCheck #18 %VS 1 Melody
  g4 fis4 g4 bes4 | % 19
  
  s1*3
  
  \barNumberCheck #22 %VS 1 Melody
  g4 fis4 g4 bes4 | % 23
  
  s1*7
  
  \barNumberCheck #30 %CH 1 Melody
  g4 fis4 g4 a4 | % 31
  
  s1*7
  
  \barNumberCheck #38 %VS 2 Melody
  g4 fis4 g4 bes4 | % 39
  
  s1*3
  
  \barNumberCheck #42 %VS 2 Melody
  g4 fis4 g4 bes4 | % 43
  
  s1*3
  
  \barNumberCheck #46 %VS 2 Melody
  g4 fis4 g4 bes4 | % 47
  
  s1*6
  
  \barNumberCheck #53
  s2. s8.  g16~ | % 54
  \barNumberCheck #54 %CH 2 Melody
  g4 fis4 g8.  a16 ~  a8 a8  | % 55
  
  s1*15
  
  \barNumberCheck #70 %GTR Melody
  f4 g4 a4 r16 c8 [ c16 ] | % 71

  s1*5
  \barNumberCheck #76 %GTR Melody
   s4 bes,4 ces8 des8 s8 s8  | % 77

  s1

  \barNumberCheck #78
   s4 es4 f8 ges8 s8 s8 ~ | % 79
  
  }
}

 PartPOneVoiceOneChords =  \chordmode {
  
        | % 1
        s1 | % 2
        s1 | 
        
        \barNumberCheck #3 %INTRO
        g4:m9~ 2~ 8. c16:13.11~ | % 4
        2 c8.:13.11 des16:13.11 ~8 d8:13.11 | % 5
        es4:13.11~ 2 d4:13.11~ | % 6
        2 des4:13.11~ 4 | % 7
        c1:13.11    | % 8
        s4 c4:13.11 s8. es16:13.11 s8 as8:maj7 | % 9
        s4. des8:5 s2 | 
        c16:13.11 s16 s8 s8 s16 s16 s4 ges4:dim7 | 
        
        \barNumberCheck #11 %VS 1
        g4:m9 s4 s4 s4 | % 12
        c4:13.11 s4 s4 s4 | % 13
        f4:5.9 s4 s4 d4:7 | % 14
        s4 s4 s4 ges4:dim5 | % 15
        g4:m9 s4 s4 s4 | % 16
        c4:13.11 s4 s4 s4 | % 17
        f4:5.9 s4 s4 s4 | % 18
        d4:7 s4 s4 ges4:dim5 | % 19
        g4:m9 s4 s4 s4 | \barNumberCheck #20
        c4:13.11 s4 s4 s4 | % 21
        f4:5.9 s4 s4 s4 | % 22
        d4:7 s4 s4 ges4:dim5 | % 23
        g4:m9 s4 s4 s4 | % 24
        c4:13.11 s4 s4 s4 | % 25
        f4:maj7 s4 s4 s4 | % 26
        ges8:dim7 s8 s4 s2 | 

        \barNumberCheck #27 %CH 1
        g4:m9 s4 s4 s4 | % 28
        c4:13.11 s4 s4 s4 | % 29
        f4:maj9 s4 s4 s4 | 
        d4:7 s4 s4 ges4:dim5 | % 31
        g4:m9 s4 s4 s4 | % 32
        des4:dim5/+c s4 s4 s4 | % 33
        f4:maj9 s4 s4 s4 | % 34
        d4:7 s4 s4 ges4:dim5 | % 35

        \barNumberCheck #35 %VS 2
        g4:m9 s4 s4 s4 | % 36
        c4:13.11 s4 s4 s4 | % 37
        f4:5.9 s4 s4 s4 | % 38
        d4:7 s4 s4 ges4:dim5 | % 39
        g4:m9 s4 s4 s4 | 
        c4:13.11 s4 s4 s4 | % 41
        f4:5.9 s4 s4 s4 | % 42
        d4:7 s4 s4 ges4:dim5 | % 43
        g4:m9 s4 s4 s4 | % 44
        c4:13.11 s4 s4 s4 | % 45
        f4:5.9 s4 s4 s4 | % 46
        d4:7 s4 s4 ges4:dim5 | % 47
        g4:m9 s4 s4 s4 | % 48
        c4:13.11 s4 s4 s4 | % 49
        f4:maj9 s4 s4 s4 | 
        d8:7 s8 s8 s8 s8 ges8:dim7 s8. g16:m9 | % 51

        \barNumberCheck #51 %CH 2
        g4:m9 s4 s4 s4 | % 52
        c4:13.11 s4 s4 s4 | % 53
        f4:maj9 s4 s4 s8. d16:7 | % 54
        s4 s4 ges8.:dim5 s16 s8 g8:m9 | % 55
        g4:m9 s4 s4 s4 | % 56
        c4:13.11 s4 s4 s8. f16:maj9 | % 57
        s4 s4 s4 s4 | % 58
        d4:7 s4 s4 ges4:dim5 \bar "||"


        \barNumberCheck #59 %KEYS
        bes4:m9 s4 s4 s8. es16:13.11 | 
        s4 s4 s4 s8. c16:m7 | % 61
        s4 s4 s4 s8. f16:m7 | % 62
        s4 s4 s4 s8. bes16:m9 | % 63
        bes4:m9 s4 s4 s8. es16:13.11 | % 64
        s4 s4 es8.:13.11 d16:13.11 s8 des8:13.11 | % 65
        c4:13.11 s4 s4 s4 | % 66
        c4:13.11 s4 s16 s16 s16 s16 s8. g16:m9 \bar "||"


        \barNumberCheck #67 %GTR
        g4:m9 s4 s4 s8. c16:13.11 | % 68
        s4 s4 s4 s8. a16:m7 | % 69
        s1 | 
        d4:m7 c4:5 d4:m7 s16 ges8:dim7 g16:m9 | % 71
        g4:m9 s4 s4 s8. c16:13.11 | % 72
        s4 s4 s4. des8:5 | % 73
        s4. des8:6 s4. des8:7 | % 74
        s4. des8:5 s4 s4 |

        \barNumberCheck #75 %CH 3
        s1 | % 76
        s4 s4 s8 s8 s8 ges8:maj9 | % 77
        s4. fes8:maj7 s4. es8:7 | % 78
        s4 es4:7 es8:7 s8 s8 as8:m9 | % 79
        as4:m9 s4 s4. f8:dim7 | 
        s4. bes8:7 s2 | % 81
        es4.:m7 f8:dim5m7 s4. ges8:maj9 | % 82
        s1 | % 83

        \barNumberCheck #83 %CH 4
        des1:11  | % 84
        s1 | % 85
        es1:11 | % 86
        s4 s4 d8:11 s4 des8:11 | % 87
        des1:11 | % 88
        s1 | % 89
        es1:11 | 
        s1 | 

        \barNumberCheck #91 %Outro
        des4:11 s4 s4 s4 | % 92
        s4 s4 des8.:11 d16:11 s8 es8:11 | % 93
        s4 s4 s4 s4 | % 94
        s4 s4 es8.:11 d16:11 s8 des8:11 | % 95
        des4:11 s4 s4 s4 | % 96
        s4 s4 s4. s8 \bar "|."
    
 }
        


\score {
  <<
   \new ChordNames = "PartPOneVoiceOneChords" {\set chordChanges = ##t \PartPOneVoiceOneChords}


  % \new ChordNames {
  %   \set chordChanges = ##t
  %   % \chordProgression
  %   \newChordProgression
  % }
  \new Staff \transpose c c' { 
    \global
    
    <<
      \clef bass
      \keys
    \marks
    \new Voice { \rhythmContent }
    \new Voice { \melodyContent }
    \new Voice { \slashContent }
    \combinedBreaks
      % \repeat unfold 92 { \slashMarks }
     
    >>
  }
  >>
}




