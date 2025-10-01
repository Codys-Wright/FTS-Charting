\version "2.24.0"

% Set the font to Bravura for SMuFL support
ekmFont = "Bravura"

\include "english.ly"
\include "utils/fonts.ly"
\include "utils/chord-display.ly"
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
 s1*8  | 
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
  \key e \major
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
 \key g \major
 %KEYS
 s1*8 |
 %GTR
 \key e \major
 s1*6 |
 \key f \major
 s1*2
 %CH 3
 s1*8 |
 %CH 4
 s1*8 |
 %Outro
 s1*6 |

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
   8 s8 s2. | % 27
   
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
   s2. df8. 16 | % 60
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
   s2 4. df8 ~ | % 73
   df4. 8 ~ 4. 8 ~ | % 74
   4. 8 s2 | % 75
   
   \barNumberCheck #75 %CH 3
   R1 | % 76
   r4 s2 r8 8 ~ | % 77
   %r4 bf4 cf8 df8 r8 d8 ~ | % 77
   4. 8 ~ 4. 8 ~ | % 78
   4 s2 r8 8 ~ | % 79
   %4 4 f8 gf8 r8 d8 ~ | % 79
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
  }
}

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
   d4 4 4 4| % 26
   s8 r8 r4 r2 | % 27
   
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
   s2 df4 4 | % 75
   
   \barNumberCheck #75 %CH 3
   R1 | % 76
   s1| % 77
   %r4 bf4 cf8 df8 r8 d8 ~ | % 77
   s1| % 78
   s1| % 79
   %4 4 f8 gf8 r8 d8 ~ | % 79
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
  }
}

melodyContent = {

  \transpose c b,, {



  s1*13

  \barNumberCheck #14 %VS 1 Melody
  g4 fs4 g4 bf4 | % 15
  
  s1*3
  
  \barNumberCheck #18 %VS 1 Melody
  g4 fs4 g4 bf4 | % 19
  
  s1*3
  
  \barNumberCheck #22 %VS 1 Melody
  g4 fs4 g4 bf4 | % 23
  
  s1*7
  
  \barNumberCheck #30 %CH 1 Melody
  g4 fs4 g4 a4 | % 31
  
  s1*7
  
  \barNumberCheck #38 %VS 2 Melody
  g4 fs4 g4 bf4 | % 39
  
  s1*3
  
  \barNumberCheck #42 %VS 2 Melody
  g4 fs4 g4 bf4 | % 43
  
  s1*3
  
  \barNumberCheck #46 %VS 2 Melody
  g4 fs4 g4 bf4 | % 47
  
  s1*6
  
  \barNumberCheck #53
  s2. s8.  g16~ | % 54
  \barNumberCheck #54 %CH 2 Melody
  g4 fs4 g8.  a16 ~  a8 a8  | % 55
  
  s1*15
  
  \barNumberCheck #70 %GTR Melody
  f4 g4 a4 r16 c8 [ c16 ] | % 71

  s1*5
  \barNumberCheck #76 %GTR Melody
   s4 bf,4 cf8 df8 s8 s8  | % 77

  s1

  \barNumberCheck #78
   s4 f4 f8 gf8 s8 s8 ~ | % 79
  
  }
}

 chordProgression =  \chordmode {
  
        | % 1
        s1 | % 2
        s1 | 



        \barNumberCheck #3 %INTRO
        fs4:m9 s2 s8. b16:sus13 | % 4
        s2 b8.:sus13 c16:sus13 s8 cs8:sus13 | % 5
        d4:sus13 s2 d4:sus13 | % 6
        s2 cs4:sus13 s4 | % 7
        c1:sus13    | % 8
        s4 c4:sus13 s8. f16:sus13 s8 as8:maj7 | % 9
        s4. df8:5 s2 | 
        c16:sus13 s16 s8 s8 s16 s16 s4 gf4:dim7 | 
        
        \barNumberCheck #11 %VS 1
        fs4:m9 s2. | % 12
        b4:sus13 s4 s4 s4 | % 13
        e4:5.9 s4 s4 s4  | % 14
        cs4:7 s4 s4 f4:dim | % 15
        fs4:m9 s4 s4 s4 | % 16
        b4:sus13 s4 s4 s4 | % 17
        e4:5.9 s4 s4 s4 | % 18
        cs4:7 s4 s4 f4:dim | % 19
        fs4:m9 s4 s4 s4 | \barNumberCheck #20
        b4:sus13 s4 s4 s4 | % 21
        e4:5.9 s4 s4 s4 | % 22
        cs4:7 s4 s4 f4:dim | % 23
        fs4:m9 s4 s4 s4 | % 24
        b4:sus13 s4 s4 s4 | % 25
        e4:maj7 s4 s4 s4 | % 26
        f8:dim7 s8 s4 s2 | 

        \barNumberCheck #27 %CH 1
        fs4:m9 s4 s4 s4 | % 28
        b4:sus13 s4 s4 s4 | % 29
        e4:maj9 s4 s4 s4 | 
        cs4:7 s4 s4 f4:dim | % 31
        fs4:m9 s4 s4 s4 | % 32
        b4:7.9- s4 s4 s4 | % 33
        e4:maj9 s4 s4 s4 | % 34
        cs4:7 s4 s4 f4:dim | % 35

        \barNumberCheck #35 %VS 2
        fs4:m9 s4 s4 s4 | % 36
        b4:sus13 s4 s4 s4 | % 37
        e4:5.9 s4 s4 s4 | % 38
        cs4:7 s4 s4 f4:dim | % 39
        fs4:m9 s4 s4 s4 | 
        b4:sus13 s4 s4 s4 | % 41
        e4:5.9 s4 s4 s4 | % 42
        cs4:7 s4 s4 f4:dim | % 43
        fs4:m9 s4 s4 s4 | % 44
        b4:sus13 s4 s4 s4 | % 45
        e4:5.9 s4 s4 s4 | % 46
        cs4:7 s4 s4 f4:dim | % 47
        fs4:m9 s4 s4 s4 | % 48
        b4:sus13 s4 s4 s4 | % 49
        e4:maj9 s4 s4 s4 | 
        cs8:7 s8 s8 s8 s8 f8:dim7 s8. fs16:m9 | % 51

        \barNumberCheck #51 %CH 2
        fs4:m9 s4 s4 s4 | % 52
        b4:sus13 s4 s4 s4 | % 53
        e4:maj9 s4 s4 s8. cs16:7 | % 54
        s4 s4 f8.:dim s16 s8 fs8:m9 | % 55
        fs4:m9 s4 s4 s4 | % 56
        b4:sus13 s4 s4 s8. e16:maj9 | % 57
        s4 s4 s4 s4 | % 58
        cs4:7 s4 s4 f4:dim 


        \barNumberCheck #59 %KEYS
        a4:m9 s4 s4 s8. d16:sus13 | 
        s4 s4 s4 s8. b16:m7 | % 61
        s4 s4 s4 s8. e16:m7 | % 62
        s4 s4 s4 s8. a16:m9 | % 63
        a4:m9 s4 s4 s8. d16:sus13 | % 64
        s4 s4 d8.:sus13 cs16:sus13 s8 c8:sus13 | % 65
        b4:sus13 s4 s4 s4 | % 66
        b4:sus13 s4 s16 s16 s16 s16 s8. fs16:m9 


        \barNumberCheck #67 %GTR
        fs4:m9 s4 s4 s8. b16:sus13 | % 68
        s4 s4 s4 s8. gs16:m7 | % 69
        s1 | 
        cs4:m7 a4 cs4:m7 s16 f8:dim7 fs16:m9 | % 71
        fs4:m9 s4 s4 s8. b16:sus13 | % 72
        s4 s4 s4. c8:5 | % 73
        s4. c8:6 s4. c8:7 | % 74
        s4. c8:5 s4 s4 |

        \barNumberCheck #75 %CH 3
        s1 | % 76
        s4 s4 s8 s8 s8 f8:maj9 | % 77
        s4. ef8:maj7 s4. d8:7 | % 78
        s4 d4:7 d8:sus7 s8 s8 a8:m9 | % 79
        a4:m9 s4 s4. e8:dim7 | 
        s4. a8:7 s2 | % 81
        d4.:m7 e8:dim7 s4. f8:maj9 | % 82
        s1 | % 83

        \barNumberCheck #83 %CH 4
        c1:11  | % 84
        s1 | % 85
        d1:11 | % 86
        s4 s4 cs8:11 s4 c8:11 | % 87
        c1:11 | % 88
        s1 | % 89
        d1:11 | 
        s1 | 

        \barNumberCheck #91 %Outro
        c4:11 s4 s4 s4 | % 92
        s4 s4 c8.:11 cs16:11 s8 d8:11 | % 93
        s1 | % 94
        s4 s4 d8.:11 cs16:11 s8 c8:11 | % 95
        c4:11 s4 s4 s4 | % 96
        s1 
    
 }



\score {
  <<
   \new ChordNames = "chordProgression" {
    \set chordChanges = ##t 
    \set chordNameExceptions = #chordExceptions
    \set chordRootNamer = #musejazz-chord-name->markup
    \override ChordName.font-size = #3
    \override ChordName.font-name = #"MuseJazz Text"
    \chordProgression
    }


  \new Staff \transpose c c' { 
    \global
    \set Staff.printKeyCancellation = ##f
    
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




