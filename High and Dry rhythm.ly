\version "2.24.4"
% automatically converted by musicxml2ly from High and Dry rhythm.musicxml
\pointAndClickOff

\header {
    title =  "High and Dry"
    composer =  \markup \column {
        \line { "Radiohead"}
        \line { ""} }
    
    encodingsoftware =  "MuseScore Studio 4.6.0"
    encodingdate =  "2025-09-23"
    subtitle =  "Transcribed by Cody Wright"
    poet =  "V3"
    }

#(set-global-staff-size 19.997485714285716)
\paper {
    
    paper-width = 21.01\cm
    paper-height = 29.69\cm
    top-margin = 1.5\cm
    bottom-margin = 1.5\cm
    left-margin = 1.5\cm
    right-margin = 1.5\cm
    indent = 1.6161538461538463\cm
    short-indent = 1.292923076923077\cm
    }
\layout {
    \context { \Score
        skipBars = ##t
        autoBeaming = ##f
        }
    }
PartPOneVoiceOne =  \relative es {
    \clef "bass" \numericTimeSignature\time 4/4 \key f \major | % 1
    \tempo 4=88 \once \override NoteHead.style = #'slash \stemNeutral es4
    _\markup{ \with-color #(rgb-color 1.0 0.46875 0.0) {Drum Intro!} }
    \once \override NoteHead.style = #'slash \stemNeutral es4 \once
    \override NoteHead.style = #'slash \stemNeutral es4 \once \override
    NoteHead.style = #'slash \stemNeutral es4 | % 2
     \once \override NoteHead.style = #'slash \stemNeutral es4 \once
    \override NoteHead.style = #'slash \stemNeutral es4 \once \override
    NoteHead.style = #'slash \stemNeutral es4 \once \override
    NoteHead.style = #'slash \stemNeutral es4 \break | % 3
    \mark \markup { \box { INST } } r4 _\markup{ \with-color #(rgb-color
        0.59375 0.4140625 0.265625) {SPARKLES} } _\markup{ \with-color
        #(rgb-color 0.1484375 0.6328125 0.41015625) {Synth Melody} } r4
    r4 \once \override NoteHead.style = #'slash \stemDown e8. [ \once
    \override NoteHead.style = #'slash \stemDown e16 ] | % 4
    r4 r4 \once \override NoteHead.style = #'slash \stemDown e8. [ \once
    \override NoteHead.style = #'slash \stemDown e16 ~ ] \once \override
    NoteHead.style = #'slash \stemDown e8 [ \once \override
    NoteHead.style = #'slash \stemDown e8 ] | % 5
    r4 _\markup{ \with-color #(rgb-color 0.1484375 0.6328125 0.41015625)
        {<--Arps -------->} } r4 r4 r4 | % 6
        r4 r4 r4 r4 \break | % 7
        R1 | % 8
         \once \override NoteHead.style = #'slash \stemNeutral e4 \once
        \override NoteHead.style = #'slash \stemNeutral e4 \once
        \override NoteHead.style = #'slash \stemDown e8. -> [ \once
        \override NoteHead.style = #'slash \stemDown e16 ~ -> ] \once
        \override NoteHead.style = #'slash \stemDown e8 [ \once
        \override NoteHead.style = #'slash \stemDown e8 ~ -> ] | % 9
         \once \override NoteHead.style = #'slash \stemDown e4. \once
        \override NoteHead.style = #'slash \stemDown e8 ~ _\markup{
            \with-color #(rgb-color 1.0 0.46875 0.0) {Fill} } \stemDown
        e2 | \barNumberCheck #10
         \once \override NoteHead.style = #'slash \stemDown e16 -> [
        \once \override NoteHead.style = #'slash \stemDown e16 -> ] r8 r8
        \once \override NoteHead.style = #'slash \stemDown e16 -> [
        \once \override NoteHead.style = #'slash \stemDown e16 -> ] r4
        \stemDown e4 \break | % 11
        \mark \markup { \box { VS 1 } } R1*3 | % 14
        \stemDown g4 \stemDown fis4 \stemDown g4 \stemDown bes4 \break | % 15
        R1*3 | % 18
        \stemDown g4 \stemDown fis4 \stemDown g4 \stemDown bes4 \break | % 19
        R1*3 | % 22
        \stemDown g4 \stemDown fis4 \stemDown g4 \stemDown bes4 \break | % 23
        R1*3 | % 26
         \once \override NoteHead.style = #'slash \stemDown e,8 -> r8 r4
        _\markup{ \small\italic {...dont leave me haiiiiiiiiiiiiiii} } r2
        \break | % 27
        \mark \markup { \box { CH 1 } } R1*3 | \barNumberCheck #30
        \stemDown g4 \stemDown fis4 \stemDown g4 \stemDown a4 \break | % 31
        R1*3 | % 34
        \stemDown g4 _\markup{ \with-color #(rgb-color 1.0 0.46875 0.0)
            {BIG FILL!} } \stemDown fis4 \stemDown g4 \stemDown a4
        \pageBreak | % 35
        \mark \markup { \box { VS 2 } } R1*3 | % 38
        \stemDown g4 \stemDown fis4 \stemDown g4 \stemDown bes4 \break | % 39
        R1*3 | % 42
        \stemDown g4 \stemDown fis4 \stemDown g4 \stemDown bes4
        _\markup{ \with-color #(rgb-color 0.59375 0.4140625 0.265625)
            {SPARKLES} } \break | % 43
        R1*3 | % 46
        \stemDown g4 \stemDown fis4 \stemDown g4 \stemDown bes4 \break | % 47
        R1*3 | \barNumberCheck #50
         \once \override NoteHead.style = #'slash \stemDown e,8 -> [
        \once \override NoteHead.style = #'slash \stemDown e8 -> \once
        \override NoteHead.style = #'slash \stemDown e8 -> \once
        \override NoteHead.style = #'slash \stemDown e8 -> ] \once
        \override NoteHead.style = #'slash \stemDown e8 -> [ \once
        \override NoteHead.style = #'slash \stemDown e8 -> ] r8. \once
        \override NoteHead.style = #'slash \stemDown f16 \break | % 51
        \mark \markup { \box { CH 2 } } R1*2 | % 53
        r2 r4 \once \override NoteHead.style = #'slash \stemDown es8. [
        \stemDown g16 ~ ] | % 54
        \stemDown g4 -> \stemDown fis4 \stemDown g8. -> [ \stemDown a16
        ~ -> ] \stemDown a8 [ \stemDown a8 -> ] \break | % 55
        R1 | % 56
        r2 r4 \once \override NoteHead.style = #'slash \stemDown es8. [
        \once \override NoteHead.style = #'slash \stemDown es16 -> ] | % 57
        R1 | % 58
        \stemDown g4 _\markup{ \with-color #(rgb-color 1.0 0.46875 0.0)
            {BIG FILL!} } \stemDown fis4 \stemDown g4 \stemDown bes4
        \bar "||"
        \break | % 59
        \key as \major \mark \markup { \box { INST } } \mark \markup {
            \box { KEYS } } r2 r4 \once \override NoteHead.style =
        #'slash \stemDown e,8. [ \once \override NoteHead.style =
        #'slash \stemDown e16 ] | \barNumberCheck #60
        r2 r4 \once \override NoteHead.style = #'slash \stemDown e8. [
        \once \override NoteHead.style = #'slash \stemDown e16 ] | % 61
        r2 r4 \once \override NoteHead.style = #'slash \stemDown e8. [
        \once \override NoteHead.style = #'slash \stemDown e16 ] | % 62
        r2 \once \override NoteHead.style = #'slash \stemDown es4 ->
        \once \override NoteHead.style = #'slash \stemDown es8. -> [
        \once \override NoteHead.style = #'slash \stemDown es16 -> ]
        \break | % 63
        r2 r4 \once \override NoteHead.style = #'slash \stemDown es8. [
        \once \override NoteHead.style = #'slash \stemDown es16 ] | % 64
        r4 r4 \once \override NoteHead.style = #'slash \stemDown e8. [
        \once \override NoteHead.style = #'slash \stemDown e16 ~ ] \once
        \override NoteHead.style = #'slash \stemDown e8 [ \once
        \override NoteHead.style = #'slash \stemDown e8 -> ] | % 65
        R1 | % 66
        r2 \once \override NoteHead.style = #'slash \stemDown es16 -> [
        \once \override NoteHead.style = #'slash \stemDown es16 -> \once
        \override NoteHead.style = #'slash \stemDown es16 -> ] r16 r8.
        \once \override NoteHead.style = #'slash \stemDown es16 -> \bar
        "||"
        \break | % 67
        \key f \major \mark \markup { \box { INST } } \mark \markup {
            \box { GTR } } r4 _\markup{ \with-color #(rgb-color
            0.20703125 0.515625 0.890625) {GTR SOLO} } r4 r4 \once
        \override NoteHead.style = #'slash \stemDown e8. [ \once
        \override NoteHead.style = #'slash \stemDown e16 ] | % 68
        r2 r4 \once \override NoteHead.style = #'slash \stemDown e8. [
        \once \override NoteHead.style = #'slash \stemDown e16 ] | % 69
        R1 | \barNumberCheck #70
        \stemDown f4 -> \stemDown g4 -> \stemDown a4 -> r16 \stemDown c8
        -> [ \stemDown c16 -> ] \break | % 71
        r2 r4 \once \override NoteHead.style = #'slash \stemDown e,8. [
        \once \override NoteHead.style = #'slash \stemDown e16 ] | % 72
        r2 \once \override NoteHead.style = #'slash \stemDown es4. \once
        \override NoteHead.style = #'slash \stemDown es8 ~ -> _\markup{
            \with-color #(rgb-color 0.1484375 0.6328125 0.41015625)
            {Synth Walkup} } | % 73
        \key ges \major \once \override NoteHead.style = #'slash
        \stemDown es4. \once \override NoteHead.style = #'slash
        \stemDown es8 ~ -> \once \override NoteHead.style = #'slash
        \stemDown es4. \once \override NoteHead.style = #'slash
        \stemDown es8 ~ -> | % 74
         \once \override NoteHead.style = #'slash \stemDown es4. \once
        \override NoteHead.style = #'slash \stemDown es8 r2 \pageBreak | % 75
        \key ges \major \mark \markup { \box { CH 3 } } R1 _\markup{
            \with-color #(rgb-color 0.20703125 0.515625 0.890625) {Gtr
                Riffs} } _\markup{ \with-color #(rgb-color 0.59375
            0.4140625 0.265625) {SPARKLES} } | % 76
        r4 _\markup{ \with-color #(rgb-color 0.56640625 0.25390625
            0.671875) {Bass Walkdown} } \stemUp bes4 -> _\markup{
            \with-color #(rgb-color 0.89453125 0.12890625 0.0) {BAND
                HITS} } \stemUp ces8 -> [ \stemUp des8 -> ] r8 \once
        \override NoteHead.style = #'slash \stemDown es8 ~ -> | % 77
         \once \override NoteHead.style = #'slash \stemNeutral es4.
        \once \override NoteHead.style = #'slash \stemDown es8 ~ ->
        \once \override NoteHead.style = #'slash \stemNeutral es4. \once
        \override NoteHead.style = #'slash \stemNeutral es8 ~ ->
        _\markup{ \with-color #(rgb-color 1.0 0.46875 0.0) {BIG TOMS} }
        | % 78
         \once \override NoteHead.style = #'slash \stemDown es4
        \stemDown es4 -> \stemDown f8 -> [ \stemDown ges8 -> ] r8 \once
        \override NoteHead.style = #'slash \stemDown es8 ~ \break | % 79
         \once \override NoteHead.style = #'slash \stemNeutral es4
        _\markup{ \with-color #(rgb-color 0.89453125 0.12890625 0.0)
            {Back to Groove} } \once \override NoteHead.style = #'slash
        \stemNeutral es4 \once \override NoteHead.style = #'slash
        \stemNeutral es4. \once \override NoteHead.style = #'slash
        \stemDown es8 ~ -> | \barNumberCheck #80
         \once \override NoteHead.style = #'slash \stemNeutral es4.
        \once \override NoteHead.style = #'slash \stemNeutral es8 ~ ->
        \stemDown es2 | % 81
         \once \override NoteHead.style = #'slash \stemDown es4. \once
        \override NoteHead.style = #'slash \stemDown es8 ~ \once
        \override NoteHead.style = #'slash \stemDown es4. \once
        \override NoteHead.style = #'slash \stemDown es8 ~ _\markup{
            \with-color #(rgb-color 1.0 0.46875 0.0) {HUGE FILL} } | % 82
         \once \override NoteHead.style = #'slash \stemNeutral es4 r4 r2
        \break | % 83
        \mark \markup { \box { CH 4 } } \once \override NoteHead.style =
        #'slash \stemNeutral es4 _\markup{ \with-color #(rgb-color
            0.89453125 0.12890625 0.0) {8th Note Vamp} } _\markup{
            \with-color #(rgb-color 0.1484375 0.6328125 0.41015625)
            {Organ + Rhodes} } \once \override NoteHead.style = #'slash
        \stemNeutral es4 \once \override NoteHead.style = #'slash
        \stemNeutral es4 \once \override NoteHead.style = #'slash
        \stemNeutral es4 _\markup{ \with-color #(rgb-color 0.20703125
            0.515625 0.890625) {funkkkk} } | % 84
         \once \override NoteHead.style = #'slash \stemNeutral es4 \once
        \override NoteHead.style = #'slash \stemNeutral es4 \once
        \override NoteHead.style = #'slash \stemNeutral es4 \once
        \override NoteHead.style = #'slash \stemNeutral es4 | % 85
        R1 | % 86
        r2 \once \override NoteHead.style = #'slash \stemDown es8 ~ ->
        \once \override NoteHead.style = #'slash \stemDown es4 \once
        \override NoteHead.style = #'slash \stemDown es8 -> \break | % 87
        R1*4 \break | % 91
        \mark \markup { \box { OUT } } R1 | % 92
        r2 \once \override NoteHead.style = #'slash \stemDown es8. -> [
        \once \override NoteHead.style = #'slash \stemDown es16 ~ -> ]
        \once \override NoteHead.style = #'slash \stemDown es8 [ \once
        \override NoteHead.style = #'slash \stemDown es8 -> ] | % 93
        R1 | % 94
        r2 \once \override NoteHead.style = #'slash \stemDown es8. -> [
        \once \override NoteHead.style = #'slash \stemDown es16 ~ -> ]
        \once \override NoteHead.style = #'slash \stemDown es8 [ \once
        \override NoteHead.style = #'slash \stemDown es8 -> ] \break | % 95
        R1 | % 96
        r2 \once \override NoteHead.style = #'slash \stemDown es4. \once
        \override NoteHead.style = #'slash \stemDown es8 _\markup{
            \with-color #(rgb-color 0.89453125 0.12890625 0.0) {LAST HIT
                (Just Drums and Vocals)} } \bar "|."
        }
    
    PartPOneVoiceOneChords =  \chordmode {
        | % 1
        s4 s4 s4 s4 | % 2
        s4 s4 s4 s4 | % 3
        g4:m9 s4 s4 s8. c16:13.11 | % 4
        s4 s4 c8.:13.11 des16:13.11 s8 d8:13.11 | % 5
        es4:13.11 s4 s4 d4:13.11 | % 6
        s4 s4 des4:13.11 s4 | % 7
        s1 | % 8
        s4 c4:13.11 s8. es16:13.11 s8 as8:maj7 | % 9
        s4. des8:5 s2 | \barNumberCheck #10
        c16:13.11 s16 s8 s8 s16 s16 s4 ges4:dim7 | % 11
        s1 s1 s1 | % 14
        s4 s4 s4 ges4:dim5 | % 15
        s1 s1 s1 | % 18
        d4:7 s4 s4 ges4:dim5 | % 19
        s1 s1 s1 | % 22
        d4:7 s4 s4 ges4:dim5 | % 23
        s1 s1 s1 | % 26
        ges8:dim7 s8 s4 s2 | % 27
        s1 s1 s1 | \barNumberCheck #30
        d4:7 s4 s4 ges4:dim5 | % 31
        s1 s1 s1 | % 34
        d4:7 s4 s4 ges4:dim5 | % 35
        s1 s1 s1 | % 38
        d4:7 s4 s4 ges4:dim5 | % 39
        s1 s1 s1 | % 42
        d4:7 s4 s4 ges4:dim5 | % 43
        s1 s1 s1 | % 46
        d4:7 s4 s4 ges4:dim5 | % 47
        s1 s1 s1 | \barNumberCheck #50
        d8:7 s8 s8 s8 s8 ges8:dim7 s8. g16:m9 | % 51
        s1 s1 | % 53
        s2 s4 s8. d16:7 | % 54
        s4 s4 ges8.:dim5 s16 s8 g8:m9 | % 55
        s1 | % 56
        s2 s4 s8. f16:maj9 | % 57
        s1 | % 58
        d4:7 s4 s4 ges4:dim5 \bar "||"
        s2 s4 s8. es16:13.11 | \barNumberCheck #60
        s2 s4 s8. c16:m7 | % 61
        s2 s4 s8. f16:m7 | % 62
        s2 s4 s8. bes16:m9 | % 63
        s2 s4 s8. es16:13.11 | % 64
        s4 s4 es8.:13.11 d16:13.11 s8 des8:13.11 | % 65
        s1 | % 66
        s2 s16 s16 s16 s16 s8. g16:m9 \bar "||"
        g4:m9 s4 s4 s8. c16:13.11 | % 68
        s2 s4 s8. a16:m7 | % 69
        s1 | \barNumberCheck #70
        d4:m7 c4:5 d4:m7 s16 ges8:dim7 g16:m9 | % 71
        s2 s4 s8. c16:13.11 | % 72
        s2 s4. des8:5 | % 73
        s4. des8:6 s4. des8:7 | % 74
        s4. des8:5 s2 | % 75
        s1 | % 76
        s4 s4 s8 s8 s8 ges8:maj9 | % 77
        s4. fes8:maj7 s4. es8:7 | % 78
        s4 es4:7 es8:7 s8 s8 as8:m9 | % 79
        as4:m9 s4 s4. f8:dim7 | \barNumberCheck #80
        s4. bes8:7 s2 | % 81
        es4.:m7 f8:dim5m7 s4. ges8:maj9 | % 82
        s4 s4 s2 | % 83
        des4:11 s4 s4 s4 | % 84
        s4 s4 s4 s4 | % 85
        s1 | % 86
        s2 d8:11 s4 des8:11 | % 87
        s1 s1 s1 s1 | % 91
        s1 | % 92
        s2 des8.:11 d16:11 s8 es8:11 | % 93
        s1 | % 94
        s2 es8.:11 d16:11 s8 des8:11 | % 95
        s1 | % 96
        s2 s4. s8 \bar "|."
        }
    
    
    % The score definition
    \score {
        <<
            
            \context ChordNames = "PartPOneVoiceOneChords" { \PartPOneVoiceOneChords}
            \new Staff
            <<
                \set Staff.instrumentName = "Piano"
                \set Staff.shortInstrumentName = "Pno."
                
                \context Staff << 
                    \mergeDifferentlyDottedOn\mergeDifferentlyHeadedOn
                    \context Voice = "PartPOneVoiceOne" {  \PartPOneVoiceOne }
                    >>
                >>
            
            >>
        \layout {}
        % To create MIDI output, uncomment the following line:
        %  \midi {\tempo 4 = 88 }
        }
    
