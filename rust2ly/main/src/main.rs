use lilypond::{compile, is_lilypond_file};
use lilypond::notation::note::Note;
use lilypond::notation::pitch::{NoteName, Accidental, Octave};
use lilypond::notation::rhythm::{Length, DurationType, Dots};
use std::fs;
use std::env;

fn main() {
    println!("Creating a simple LilyPond file with notes...");
    
    // Create a LilyPond file with the same structure and basic formatting
    let lilypond_content = r#"\version "2.24.0"

% Set the font to Bravura for SMuFL support

\include "english.ly"
\include "../../lilypond-utils/fonts.ly"
\include "../../lilypond-utils/chord-display.ly"
\include "../../lilypond-utils/key-changes.ly"
\include "../../lilypond-utils/note-placement.ly"
\include "../../lilypond-utils/paper-setup.ly"
\include "../../lilypond-utils/capsules/measure-position-detection.ly"
\include "../../lilypond-utils/capsules/capsule-utils.ly"
\include "../../lilypond-utils/capsules/new-capsule-utils.ly"
\include "../../lilypond-utils/rehearsal-marks/rehearsal-mark-positioning.ly"
\include "../../lilypond-utils/breaks/auto-four-measure-breaks.ly"
\include "../../lilypond-utils/breaks/pseudo-indents.ly"
\include "../../lilypond-utils/breaks/auto-pseudo-indents.ly"
\include "../../lilypond-utils/layout/spacing.ly"
\include "../../lilypond-utils/layout/score-layout.ly"
\include "../../lilypond-utils/header-template.ly"
\include "../../lilypond-utils/testing.ly"

% Replicate the marks variable from test.ly
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

% Global settings including key signature
global = { \time 4/4  }

\score {
  \new Staff {
    \clef treble
    \time 4/4
    \key c \major
    
    \marks
  }
  \layout {
    \context {
      \Score
      \override RehearsalMark.self-alignment-X = #LEFT
      \override RehearsalMark.font-size = #2
    }
  }
  \midi {}
}"#;

    // Create output directory in target
    let output_dir = "target/lilypond-output";
    fs::create_dir_all(output_dir).expect("Failed to create output directory");
    
    // Write the content to a file in the output directory
    let filename = format!("{}/test_output.ly", output_dir);
    fs::write(&filename, lilypond_content).expect("Failed to write LilyPond file");
    
    println!("Created {} with content:", filename);
    println!("{}", lilypond_content);
    
    // Check if it's a valid LilyPond file
    if is_lilypond_file(&filename) {
        println!("\nFile is valid LilyPond format. Attempting to compile...");
        
        // Change to output directory before compiling so PDF is created there
        let current_dir = env::current_dir().expect("Failed to get current directory");
        env::set_current_dir(output_dir).expect("Failed to change to output directory");
        
        // Try to compile it (now using just the filename since we're in the output dir)
        let just_filename = "test_output.ly";
        if compile(just_filename) {
            println!("✅ Successfully compiled {} to PDF!", just_filename);
        } else {
            println!("❌ Failed to compile {}. Make sure LilyPond is installed.", just_filename);
        }
        
        // Change back to original directory
        env::set_current_dir(current_dir).expect("Failed to change back to original directory");
    } else {
        println!("❌ File is not a valid LilyPond file.");
    }
    
    // Test the lilypond-rs note creation
    println!("\n--- Testing lilypond-rs Note creation ---");
    
    let note_c = Note::new(NoteName::C);
    println!("Created note C: {:?}", note_c);
    
    let note_d = Note::new(NoteName::D);
    println!("Created note D: {:?}", note_d);
    
    // Test creating notes with different octaves and accidentals
    let mut note_flat = Note::new(NoteName::A);
    note_flat.pitch.accidental = Accidental::Flat;
    note_flat.pitch.octave = Octave::S2;
    note_flat.rhythm.length = Length::Eighth;
    note_flat.rhythm.dots = Dots::new(1);
    
    println!("Created note A-flat in octave 2, dotted eighth: {:?}", note_flat);
}
