use lilypond::{compile, is_lilypond_file};
use lilypond::notation::note::Note;
use lilypond::notation::pitch::{NoteName, Accidental, Octave};
use lilypond::notation::rhythm::{Length, DurationType, Dots};
use std::fs;

fn main() {
    println!("Creating a simple LilyPond file with notes...");
    
    // Create a simple LilyPond file content
    let lilypond_content = r#"\version "2.24.0"

\score {
  \new Staff {
    \clef treble
    \time 4/4
    \key c \major
    
    c4 d4 e4 f4 |
    g4 a4 b4 c'4 |
    r1 |
    c'4 b4 a4 g4 |
    f4 e4 d4 c4 |
  }
  \layout {}
  \midi {}
}"#;

    // Write the content to a file
    let filename = "test_output.ly";
    fs::write(filename, lilypond_content).expect("Failed to write LilyPond file");
    
    println!("Created {} with content:", filename);
    println!("{}", lilypond_content);
    
    // Check if it's a valid LilyPond file
    if is_lilypond_file(filename) {
        println!("\nFile is valid LilyPond format. Attempting to compile...");
        
        // Try to compile it
        if compile(filename) {
            println!("✅ Successfully compiled {} to PDF!", filename);
        } else {
            println!("❌ Failed to compile {}. Make sure LilyPond is installed.", filename);
        }
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
