use chord_namer::semitone_sequence::{SemitoneSequence, analyze_with_c_root};
use colored::*;

#[test]
fn test_major_11th_analysis() {
    println!("{}", "=== TESTING MAJOR 11TH CHORD ANALYSIS ===".bright_blue().bold());
    println!();
    
    // The sequence: 0 4 7 11 14 17
    // Should be: C E G B D F
    // This should be a major 11th chord
    let sequence = SemitoneSequence::with_c_root(vec![0, 4, 7, 11, 14, 17]);
    
    println!("{} {}", "✓".green().bold(), "Major 11th Analysis");
    println!("  Semitone Sequence: {}", sequence.semitones.iter().map(|s| s.to_string()).collect::<Vec<_>>().join(" ").bright_white());
    println!("  Note Names: {}", sequence.get_note_names().join(" ").bright_cyan());
    println!("  Root Intervals: {}", sequence.get_root_intervals().join(" ").bright_yellow());
    if sequence.get_stacked_intervals().len() > 0 {
        println!("  Stacked Intervals: {}", sequence.get_stacked_intervals().join(" ").bright_magenta());
    }
    
    if let Some(chord) = sequence.analyze_chord() {
        println!("  Recognized Chord: {}", chord.expanded_name_colored());
    } else {
        println!("  Could not recognize chord structure");
    }
    println!();
}

#[test]
fn test_major_triad_analysis() {
    println!("{}", "=== TESTING MAJOR TRIAD ANALYSIS ===".bright_blue().bold());
    println!();
    
    let sequence = SemitoneSequence::with_c_root(vec![0, 4, 7]);
    
    println!("{} {}", "✓".green().bold(), "Major Triad Analysis");
    println!("  Semitone Sequence: {}", sequence.semitones.iter().map(|s| s.to_string()).collect::<Vec<_>>().join(" ").bright_white());
    println!("  Note Names: {}", sequence.get_note_names().join(" ").bright_cyan());
    println!("  Root Intervals: {}", sequence.get_root_intervals().join(" ").bright_yellow());
    if sequence.get_stacked_intervals().len() > 0 {
        println!("  Stacked Intervals: {}", sequence.get_stacked_intervals().join(" ").bright_magenta());
    }
    
    if let Some(chord) = sequence.analyze_chord() {
        println!("  Recognized Chord: {}", chord.expanded_name_colored());
    } else {
        println!("  Could not recognize chord structure");
    }
    println!();
}

#[test]
fn test_major_7th_analysis() {
    println!("=== TESTING MAJOR 7TH ANALYSIS ===");
    analyze_with_c_root(vec![0, 4, 7, 11]);
}

#[test]
fn test_major_9th_analysis() {
    println!("=== TESTING MAJOR 9TH ANALYSIS ===");
    analyze_with_c_root(vec![0, 4, 7, 11, 14]);
}

#[test]
fn test_6_9_chord_analysis() {
    println!("=== TESTING 6/9 CHORD ANALYSIS ===");
    analyze_with_c_root(vec![0, 4, 7, 9, 14]);
}
