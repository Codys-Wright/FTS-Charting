use chord_namer::{ChordName, ChordQuality, MajorSeventh, Extension, OmittedInterval, semitone_sequence::SemitoneSequence};
use colored::*;

fn print_test_header(test_name: &str) {
    println!("\n{}", format!("=== TESTING {} ===", test_name).bold().blue());
}

fn print_detailed_success_with_semitones(
    test_name: &str,
    semitones: &[u32],
    chord_name: Option<&ChordName>,
    interval_name: Option<&str>,
) {
    let sequence = SemitoneSequence::with_c_root(semitones.to_vec());
    
    println!("✓ {}", test_name.green().bold());
    println!("  Semitone Sequence: {}", semitones.iter().map(|s| s.to_string()).collect::<Vec<_>>().join(" "));
    println!("  Note Names: {}", sequence.get_note_names_interval_based().join(" "));
    println!("  Root Intervals: {}", sequence.get_root_intervals().join(" "));
    if semitones.len() > 1 {
        println!("  Stacked Intervals: {}", sequence.get_stacked_intervals().join(" "));
    }
    
    if let Some(chord) = chord_name {
        println!("  Recognized Chord: {}", chord.expanded_name_colored());
    } else if let Some(interval) = interval_name {
        println!("  Interval: {}", interval.cyan().bold());
    } else {
        println!("  No chord or interval recognized");
    }
    println!();
}

#[test]
fn test_dyads() {
    print_test_header("DYADS (TWO-NOTE INTERVALS)");
    
    // Minor 2nd (1 semitone)
    let semitones = vec![0, 1];
    let sequence = SemitoneSequence::with_c_root(semitones.clone());
    let chord = sequence.analyze_chord();
    let interval = sequence.get_interval_name();
    
    print_detailed_success_with_semitones(
        "Minor 2nd",
        &semitones,
        chord.as_ref(),
        interval.as_deref()
    );
    
    // Major 2nd (2 semitones)
    let semitones = vec![0, 2];
    let sequence = SemitoneSequence::with_c_root(semitones.clone());
    let chord = sequence.analyze_chord();
    let interval = sequence.get_interval_name();
    
    print_detailed_success_with_semitones(
        "Major 2nd",
        &semitones,
        chord.as_ref(),
        interval.as_deref()
    );
    
    // Minor 3rd (3 semitones)
    let semitones = vec![0, 3];
    let sequence = SemitoneSequence::with_c_root(semitones.clone());
    let chord = sequence.analyze_chord();
    let interval = sequence.get_interval_name();
    
    print_detailed_success_with_semitones(
        "Minor 3rd",
        &semitones,
        chord.as_ref(),
        interval.as_deref()
    );
    
    // Major 3rd (4 semitones)
    let semitones = vec![0, 4];
    let sequence = SemitoneSequence::with_c_root(semitones.clone());
    let chord = sequence.analyze_chord();
    let interval = sequence.get_interval_name();
    
    print_detailed_success_with_semitones(
        "Major 3rd",
        &semitones,
        chord.as_ref(),
        interval.as_deref()
    );
    
    // Perfect 4th (5 semitones)
    let semitones = vec![0, 5];
    let sequence = SemitoneSequence::with_c_root(semitones.clone());
    let chord = sequence.analyze_chord();
    let interval = sequence.get_interval_name();
    
    print_detailed_success_with_semitones(
        "Perfect 4th",
        &semitones,
        chord.as_ref(),
        interval.as_deref()
    );
    
    // Tritone (6 semitones)
    let semitones = vec![0, 6];
    let sequence = SemitoneSequence::with_c_root(semitones.clone());
    let chord = sequence.analyze_chord();
    let interval = sequence.get_interval_name();
    
    print_detailed_success_with_semitones(
        "Tritone",
        &semitones,
        chord.as_ref(),
        interval.as_deref()
    );
    
    // Perfect 5th (7 semitones)
    let semitones = vec![0, 7];
    let sequence = SemitoneSequence::with_c_root(semitones.clone());
    let chord = sequence.analyze_chord();
    let interval = sequence.get_interval_name();
    
    print_detailed_success_with_semitones(
        "Perfect 5th",
        &semitones,
        chord.as_ref(),
        interval.as_deref()
    );
    
    // Minor 6th (8 semitones)
    let semitones = vec![0, 8];
    let sequence = SemitoneSequence::with_c_root(semitones.clone());
    let chord = sequence.analyze_chord();
    let interval = sequence.get_interval_name();
    
    print_detailed_success_with_semitones(
        "Minor 6th",
        &semitones,
        chord.as_ref(),
        interval.as_deref()
    );
    
    // Major 6th (9 semitones)
    let semitones = vec![0, 9];
    let sequence = SemitoneSequence::with_c_root(semitones.clone());
    let chord = sequence.analyze_chord();
    let interval = sequence.get_interval_name();
    
    print_detailed_success_with_semitones(
        "Major 6th",
        &semitones,
        chord.as_ref(),
        interval.as_deref()
    );
    
    // Minor 7th (10 semitones)
    let semitones = vec![0, 10];
    let sequence = SemitoneSequence::with_c_root(semitones.clone());
    let chord = sequence.analyze_chord();
    let interval = sequence.get_interval_name();
    
    print_detailed_success_with_semitones(
        "Minor 7th",
        &semitones,
        chord.as_ref(),
        interval.as_deref()
    );
    
    // Major 7th (11 semitones)
    let semitones = vec![0, 11];
    let sequence = SemitoneSequence::with_c_root(semitones.clone());
    let chord = sequence.analyze_chord();
    let interval = sequence.get_interval_name();
    
    print_detailed_success_with_semitones(
        "Major 7th",
        &semitones,
        chord.as_ref(),
        interval.as_deref()
    );
    
    // Octave (12 semitones)
    let semitones = vec![0, 12];
    let sequence = SemitoneSequence::with_c_root(semitones.clone());
    let chord = sequence.analyze_chord();
    let interval = sequence.get_interval_name();
    
    print_detailed_success_with_semitones(
        "Octave",
        &semitones,
        chord.as_ref(),
        interval.as_deref()
    );
}
