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
fn test_compound_intervals() {
    print_test_header("COMPOUND INTERVALS (BEYOND OCTAVE)");
    
    // Minor 9th (13 semitones)
    let semitones = vec![0, 13];
    let sequence = SemitoneSequence::with_c_root(semitones.clone());
    let chord = sequence.analyze_chord();
    let interval = sequence.get_interval_name();
    
    print_detailed_success_with_semitones(
        "Minor 9th",
        &semitones,
        chord.as_ref(),
        interval.as_deref()
    );
    
    // Major 9th (14 semitones)
    let semitones = vec![0, 14];
    let sequence = SemitoneSequence::with_c_root(semitones.clone());
    let chord = sequence.analyze_chord();
    let interval = sequence.get_interval_name();
    
    print_detailed_success_with_semitones(
        "Major 9th",
        &semitones,
        chord.as_ref(),
        interval.as_deref()
    );
    
    // Minor 10th (15 semitones)
    let semitones = vec![0, 15];
    let sequence = SemitoneSequence::with_c_root(semitones.clone());
    let chord = sequence.analyze_chord();
    let interval = sequence.get_interval_name();
    
    print_detailed_success_with_semitones(
        "Minor 10th",
        &semitones,
        chord.as_ref(),
        interval.as_deref()
    );
    
    // Major 10th (16 semitones)
    let semitones = vec![0, 16];
    let sequence = SemitoneSequence::with_c_root(semitones.clone());
    let chord = sequence.analyze_chord();
    let interval = sequence.get_interval_name();
    
    print_detailed_success_with_semitones(
        "Major 10th",
        &semitones,
        chord.as_ref(),
        interval.as_deref()
    );
    
    // Perfect 11th (17 semitones)
    let semitones = vec![0, 17];
    let sequence = SemitoneSequence::with_c_root(semitones.clone());
    let chord = sequence.analyze_chord();
    let interval = sequence.get_interval_name();
    
    print_detailed_success_with_semitones(
        "Perfect 11th",
        &semitones,
        chord.as_ref(),
        interval.as_deref()
    );
    
    // Minor 12th (18 semitones)
    let semitones = vec![0, 18];
    let sequence = SemitoneSequence::with_c_root(semitones.clone());
    let chord = sequence.analyze_chord();
    let interval = sequence.get_interval_name();
    
    print_detailed_success_with_semitones(
        "Minor 12th",
        &semitones,
        chord.as_ref(),
        interval.as_deref()
    );
    
    // Perfect 12th (19 semitones)
    let semitones = vec![0, 19];
    let sequence = SemitoneSequence::with_c_root(semitones.clone());
    let chord = sequence.analyze_chord();
    let interval = sequence.get_interval_name();
    
    print_detailed_success_with_semitones(
        "Perfect 12th",
        &semitones,
        chord.as_ref(),
        interval.as_deref()
    );
    
    // Minor 13th (20 semitones)
    let semitones = vec![0, 20];
    let sequence = SemitoneSequence::with_c_root(semitones.clone());
    let chord = sequence.analyze_chord();
    let interval = sequence.get_interval_name();
    
    print_detailed_success_with_semitones(
        "Minor 13th",
        &semitones,
        chord.as_ref(),
        interval.as_deref()
    );
    
    // Major 13th (21 semitones)
    let semitones = vec![0, 21];
    let sequence = SemitoneSequence::with_c_root(semitones.clone());
    let chord = sequence.analyze_chord();
    let interval = sequence.get_interval_name();
    
    print_detailed_success_with_semitones(
        "Major 13th",
        &semitones,
        chord.as_ref(),
        interval.as_deref()
    );
    
    // Minor 14th (22 semitones)
    let semitones = vec![0, 22];
    let sequence = SemitoneSequence::with_c_root(semitones.clone());
    let chord = sequence.analyze_chord();
    let interval = sequence.get_interval_name();
    
    print_detailed_success_with_semitones(
        "Minor 14th",
        &semitones,
        chord.as_ref(),
        interval.as_deref()
    );
    
    // Major 14th (23 semitones)
    let semitones = vec![0, 23];
    let sequence = SemitoneSequence::with_c_root(semitones.clone());
    let chord = sequence.analyze_chord();
    let interval = sequence.get_interval_name();
    
    print_detailed_success_with_semitones(
        "Major 14th",
        &semitones,
        chord.as_ref(),
        interval.as_deref()
    );
    
    // Double Octave (24 semitones)
    let semitones = vec![0, 24];
    let sequence = SemitoneSequence::with_c_root(semitones.clone());
    let chord = sequence.analyze_chord();
    let interval = sequence.get_interval_name();
    
    print_detailed_success_with_semitones(
        "Double Octave",
        &semitones,
        chord.as_ref(),
        interval.as_deref()
    );
}
