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
fn test_dominant_seventh() {
    print_test_header("DOMINANT SEVENTH CHORDS");
    
    // Dominant 7th (1 3 5 7) - C E G Bb
    let semitones = vec![0, 4, 7, 10];
    let sequence = SemitoneSequence::with_c_root(semitones.clone());
    let chord = sequence.analyze_chord();
    let interval = sequence.get_interval_name();
    
    print_detailed_success_with_semitones(
        "Dominant 7th",
        &semitones,
        chord.as_ref(),
        interval.as_deref()
    );
    
    // Dominant 7th omit3 (1 5 7) - C G Bb
    let semitones = vec![0, 7, 10];
    let sequence = SemitoneSequence::with_c_root(semitones.clone());
    let chord = sequence.analyze_chord();
    let interval = sequence.get_interval_name();
    
    print_detailed_success_with_semitones(
        "Dominant 7th omit3",
        &semitones,
        chord.as_ref(),
        interval.as_deref()
    );
    
    // Dominant 7th omit5 (1 3 7) - C E Bb
    let semitones = vec![0, 4, 10];
    let sequence = SemitoneSequence::with_c_root(semitones.clone());
    let chord = sequence.analyze_chord();
    let interval = sequence.get_interval_name();
    
    print_detailed_success_with_semitones(
        "Dominant 7th omit5",
        &semitones,
        chord.as_ref(),
        interval.as_deref()
    );
}

#[test]
fn test_dominant_ninth() {
    print_test_header("DOMINANT NINTH CHORDS");
    
    // Dominant 9th (1 3 5 7 9) - C E G Bb D
    let semitones = vec![0, 4, 7, 10, 14];
    let sequence = SemitoneSequence::with_c_root(semitones.clone());
    let chord = sequence.analyze_chord();
    let interval = sequence.get_interval_name();
    
    print_detailed_success_with_semitones(
        "Dominant 9th",
        &semitones,
        chord.as_ref(),
        interval.as_deref()
    );
    
    // Dominant 9th omit3 (1 5 7 9) - C G Bb D
    let semitones = vec![0, 7, 10, 14];
    let sequence = SemitoneSequence::with_c_root(semitones.clone());
    let chord = sequence.analyze_chord();
    let interval = sequence.get_interval_name();
    
    print_detailed_success_with_semitones(
        "Dominant 9th omit3",
        &semitones,
        chord.as_ref(),
        interval.as_deref()
    );
    
    // Dominant 9th omit5 (1 3 7 9) - C E Bb D
    let semitones = vec![0, 4, 10, 14];
    let sequence = SemitoneSequence::with_c_root(semitones.clone());
    let chord = sequence.analyze_chord();
    let interval = sequence.get_interval_name();
    
    print_detailed_success_with_semitones(
        "Dominant 9th omit5",
        &semitones,
        chord.as_ref(),
        interval.as_deref()
    );
}

#[test]
fn test_dominant_thirteenth() {
    print_test_header("DOMINANT THIRTEENTH CHORDS");
    
    // Dominant 13th (1 3 5 7 9 11 13) - C E G Bb D F A
    let semitones = vec![0, 4, 7, 10, 14, 17, 21];
    let sequence = SemitoneSequence::with_c_root(semitones.clone());
    let chord = sequence.analyze_chord();
    let interval = sequence.get_interval_name();
    
    print_detailed_success_with_semitones(
        "Dominant 13th",
        &semitones,
        chord.as_ref(),
        interval.as_deref()
    );
    
    // Dominant 13th omit5 (1 3 7 9 11 13) - C E Bb D F A
    let semitones = vec![0, 4, 10, 14, 17, 21];
    let sequence = SemitoneSequence::with_c_root(semitones.clone());
    let chord = sequence.analyze_chord();
    let interval = sequence.get_interval_name();
    
    print_detailed_success_with_semitones(
        "Dominant 13th omit5",
        &semitones,
        chord.as_ref(),
        interval.as_deref()
    );
    
    // Dominant 13th omit9 (1 3 5 7 11 13) - C E G Bb F A
    let semitones = vec![0, 4, 7, 10, 17, 21];
    let sequence = SemitoneSequence::with_c_root(semitones.clone());
    let chord = sequence.analyze_chord();
    let interval = sequence.get_interval_name();
    
    print_detailed_success_with_semitones(
        "Dominant 13th omit9",
        &semitones,
        chord.as_ref(),
        interval.as_deref()
    );
}

#[test]
fn test_dominant_sharp_eleventh() {
    print_test_header("DOMINANT SHARP ELEVENTH CHORDS");
    
    // Dominant 7#11 (1 3 5 7 #11) - C E G Bb F#
    let semitones = vec![0, 4, 7, 10, 18];
    let sequence = SemitoneSequence::with_c_root(semitones.clone());
    let chord = sequence.analyze_chord();
    let interval = sequence.get_interval_name();
    
    print_detailed_success_with_semitones(
        "Dominant 7#11",
        &semitones,
        chord.as_ref(),
        interval.as_deref()
    );
    
    // Dominant 7#11 omit5 (1 3 7 #11) - C E Bb F#
    let semitones = vec![0, 4, 10, 18];
    let sequence = SemitoneSequence::with_c_root(semitones.clone());
    let chord = sequence.analyze_chord();
    let interval = sequence.get_interval_name();
    
    print_detailed_success_with_semitones(
        "Dominant 7#11 omit5",
        &semitones,
        chord.as_ref(),
        interval.as_deref()
    );
    
    // Dominant 9#11 (1 3 5 7 9 #11) - C E G Bb D F#
    let semitones = vec![0, 4, 7, 10, 14, 18];
    let sequence = SemitoneSequence::with_c_root(semitones.clone());
    let chord = sequence.analyze_chord();
    let interval = sequence.get_interval_name();
    
    print_detailed_success_with_semitones(
        "Dominant 9#11",
        &semitones,
        chord.as_ref(),
        interval.as_deref()
    );
    
    // Dominant 9#11 omit5 (1 3 7 9 #11) - C E Bb D F#
    let semitones = vec![0, 4, 10, 14, 18];
    let sequence = SemitoneSequence::with_c_root(semitones.clone());
    let chord = sequence.analyze_chord();
    let interval = sequence.get_interval_name();
    
    print_detailed_success_with_semitones(
        "Dominant 9#11 omit5",
        &semitones,
        chord.as_ref(),
        interval.as_deref()
    );
}

#[test]
fn test_dominant_sharp_eleventh_full() {
    print_test_header("DOMINANT SHARP ELEVENTH CHORDS (FULL SET)");
    
    // Dominant 7#11 omit5 (1 3 7 #11) - C E Bb F#
    let semitones = vec![0, 4, 10, 18];
    let sequence = SemitoneSequence::with_c_root(semitones.clone());
    let chord = sequence.analyze_chord();
    let interval = sequence.get_interval_name();
    
    print_detailed_success_with_semitones(
        "Dominant 7#11 omit5",
        &semitones,
        chord.as_ref(),
        interval.as_deref()
    );
    
    // Dominant 7#11 (1 3 5 7 #11) - C E G Bb F#
    let semitones = vec![0, 4, 7, 10, 18];
    let sequence = SemitoneSequence::with_c_root(semitones.clone());
    let chord = sequence.analyze_chord();
    let interval = sequence.get_interval_name();
    
    print_detailed_success_with_semitones(
        "Dominant 7#11",
        &semitones,
        chord.as_ref(),
        interval.as_deref()
    );
    
    // Dominant 9#11 omit5 (1 3 7 9 #11) - C E Bb D F#
    let semitones = vec![0, 4, 10, 14, 18];
    let sequence = SemitoneSequence::with_c_root(semitones.clone());
    let chord = sequence.analyze_chord();
    let interval = sequence.get_interval_name();
    
    print_detailed_success_with_semitones(
        "Dominant 9#11 omit5",
        &semitones,
        chord.as_ref(),
        interval.as_deref()
    );
    
    // Dominant 9#11 (1 3 5 7 9 #11) - C E G Bb D F#
    let semitones = vec![0, 4, 7, 10, 14, 18];
    let sequence = SemitoneSequence::with_c_root(semitones.clone());
    let chord = sequence.analyze_chord();
    let interval = sequence.get_interval_name();
    
    print_detailed_success_with_semitones(
        "Dominant 9#11",
        &semitones,
        chord.as_ref(),
        interval.as_deref()
    );
}
