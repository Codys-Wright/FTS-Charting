use chord_namer::{ChordName, ChordAnalysis, ChordQuality, MajorSeventh, Extension, OmittedInterval, NamingLevels, semitone_sequence::SemitoneSequence};
use colored::*;

// Helper function for colorful test output
fn print_test_header(title: &str) {
    println!("{}", format!("=== {} ===", title).bright_blue().bold());
    println!();
}

fn print_success(message: &str, chord_name: &str) {
    println!("{} {} -> {}", "✓".green().bold(), message, chord_name.bright_cyan());
}

fn print_detailed_success(message: &str, chord: &ChordName) {
    println!("{} {} -> {}", 
             "✓".green().bold(), 
             message, 
             &chord.expanded_name_colored());
}

fn print_detailed_success_with_semitones(message: &str, chord: &ChordName, semitones: &[u32]) {
    let sequence = SemitoneSequence::with_c_root(semitones.to_vec());
    
    println!("{} {}", "✓".green().bold(), message);
    println!("  Semitone Sequence: {}", semitones.iter().map(|s| s.to_string()).collect::<Vec<_>>().join(" ").bright_white());
    println!("  Note Names: {}", sequence.get_note_names_interval_based().join(" ").bright_cyan());
    println!("  Root Intervals: {}", sequence.get_root_intervals().join(" ").bright_yellow());
    if sequence.get_stacked_intervals().len() > 0 {
        println!("  Stacked Intervals: {}", sequence.get_stacked_intervals().join(" ").bright_magenta());
    }
    println!("  Recognized Chord: {}", &chord.expanded_name_colored());
    println!();
}

#[test]
fn test_major_triads() {
    print_test_header("TESTING MAJOR TRIADS");

    // Test basic major triad
    let major_triad = ChordName::major();
    assert_eq!(major_triad.quality, ChordQuality::Major(MajorSeventh::None));
    assert!(major_triad.extensions.is_empty());
    assert!(major_triad.alterations.is_empty());
    assert!(major_triad.omitted.is_empty());
    assert!(major_triad.additions.is_empty());
    
    print_detailed_success_with_semitones("Major triad", &major_triad, &[0, 4, 7]);
}

#[test]
fn test_major_sixths() {
    print_test_header("TESTING MAJOR SIXTHS");

    // Test major 6th chord
    let major_6th = ChordName::major().with_addition("6");
    assert_eq!(major_6th.quality, ChordQuality::Major(MajorSeventh::None));
    assert!(major_6th.extensions.is_empty());
    assert!(major_6th.alterations.is_empty());
    assert!(major_6th.omitted.is_empty());
    assert!(major_6th.additions.contains(&"6".to_string()));
    
    print_detailed_success_with_semitones("Major 6th", &major_6th, &[0, 4, 7, 9]);

    // Test major 6th omit3
    let major_6th_omit3 = ChordName::major().with_addition("6").omit_third();
    assert_eq!(major_6th_omit3.quality, ChordQuality::Major(MajorSeventh::None));
    assert!(major_6th_omit3.omitted.contains(&OmittedInterval::Third));
    assert!(major_6th_omit3.additions.contains(&"6".to_string()));
    
    print_detailed_success_with_semitones("Major 6th omit3", &major_6th_omit3, &[0, 7, 9]);

    // Test major 6/9 chord
    let major_6_9 = ChordName::major().with_addition("6").with_ninth();
    assert_eq!(major_6_9.quality, ChordQuality::Major(MajorSeventh::None));
    assert!(major_6_9.extensions.contains(&Extension::Ninth));
    assert!(major_6_9.additions.contains(&"6".to_string()));
    
    print_detailed_success_with_semitones("6/9", &major_6_9, &[0, 4, 7, 9, 14]);

    // Test major 6/9 omit5
    let major_6_9_omit5 = ChordName::major().with_addition("6").with_ninth().omit_fifth();
    assert_eq!(major_6_9_omit5.quality, ChordQuality::Major(MajorSeventh::None));
    assert!(major_6_9_omit5.extensions.contains(&Extension::Ninth));
    assert!(major_6_9_omit5.omitted.contains(&OmittedInterval::Fifth));
    assert!(major_6_9_omit5.additions.contains(&"6".to_string()));
    
    print_detailed_success_with_semitones("6/9 omit5", &major_6_9_omit5, &[0, 4, 9, 14]);
}

#[test]
fn test_major_sevenths() {
    println!("=== TESTING MAJOR SEVENTHS ===");
    println!();

    // Test major 7th chord
    let major_7th = ChordName::major7();
    assert_eq!(major_7th.quality, ChordQuality::Major(MajorSeventh::Major7));
    assert!(major_7th.extensions.is_empty());
    assert!(major_7th.alterations.is_empty());
    assert!(major_7th.omitted.is_empty());
    assert!(major_7th.additions.is_empty());
    
    print_detailed_success_with_semitones("Major 7th", &major_7th, &[0, 4, 7, 11]);

    // Test major 7th omit3
    let major_7th_omit3 = ChordName::major7().omit_third();
    assert_eq!(major_7th_omit3.quality, ChordQuality::Major(MajorSeventh::Major7));
    assert!(major_7th_omit3.omitted.contains(&OmittedInterval::Third));
    
    print_detailed_success_with_semitones("Major 7th omit3", &major_7th_omit3, &[0, 7, 11]);

    // Test major 7th omit5
    let major_7th_omit5 = ChordName::major7().omit_fifth();
    assert_eq!(major_7th_omit5.quality, ChordQuality::Major(MajorSeventh::Major7));
    assert!(major_7th_omit5.omitted.contains(&OmittedInterval::Fifth));
    
    print_detailed_success_with_semitones("Major 7th omit5", &major_7th_omit5, &[0, 4, 11]);
}

#[test]
fn test_major_ninths() {
    print_test_header("TESTING MAJOR NINTHS");

    // Test major 9th chord (complete sequence: 7th + 9th)
    let major_9th = ChordName::major7().with_ninth();
    assert_eq!(major_9th.quality, ChordQuality::Major(MajorSeventh::Major7));
    assert!(major_9th.extensions.contains(&Extension::Ninth));
    assert!(major_9th.alterations.is_empty());
    assert!(major_9th.omitted.is_empty());
    assert!(major_9th.additions.is_empty());
    assert!(major_9th.has_complete_extension_sequence());
    
    print_detailed_success_with_semitones("Major 9th", &major_9th, &[0, 4, 7, 11, 14]);

    // Test major 9th omit5
    let major_9th_omit5 = ChordName::major7().with_ninth().omit_fifth();
    assert_eq!(major_9th_omit5.quality, ChordQuality::Major(MajorSeventh::Major7));
    assert!(major_9th_omit5.extensions.contains(&Extension::Ninth));
    assert!(major_9th_omit5.omitted.contains(&OmittedInterval::Fifth));
    
    print_detailed_success_with_semitones("Major 9th omit5", &major_9th_omit5, &[0, 4, 11, 14]);

    // Test major 9th omit3
    let major_9th_omit3 = ChordName::major7().with_ninth().omit_third();
    assert_eq!(major_9th_omit3.quality, ChordQuality::Major(MajorSeventh::Major7));
    assert!(major_9th_omit3.extensions.contains(&Extension::Ninth));
    assert!(major_9th_omit3.omitted.contains(&OmittedInterval::Third));
    
    print_detailed_success_with_semitones("Major 9th omit3", &major_9th_omit3, &[0, 7, 11, 14]);

    // Test major add9 (triad with 9th - incomplete sequence)
    let major_add9 = ChordName::major().with_ninth();
    assert_eq!(major_add9.quality, ChordQuality::Major(MajorSeventh::None));
    assert!(major_add9.extensions.contains(&Extension::Ninth));
    assert!(!major_add9.has_complete_extension_sequence()); // Should be false since no 7th
    
    print_detailed_success_with_semitones("Major add9", &major_add9, &[0, 4, 7, 14]);
}

#[test]
fn test_major_elevenths() {
    print_test_header("TESTING MAJOR ELEVENTHS");

    // Test major 11th chord (complete sequence: 7th + 9th + 11th)
    let major_11th = ChordName::major7().with_ninth().with_eleventh();
    assert_eq!(major_11th.quality, ChordQuality::Major(MajorSeventh::Major7));
    assert!(major_11th.extensions.contains(&Extension::Ninth));
    assert!(major_11th.extensions.contains(&Extension::Eleventh));
    assert!(major_11th.has_complete_extension_sequence());
    
    print_detailed_success("Major 11th", &major_11th);

    // Test major 11th omit5
    let major_11th_omit5 = ChordName::major7().with_ninth().with_eleventh().omit_fifth();
    assert_eq!(major_11th_omit5.quality, ChordQuality::Major(MajorSeventh::Major7));
    assert!(major_11th_omit5.extensions.contains(&Extension::Ninth));
    assert!(major_11th_omit5.extensions.contains(&Extension::Eleventh));
    assert!(major_11th_omit5.omitted.contains(&OmittedInterval::Fifth));
    
    print_detailed_success_with_semitones("Major 11th omit5", &major_11th_omit5, &[0, 4, 11, 14, 17]);

    // Test major 7th add11 (7th + 11th without 9th)
    let major_7_add11 = ChordName::major7().with_eleventh();
    assert_eq!(major_7_add11.quality, ChordQuality::Major(MajorSeventh::Major7));
    assert!(major_7_add11.extensions.contains(&Extension::Eleventh));
    assert!(!major_7_add11.has_complete_extension_sequence()); // Incomplete sequence
    
    print_detailed_success_with_semitones("Major 7th add11", &major_7_add11, &[0, 4, 7, 11, 17]);

    // Test major 9th (7th + 9th)
    let major_9th = ChordName::major7().with_ninth();
    assert_eq!(major_9th.quality, ChordQuality::Major(MajorSeventh::Major7));
    assert!(major_9th.extensions.contains(&Extension::Ninth));
    assert!(major_9th.has_complete_extension_sequence()); // Complete sequence
    
    print_detailed_success_with_semitones("Major 9th", &major_9th, &[0, 4, 7, 11, 14]);
}

#[test]
fn test_major_thirteenths() {
    print_test_header("TESTING MAJOR THIRTEENTHS");

    // Test major 13th chord (complete sequence: 7th + 9th + 11th + 13th)
    let major_13th = ChordName::major7().with_ninth().with_eleventh().with_thirteenth();
    assert_eq!(major_13th.quality, ChordQuality::Major(MajorSeventh::Major7));
    assert!(major_13th.extensions.contains(&Extension::Ninth));
    assert!(major_13th.extensions.contains(&Extension::Eleventh));
    assert!(major_13th.extensions.contains(&Extension::Thirteenth));
    assert!(major_13th.has_complete_extension_sequence());
    
    print_detailed_success_with_semitones("Major 13th", &major_13th, &[0, 4, 7, 11, 14, 17, 21]);

    // Test that [0, 4, 7, 11, 14, 17] (without 13th) is NOT maj13
    let sequence_without_13th = SemitoneSequence::with_c_root(vec![0, 4, 7, 11, 14, 17]);
    if let Some(chord) = sequence_without_13th.analyze_chord() {
        assert_ne!(chord.expanded_name(), "maj13", "Sequence [0, 4, 7, 11, 14, 17] should NOT be maj13 - it's missing the 13th!");
        assert_eq!(chord.expanded_name(), "maj11", "Sequence [0, 4, 7, 11, 14, 17] should be maj11");
    } else {
        panic!("Sequence [0, 4, 7, 11, 14, 17] should be recognized as maj11");
    }

    // Test major 13th omit5
    let major_13th_omit5 = ChordName::major7().with_ninth().with_eleventh().with_thirteenth().omit_fifth();
    assert_eq!(major_13th_omit5.quality, ChordQuality::Major(MajorSeventh::Major7));
    assert!(major_13th_omit5.extensions.contains(&Extension::Ninth));
    assert!(major_13th_omit5.extensions.contains(&Extension::Eleventh));
    assert!(major_13th_omit5.extensions.contains(&Extension::Thirteenth));
    assert!(major_13th_omit5.omitted.contains(&OmittedInterval::Fifth));
    
    print_detailed_success_with_semitones("Major 13th omit5", &major_13th_omit5, &[0, 4, 7, 11, 14, 17, 21]);

    // Test major 13th omit9
    let major_13th_omit9 = ChordName::major7().with_eleventh().with_thirteenth().omit_ninth();
    assert_eq!(major_13th_omit9.quality, ChordQuality::Major(MajorSeventh::Major7));
    assert!(major_13th_omit9.extensions.contains(&Extension::Eleventh));
    assert!(major_13th_omit9.extensions.contains(&Extension::Thirteenth));
    assert!(major_13th_omit9.omitted.contains(&OmittedInterval::Ninth));
    assert!(!major_13th_omit9.has_complete_extension_sequence()); // Incomplete sequence
    
    print_detailed_success_with_semitones("Major 13th omit9", &major_13th_omit9, &[0, 4, 7, 11, 17, 21]);

    // Test major 11th omit3 (7th + 9th + 11th without 3rd)
    let major_11th_omit3 = ChordName::major7().with_ninth().with_eleventh().omit_third();
    assert_eq!(major_11th_omit3.quality, ChordQuality::Major(MajorSeventh::Major7));
    assert!(major_11th_omit3.extensions.contains(&Extension::Ninth));
    assert!(major_11th_omit3.extensions.contains(&Extension::Eleventh));
    assert!(major_11th_omit3.omitted.contains(&OmittedInterval::Third));
    
    print_detailed_success_with_semitones("Major 11th omit3", &major_11th_omit3, &[0, 4, 7, 11, 14, 17]);

    // Test major 7th add13 (incomplete sequence: 7th + 13th without 9th and 11th)
    let major_7_add13 = ChordName::major7().with_thirteenth();
    assert_eq!(major_7_add13.quality, ChordQuality::Major(MajorSeventh::Major7));
    assert!(major_7_add13.extensions.contains(&Extension::Thirteenth));
    assert!(!major_7_add13.has_complete_extension_sequence()); // Incomplete sequence
    
    print_detailed_success_with_semitones("Major 7th add13", &major_7_add13, &[0, 4, 7, 11, 14, 17, 21]);
}

#[test]
fn test_major_sixth_chords() {
    print_test_header("TESTING MAJOR SIXTH CHORDS");

    // Test basic major 6th chord (should be "maj6" not "maj 6")
    let major_6th = ChordName::major().with_addition("6");
    assert_eq!(major_6th.quality, ChordQuality::Major(MajorSeventh::None));
    assert!(major_6th.additions.contains(&"6".to_string()));
    assert!(!major_6th.quality_has_seventh());
    
    print_detailed_success_with_semitones("Major 6th", &major_6th, &[0, 4, 7, 9]);

    // Test major 6th with 7th (should be "maj7 6")
    let major_7_6th = ChordName::major7().with_addition("6");
    assert_eq!(major_7_6th.quality, ChordQuality::Major(MajorSeventh::Major7));
    assert!(major_7_6th.additions.contains(&"6".to_string()));
    assert!(major_7_6th.quality_has_seventh());
    
    print_detailed_success_with_semitones("Major 7th add6", &major_7_6th, &[0, 4, 7, 9, 11]);

    // Test 6/9 chord (should be "6/9" not "maj6/9")
    let major_6_9 = ChordName::major().with_addition("6").with_ninth();
    assert_eq!(major_6_9.quality, ChordQuality::Major(MajorSeventh::None));
    assert!(major_6_9.additions.contains(&"6".to_string()));
    assert!(major_6_9.extensions.contains(&Extension::Ninth));
    assert!(!major_6_9.quality_has_seventh());
    
    print_detailed_success_with_semitones("6/9", &major_6_9, &[0, 4, 7, 9, 14]);
}

#[test]
fn test_major_6_9_chord() {
    print_test_header("TESTING 6/9 CHORD");

    // Test 6/9 chord (should be "6/9" not "maj6/9")
    let major_6_9 = ChordName::major().with_addition("6").with_ninth();
    assert_eq!(major_6_9.quality, ChordQuality::Major(MajorSeventh::None));
    assert!(major_6_9.additions.contains(&"6".to_string()));
    assert!(major_6_9.extensions.contains(&Extension::Ninth));
    assert!(!major_6_9.quality_has_seventh());
    
    print_detailed_success_with_semitones("6/9", &major_6_9, &[0, 4, 7, 9, 14]);

    // Test major 11th chord (complete sequence: 7th + 9th + 11th)
    let major_11th = ChordName::major7().with_ninth().with_eleventh();
    assert_eq!(major_11th.quality, ChordQuality::Major(MajorSeventh::Major7));
    assert!(major_11th.extensions.contains(&Extension::Ninth));
    assert!(major_11th.extensions.contains(&Extension::Eleventh));
    assert!(major_11th.has_complete_extension_sequence());
    
    print_detailed_success_with_semitones("Major 11th", &major_11th, &[0, 4, 7, 11, 14, 17]);
}

#[test]
fn test_suspended_chords() {
    print_test_header("TESTING SUSPENDED CHORDS");

    // Test sus2 chord
    let sus2 = ChordName::major().with_sus2();
    assert_eq!(sus2.quality, ChordQuality::Major(MajorSeventh::None));
    assert!(sus2.has_sus2());
    assert!(sus2.is_suspended());
    
    print_detailed_success_with_semitones("Sus2", &sus2, &[0, 2, 7]);

    // Test sus4 chord
    let sus4 = ChordName::major().with_sus4();
    assert_eq!(sus4.quality, ChordQuality::Major(MajorSeventh::None));
    assert!(sus4.has_sus4());
    assert!(sus4.is_suspended());
    
    print_detailed_success_with_semitones("Sus4", &sus4, &[0, 5, 7]);

    // Test sus2 with 7th
    let sus2_7 = ChordName::major7().with_sus2();
    assert_eq!(sus2_7.quality, ChordQuality::Major(MajorSeventh::Major7));
    assert!(sus2_7.has_sus2());
    assert!(sus2_7.is_suspended());
    
    print_detailed_success_with_semitones("Sus2 7th", &sus2_7, &[0, 2, 7, 11]);

    // Test sus4 with 7th
    let sus4_7 = ChordName::major7().with_sus4();
    assert_eq!(sus4_7.quality, ChordQuality::Major(MajorSeventh::Major7));
    assert!(sus4_7.has_sus4());
    assert!(sus4_7.is_suspended());
    
    print_detailed_success_with_semitones("Sus4 7th", &sus4_7, &[0, 5, 7, 11]);

    // Test 9sus4 chord
    let sus4_9 = ChordName::major().with_sus4().with_ninth();
    assert_eq!(sus4_9.quality, ChordQuality::Major(MajorSeventh::None));
    assert!(sus4_9.has_sus4());
    assert!(sus4_9.extensions.contains(&Extension::Ninth));
    
    print_detailed_success_with_semitones("9sus4", &sus4_9, &[0, 5, 7, 14]);

    // Test 13sus4 chord
    let sus4_13 = ChordName::major().with_sus4().with_thirteenth();
    assert_eq!(sus4_13.quality, ChordQuality::Major(MajorSeventh::None));
    assert!(sus4_13.has_sus4());
    assert!(sus4_13.extensions.contains(&Extension::Thirteenth));
    
    print_detailed_success_with_semitones("13sus4", &sus4_13, &[0, 5, 7, 17]);

    // Test maj7sus4 chord
    let maj7sus4 = ChordName::major7().with_sus4();
    assert_eq!(maj7sus4.quality, ChordQuality::Major(MajorSeventh::Major7));
    assert!(maj7sus4.has_sus4());
    assert!(maj7sus4.quality_has_seventh());
    
    print_detailed_success_with_semitones("Maj7sus4", &maj7sus4, &[0, 5, 7, 11]);
}
