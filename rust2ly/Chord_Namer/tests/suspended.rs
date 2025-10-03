use chord_namer::{ChordName, NamingLevels, semitone_sequence::SemitoneSequence};
use colored::*;

fn print_detailed_success_with_semitones(
    test_name: &str,
    semitones: &[u32],
    expected_chord: &ChordName,
) {
    let sequence = SemitoneSequence::new(semitones.to_vec(), None);
    let analysis = sequence.analyze_chord();
    
    println!("✓ {}", test_name.bright_green());
    println!("  Semitone Sequence: {}", semitones.iter().map(|s| s.to_string()).collect::<Vec<_>>().join(" ").bright_cyan());
    println!("  Note Names: {}", sequence.get_note_names_interval_based().join(" ").bright_cyan());
    println!("  Root Intervals: {}", sequence.get_root_intervals().join(" ").bright_cyan());
    println!("  Stacked Intervals: {}", sequence.get_stacked_intervals().join(" ").bright_cyan());
    
    if let Some(actual_chord) = analysis {
        println!("  Recognized Chord: {}", actual_chord.expanded().bright_yellow());
        
        // Assert that the recognized chord matches expected
        assert_eq!(actual_chord.quality, expected_chord.quality, "Chord quality mismatch for {}", test_name);
        assert_eq!(actual_chord.extensions, expected_chord.extensions, "Extensions mismatch for {}", test_name);
        
        // Check alterations (order doesn't matter)
        let actual_alterations = &actual_chord.alterations;
        let expected_alterations = &expected_chord.alterations;
        assert_eq!(actual_alterations.len(), expected_alterations.len(), "Alterations count mismatch for {}", test_name);
        for alteration in expected_alterations {
            assert!(actual_alterations.contains(alteration), "Missing alteration {:?} for {}", alteration, test_name);
        }
        
        assert_eq!(actual_chord.omitted, expected_chord.omitted, "Omitted intervals mismatch for {}", test_name);
    } else {
        panic!("Failed to recognize chord for {}", test_name);
    }
}

#[test]
fn test_suspended_chords() {
    use chord_namer::{ChordQuality, MajorSeventh, Extension, Alteration, OmittedInterval};
    
    println!("\n=== TESTING SUSPENDED CHORDS ===");
    
    // sus4: C F G (1 6 8)
    print_detailed_success_with_semitones(
        "Suspended 4th",
        &[0, 5, 7],
        &ChordName {
            quality: ChordQuality::Suspended(MajorSeventh::None),
            extensions: vec![],
            alterations: vec![],
            omitted: vec![],
            additions: vec!["sus4".to_string()],
        }
    );
    
    // sus2: C D G (1 3 8)
    print_detailed_success_with_semitones(
        "Suspended 2nd",
        &[0, 2, 7],
        &ChordName {
            quality: ChordQuality::Suspended(MajorSeventh::None),
            extensions: vec![],
            alterations: vec![],
            omitted: vec![],
            additions: vec!["sus2".to_string()],
        }
    );
    
    // 7sus4 omit5: C F B♭ (1 6 11)
    print_detailed_success_with_semitones(
        "Dominant 7sus4 omit5",
        &[0, 5, 10],
        &ChordName {
            quality: ChordQuality::Suspended(MajorSeventh::Dominant7),
            extensions: vec![],
            alterations: vec![],
            omitted: vec![OmittedInterval::Fifth],
            additions: vec!["sus4".to_string()],
        }
    );
    
    // 7sus4: C F G B♭ (1 6 8 11)
    print_detailed_success_with_semitones(
        "Dominant 7sus4",
        &[0, 5, 7, 10],
        &ChordName {
            quality: ChordQuality::Suspended(MajorSeventh::Dominant7),
            extensions: vec![],
            alterations: vec![],
            omitted: vec![],
            additions: vec!["sus4".to_string()],
        }
    );
    
    // 11 omit5: C D F B♭ (1 3 6 11)
    print_detailed_success_with_semitones(
        "Dominant 11 omit5",
        &[0, 2, 5, 10],
        &ChordName {
            quality: ChordQuality::Major(MajorSeventh::Dominant7),
            extensions: vec![Extension::Ninth, Extension::Eleventh],
            alterations: vec![],
            omitted: vec![OmittedInterval::Third, OmittedInterval::Fifth],
            additions: vec![],
        }
    );
    
    // 7sus4: C F G B♭ (1 6 8 11)
    print_detailed_success_with_semitones(
        "Dominant 7sus4",
        &[0, 5, 7, 10],
        &ChordName {
            quality: ChordQuality::Suspended(MajorSeventh::Dominant7),
            extensions: vec![],
            alterations: vec![],
            omitted: vec![],
            additions: vec!["sus4".to_string()],
        }
    );
    
    // 11: C D F G B♭ (1 3 6 8 11)
    print_detailed_success_with_semitones(
        "Dominant 11",
        &[0, 2, 5, 7, 10],
        &ChordName {
            quality: ChordQuality::Major(MajorSeventh::Dominant7),
            extensions: vec![Extension::Ninth, Extension::Eleventh],
            alterations: vec![],
            omitted: vec![OmittedInterval::Third],
            additions: vec![],
        }
    );
}
