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
fn test_altered_chords() {
    use chord_namer::{ChordQuality, MajorSeventh, Extension, Alteration, OmittedInterval};
    
    println!("\n=== TESTING ALTERED CHORDS ===");
    
    // 7b9 omit5: C E B♭ D♭ (1 2 5 11)
    print_detailed_success_with_semitones(
        "Dominant 7b9 omit5",
        &[0, 4, 10, 13],
        &ChordName {
            quality: ChordQuality::Major(MajorSeventh::Dominant7),
            extensions: vec![],
            alterations: vec![Alteration::Flat9],
            omitted: vec![OmittedInterval::Fifth],
            additions: vec![],
        }
    );
    
    // 7b9: C E G B♭ D♭ (1 2 5 8 11)
    print_detailed_success_with_semitones(
        "Dominant 7b9",
        &[0, 4, 7, 10, 13],
        &ChordName {
            quality: ChordQuality::Major(MajorSeventh::Dominant7),
            extensions: vec![],
            alterations: vec![Alteration::Flat9],
            omitted: vec![],
            additions: vec![],
        }
    );
    
    // 7b9#11: C E G B♭ D♭ F♯ (1 2 5 7 8 11)
    print_detailed_success_with_semitones(
        "Dominant 7b9#11",
        &[0, 4, 7, 10, 13, 18],
        &ChordName {
            quality: ChordQuality::Major(MajorSeventh::Dominant7),
            extensions: vec![],
            alterations: vec![Alteration::Flat9, Alteration::Sharp11],
            omitted: vec![],
            additions: vec![],
        }
    );
    
    // 7#9 omit5: C E B♭ D♯ (1 4 5 11)
    print_detailed_success_with_semitones(
        "Dominant 7#9 omit5",
        &[0, 4, 10, 15],
        &ChordName {
            quality: ChordQuality::Major(MajorSeventh::Dominant7),
            extensions: vec![],
            alterations: vec![Alteration::Sharp9],
            omitted: vec![OmittedInterval::Fifth],
            additions: vec![],
        }
    );
    
    // 7#9: C E G B♭ D♯ (1 4 5 8 11)
    print_detailed_success_with_semitones(
        "Dominant 7#9",
        &[0, 4, 7, 10, 15],
        &ChordName {
            quality: ChordQuality::Major(MajorSeventh::Dominant7),
            extensions: vec![],
            alterations: vec![Alteration::Sharp9],
            omitted: vec![],
            additions: vec![],
        }
    );
    
    // 7#5#9: C E G♯ B♭ D♯ (1 4 5 9 11)
    print_detailed_success_with_semitones(
        "Dominant 7#5#9",
        &[0, 4, 8, 10, 15],
        &ChordName {
            quality: ChordQuality::Major(MajorSeventh::Dominant7),
            extensions: vec![],
            alterations: vec![Alteration::Sharp5, Alteration::Sharp9],
            omitted: vec![],
            additions: vec![],
        }
    );
    
    // 7#9#11: C E G B♭ D♯ F♯ (1 4 5 7 8 11)
    print_detailed_success_with_semitones(
        "Dominant 7#9#11",
        &[0, 4, 7, 10, 15, 18],
        &ChordName {
            quality: ChordQuality::Major(MajorSeventh::Dominant7),
            extensions: vec![],
            alterations: vec![Alteration::Sharp9, Alteration::Sharp11],
            omitted: vec![],
            additions: vec![],
        }
    );
    
    // 13b9: C E G B♭ D♭ A (1 2 5 8 10 11)
    print_detailed_success_with_semitones(
        "Dominant 13b9",
        &[0, 4, 7, 10, 13, 21],
        &ChordName {
            quality: ChordQuality::Major(MajorSeventh::Dominant7),
            extensions: vec![Extension::Thirteenth],
            alterations: vec![Alteration::Flat9],
            omitted: vec![],
            additions: vec![],
        }
    );
    
    // 13#11: C E G B♭ D F♯ A (1 3 5 7 8 10 11)
    print_detailed_success_with_semitones(
        "Dominant 13#11",
        &[0, 4, 7, 10, 14, 18, 21],
        &ChordName {
            quality: ChordQuality::Major(MajorSeventh::Dominant7),
            extensions: vec![Extension::Ninth, Extension::Thirteenth],
            alterations: vec![Alteration::Sharp11],
            omitted: vec![],
            additions: vec![],
        }
    );
}
