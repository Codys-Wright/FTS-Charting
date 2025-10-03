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
        assert_eq!(actual_chord.additions, expected_chord.additions, "Additions mismatch for {}", test_name);
    } else {
        panic!("Failed to recognize chord for {}", test_name);
    }
}

#[test]
fn test_minor_chords() {
    use chord_namer::{ChordQuality, MajorSeventh, MinorSeventh, Extension, OmittedInterval};
    
    println!("\n=== TESTING MINOR CHORDS ===");
    
    // Basic minor triad: C E♭ G (1 4 8)
    print_detailed_success_with_semitones(
        "Minor",
        &[0, 3, 7],
        &ChordName {
            quality: ChordQuality::Minor(MinorSeventh::None),
            extensions: vec![],
            alterations: vec![],
            omitted: vec![],
            additions: vec![],
        }
    );
    
    // Minor 7th omit5: C E♭ B♭ (1 4 11)
    print_detailed_success_with_semitones(
        "Minor 7th omit5",
        &[0, 3, 10],
        &ChordName {
            quality: ChordQuality::Minor(MinorSeventh::Minor7),
            extensions: vec![],
            alterations: vec![],
            omitted: vec![OmittedInterval::Fifth],
            additions: vec![],
        }
    );
    
    // Minor 7th: C E♭ G B♭ (1 4 8 11)
    print_detailed_success_with_semitones(
        "Minor 7th",
        &[0, 3, 7, 10],
        &ChordName {
            quality: ChordQuality::Minor(MinorSeventh::Minor7),
            extensions: vec![],
            alterations: vec![],
            omitted: vec![],
            additions: vec![],
        }
    );
    
    // Minor/Major 7th omit5: C E♭ B (1 4 12)
    print_detailed_success_with_semitones(
        "Minor/Major 7th omit5",
        &[0, 3, 11],
        &ChordName {
            quality: ChordQuality::Minor(MinorSeventh::MinorMajor7),
            extensions: vec![],
            alterations: vec![],
            omitted: vec![OmittedInterval::Fifth],
            additions: vec![],
        }
    );
    
    // Minor/Major 7th: C E♭ G B (1 4 8 12)
    print_detailed_success_with_semitones(
        "Minor/Major 7th",
        &[0, 3, 7, 11],
        &ChordName {
            quality: ChordQuality::Minor(MinorSeventh::MinorMajor7),
            extensions: vec![],
            alterations: vec![],
            omitted: vec![],
            additions: vec![],
        }
    );
    
    // Minor/Major 9th omit5: C D E♭ B (1 3 4 12)
    print_detailed_success_with_semitones(
        "Minor/Major 9th omit5",
        &[0, 2, 3, 11],
        &ChordName {
            quality: ChordQuality::Minor(MinorSeventh::MinorMajor7),
            extensions: vec![Extension::Ninth],
            alterations: vec![],
            omitted: vec![OmittedInterval::Fifth],
            additions: vec![],
        }
    );
    
    // Minor/Major 9th: C D E♭ G B (1 3 4 8 12)
    print_detailed_success_with_semitones(
        "Minor/Major 9th",
        &[0, 2, 3, 7, 11],
        &ChordName {
            quality: ChordQuality::Minor(MinorSeventh::MinorMajor7),
            extensions: vec![Extension::Ninth],
            alterations: vec![],
            omitted: vec![],
            additions: vec![],
        }
    );
    
    // Minor 9th omit5: C D E♭ B♭ (1 3 4 11)
    print_detailed_success_with_semitones(
        "Minor 9th omit5",
        &[0, 2, 3, 10],
        &ChordName {
            quality: ChordQuality::Minor(MinorSeventh::Minor7),
            extensions: vec![Extension::Ninth],
            alterations: vec![],
            omitted: vec![OmittedInterval::Fifth],
            additions: vec![],
        }
    );
    
    // Minor 9th: C D E♭ G B♭ (1 3 4 8 11)
    print_detailed_success_with_semitones(
        "Minor 9th",
        &[0, 2, 3, 7, 10],
        &ChordName {
            quality: ChordQuality::Minor(MinorSeventh::Minor7),
            extensions: vec![Extension::Ninth],
            alterations: vec![],
            omitted: vec![],
            additions: vec![],
        }
    );
    
    // Minor 11th omit5: C D E♭ F B♭ (1 3 4 6 11)
    print_detailed_success_with_semitones(
        "Minor 11th omit5",
        &[0, 2, 3, 5, 10],
        &ChordName {
            quality: ChordQuality::Minor(MinorSeventh::Minor7),
            extensions: vec![Extension::Ninth, Extension::Eleventh],
            alterations: vec![],
            omitted: vec![OmittedInterval::Fifth],
            additions: vec![],
        }
    );
    
    // Minor 11th omit9: C E♭ F G B♭ (1 4 6 8 11)
    print_detailed_success_with_semitones(
        "Minor 11th omit9",
        &[0, 3, 5, 7, 10],
        &ChordName {
            quality: ChordQuality::Minor(MinorSeventh::Minor7),
            extensions: vec![Extension::Ninth, Extension::Eleventh],
            alterations: vec![],
            omitted: vec![OmittedInterval::Ninth],
            additions: vec![],
        }
    );
    
    // Minor 11th: C D E♭ F G B♭ (1 3 4 6 8 11)
    print_detailed_success_with_semitones(
        "Minor 11th",
        &[0, 2, 3, 5, 7, 10],
        &ChordName {
            quality: ChordQuality::Minor(MinorSeventh::Minor7),
            extensions: vec![Extension::Ninth, Extension::Eleventh],
            alterations: vec![],
            omitted: vec![],
            additions: vec![],
        }
    );
    
    // Minor 13th omit5: C D E♭ F A B♭ (1 3 4 6 10 11)
    print_detailed_success_with_semitones(
        "Minor 13th omit5",
        &[0, 2, 3, 5, 9, 10],
        &ChordName {
            quality: ChordQuality::Minor(MinorSeventh::Minor7),
            extensions: vec![Extension::Ninth, Extension::Eleventh, Extension::Thirteenth],
            alterations: vec![],
            omitted: vec![OmittedInterval::Fifth],
            additions: vec![],
        }
    );
    
    // Minor 13th omit9: C E♭ F G A B♭ (1 4 6 8 10 11)
    print_detailed_success_with_semitones(
        "Minor 13th omit9",
        &[0, 3, 5, 7, 9, 10],
        &ChordName {
            quality: ChordQuality::Minor(MinorSeventh::Minor7),
            extensions: vec![Extension::Ninth, Extension::Eleventh, Extension::Thirteenth],
            alterations: vec![],
            omitted: vec![OmittedInterval::Ninth],
            additions: vec![],
        }
    );
    
    // Minor 13th: C D E♭ F G A B♭ (1 3 4 6 8 10 11)
    print_detailed_success_with_semitones(
        "Minor 13th",
        &[0, 2, 3, 5, 7, 9, 10],
        &ChordName {
            quality: ChordQuality::Minor(MinorSeventh::Minor7),
            extensions: vec![Extension::Ninth, Extension::Eleventh, Extension::Thirteenth],
            alterations: vec![],
            omitted: vec![],
            additions: vec![],
        }
    );
    
    // Minor 6th: C E♭ G A (1 4 8 10)
    print_detailed_success_with_semitones(
        "Minor 6th",
        &[0, 3, 7, 9],
        &ChordName {
            quality: ChordQuality::Minor(MinorSeventh::None),
            extensions: vec![],
            alterations: vec![],
            omitted: vec![],
            additions: vec!["6".to_string()],
        }
    );
    
    // Minor 6/9 omit5: C D E♭ A (1 3 4 10)
    print_detailed_success_with_semitones(
        "Minor 6/9 omit5",
        &[0, 2, 3, 9],
        &ChordName {
            quality: ChordQuality::Minor(MinorSeventh::None),
            extensions: vec![],
            alterations: vec![],
            omitted: vec![OmittedInterval::Fifth],
            additions: vec!["6".to_string(), "9".to_string()],
        }
    );
    
    // Minor 6/9: C D E♭ G A (1 3 4 8 10)
    print_detailed_success_with_semitones(
        "Minor 6/9",
        &[0, 2, 3, 7, 9],
        &ChordName {
            quality: ChordQuality::Minor(MinorSeventh::None),
            extensions: vec![],
            alterations: vec![],
            omitted: vec![],
            additions: vec!["6".to_string(), "9".to_string()],
        }
    );
}
