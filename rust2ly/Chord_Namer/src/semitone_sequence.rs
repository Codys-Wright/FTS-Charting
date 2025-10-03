use crate::{NoteName, Note, ChordName, ChordQuality, MajorSeventh, MinorSeventh};

/// Represents a semitone sequence with analysis capabilities
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SemitoneSequence {
    pub semitones: Vec<u32>,
    pub root_note: Note,
}

impl SemitoneSequence {
    /// Create a new semitone sequence with a root note (defaults to C)
    pub fn new(semitones: Vec<u32>, root_note: Option<Note>) -> Self {
        Self {
            semitones,
            root_note: root_note.unwrap_or_else(|| Note::new(NoteName::C)),
        }
    }

    /// Create a semitone sequence with C as root
    pub fn with_c_root(semitones: Vec<u32>) -> Self {
        Self::new(semitones, None)
    }

    /// Get the note names for this semitone sequence
    pub fn get_note_names(&self) -> Vec<String> {
        self.semitones.iter()
            .map(|&semitone| {
                let note = self.root_note.transpose(semitone as i8);
                format!("{}", note)
            })
            .collect()
    }

    /// Get the note names with proper interval-based spelling
    pub fn get_note_names_interval_based(&self) -> Vec<String> {
        self.semitones.iter()
            .map(|&semitone| {
                self.get_interval_based_note_name(semitone)
            })
            .collect()
    }

    /// Get a note name based on its interval relationship to the root
    fn get_interval_based_note_name(&self, semitones: u32) -> String {
        if semitones == 0 {
            return format!("{}", self.root_note);
        }

        // Get the interval to determine the letter name
        if let Some(interval) = crate::intervals::semitones_to_compound_interval(semitones) {
            let letter_name = self.get_letter_name_for_interval(interval.number);
            let accidental = self.get_accidental_for_interval(semitones, interval.number);
            
            if let Some(acc) = accidental {
                format!("{}{}", letter_name, acc)
            } else {
                letter_name
            }
        } else {
            // Fallback to simple transposition
            let note = self.root_note.transpose(semitones as i8);
            format!("{}", note)
        }
    }

    /// Get the letter name for an interval number
    fn get_letter_name_for_interval(&self, interval_number: crate::intervals::IntervalNumber) -> String {
        let root_letter = match self.root_note.name {
            crate::NoteName::C => "C",
            crate::NoteName::D => "D", 
            crate::NoteName::E => "E",
            crate::NoteName::F => "F",
            crate::NoteName::G => "G",
            crate::NoteName::A => "A",
            crate::NoteName::B => "B",
        };

        match interval_number {
            crate::intervals::IntervalNumber::Unison => root_letter.to_string(),
            crate::intervals::IntervalNumber::Second => self.next_letter(root_letter, 1),
            crate::intervals::IntervalNumber::Third => self.next_letter(root_letter, 2),
            crate::intervals::IntervalNumber::Fourth => self.next_letter(root_letter, 3),
            crate::intervals::IntervalNumber::Tritone => self.next_letter(root_letter, 3), // Same as 4th
            crate::intervals::IntervalNumber::Fifth => self.next_letter(root_letter, 4),
            crate::intervals::IntervalNumber::Sixth => self.next_letter(root_letter, 5),
            crate::intervals::IntervalNumber::Seventh => self.next_letter(root_letter, 6),
            crate::intervals::IntervalNumber::Octave => root_letter.to_string(),
            crate::intervals::IntervalNumber::Ninth => self.next_letter(root_letter, 1),
            crate::intervals::IntervalNumber::Tenth => self.next_letter(root_letter, 2),
            crate::intervals::IntervalNumber::Eleventh => self.next_letter(root_letter, 3),
            crate::intervals::IntervalNumber::Twelfth => self.next_letter(root_letter, 4),
            crate::intervals::IntervalNumber::Thirteenth => self.next_letter(root_letter, 5),
            crate::intervals::IntervalNumber::Fourteenth => self.next_letter(root_letter, 6),
        }
    }

    /// Get the next letter name by counting up the musical alphabet
    fn next_letter(&self, letter: &str, steps: u8) -> String {
        let letters = ["C", "D", "E", "F", "G", "A", "B"];
        let current_index = letters.iter().position(|&l| l == letter).unwrap_or(0);
        let new_index = (current_index + steps as usize) % 7;
        letters[new_index].to_string()
    }

    /// Get the accidental for an interval based on semitones and interval number
    fn get_accidental_for_interval(&self, semitones: u32, interval_number: crate::intervals::IntervalNumber) -> Option<String> {
        // Expected semitones for each interval in the key of C
        let expected_semitones = match interval_number {
            crate::intervals::IntervalNumber::Unison => 0,
            crate::intervals::IntervalNumber::Second => 2, // Major 2nd
            crate::intervals::IntervalNumber::Third => 4,  // Major 3rd
            crate::intervals::IntervalNumber::Fourth => 5, // Perfect 4th
            crate::intervals::IntervalNumber::Tritone => 6, // Augmented 4th
            crate::intervals::IntervalNumber::Fifth => 7,  // Perfect 5th
            crate::intervals::IntervalNumber::Sixth => 9,  // Major 6th
            crate::intervals::IntervalNumber::Seventh => 11, // Major 7th
            crate::intervals::IntervalNumber::Octave => 12,
            crate::intervals::IntervalNumber::Ninth => 14,  // Major 9th
            crate::intervals::IntervalNumber::Tenth => 16,  // Major 10th
            crate::intervals::IntervalNumber::Eleventh => 17, // Perfect 11th (but 18 semitones = #11)
            crate::intervals::IntervalNumber::Twelfth => 19, // Perfect 12th
            crate::intervals::IntervalNumber::Thirteenth => 21, // Major 13th
            crate::intervals::IntervalNumber::Fourteenth => 23, // Major 14th
        };

        let semitone_diff = semitones as i32 - expected_semitones as i32;
        
        match semitone_diff {
            0 => None, // No accidental needed
            1 => Some("♯".to_string()),
            2 => Some("𝄪".to_string()), // Double sharp
            -1 => Some("♭".to_string()),
            -2 => Some("𝄫".to_string()), // Double flat
            _ => None, // Fallback
        }
    }

    /// Get the intervals from the root for each note
    pub fn get_root_intervals(&self) -> Vec<String> {
        self.semitones.iter()
            .map(|&semitone| {
                crate::intervals::semitones_to_compound_interval(semitone)
                    .map(|interval| interval.to_compact())
                    .unwrap_or_else(|| format!("{} semitones", semitone))
            })
            .collect()
    }

    /// Get the intervals between consecutive notes
    pub fn get_stacked_intervals(&self) -> Vec<String> {
        if self.semitones.len() < 2 {
            return vec![];
        }

        let mut intervals = Vec::new();
        for i in 1..self.semitones.len() {
            let interval_semitones = self.semitones[i] as i32 - self.semitones[i-1] as i32;
            let interval_name = if interval_semitones >= 0 {
                crate::intervals::semitones_to_compound_interval(interval_semitones as u32)
                    .map(|interval| interval.to_compact())
                    .unwrap_or_else(|| format!("{} semitones", interval_semitones))
            } else {
                // Handle negative intervals (descending)
                format!("-{} semitones", -interval_semitones)
            };
            intervals.push(interval_name);
        }
        intervals
    }

    /// Get the interval name for dyads (two-note intervals)
    pub fn get_interval_name(&self) -> Option<String> {
        if self.semitones.len() != 2 {
            return None;
        }
        
        let interval_semitones = self.semitones[1] - self.semitones[0];
        crate::intervals::semitones_to_compound_interval(interval_semitones)
            .map(|interval| interval.to_compact())
    }

    /// Analyze the chord structure and return a ChordName
    pub fn analyze_chord(&self) -> Option<ChordName> {
        if self.semitones.is_empty() {
            return None;
        }

        // Convert to 0-based semitones for analysis
        let semitones: Vec<u32> = self.semitones.iter().map(|&s| s).collect();
        
        // Handle dyads (two-note intervals) - return None as they're not chords
        if semitones.len() == 2 {
            return None;
        }
        
        // Check for basic chord qualities
        let has_third = semitones.contains(&4) || semitones.contains(&3);
        let has_fifth = semitones.contains(&7);
        let has_seventh = semitones.contains(&10) || semitones.contains(&11);
        let has_ninth = semitones.contains(&14) || semitones.contains(&2);
        let has_flat_ninth = semitones.contains(&13);
        let has_sharp_ninth = semitones.contains(&15);
        let has_eleventh = semitones.contains(&17) || semitones.contains(&5);
        let has_sharp_eleventh = semitones.contains(&18);
        let has_thirteenth = semitones.contains(&21) || semitones.contains(&22) || (semitones.contains(&9) && has_seventh);
        let has_sixth = semitones.contains(&9);
        let has_sharp_fifth = semitones.contains(&8);
        let has_flat_fifth = semitones.contains(&6);
        
        // Determine chord quality
        let quality = if has_eleventh {
            // If chord has an 11th (4th), it's an 11th chord
            // But preserve the 3rd quality if present
            if semitones.contains(&3) {
                // Minor 11th chord
                if has_seventh {
                    if semitones.contains(&11) {
                        ChordQuality::Minor(MinorSeventh::MinorMajor7)
                    } else {
                        ChordQuality::Minor(MinorSeventh::Minor7)
                    }
                } else {
                    ChordQuality::Minor(MinorSeventh::None)
                }
            } else {
                // Major 11th chord
                if has_seventh {
                    if semitones.contains(&11) {
                        ChordQuality::Major(MajorSeventh::Major7)
                    } else {
                        ChordQuality::Major(MajorSeventh::Dominant7)
                    }
                } else {
                    ChordQuality::Major(MajorSeventh::None)
                }
            }
        } else if has_thirteenth {
            // If chord has a 13th (6th with 7th), it's a 13th chord
            if semitones.contains(&3) {
                // Minor 13th chord
                if has_seventh {
                    if semitones.contains(&11) {
                        ChordQuality::Minor(MinorSeventh::MinorMajor7)
                    } else {
                        ChordQuality::Minor(MinorSeventh::Minor7)
                    }
                } else {
                    ChordQuality::Minor(MinorSeventh::None)
                }
            } else {
                // Major 13th chord
                if has_seventh {
                    if semitones.contains(&11) {
                        ChordQuality::Major(MajorSeventh::Major7)
                    } else {
                        ChordQuality::Major(MajorSeventh::Dominant7)
                    }
                } else {
                    ChordQuality::Major(MajorSeventh::None)
                }
            }
        } else if semitones.contains(&3) {
            // Minor chords: check for minor 3rd first
            if has_seventh {
                if semitones.contains(&11) {
                    ChordQuality::Minor(MinorSeventh::MinorMajor7) // Minor major 7th
                } else {
                    ChordQuality::Minor(MinorSeventh::Minor7) // Minor 7th
                }
            } else {
                ChordQuality::Minor(MinorSeventh::None) // Minor triad
            }
        } else if semitones.contains(&5) || semitones.contains(&2) {
            // Suspended chords: sus4 (5 semitones) or sus2 (2 semitones) - but NOT if both are present
            if has_seventh {
                if semitones.contains(&11) {
                    ChordQuality::Suspended(MajorSeventh::Major7)
                } else {
                    ChordQuality::Suspended(MajorSeventh::Dominant7)
                }
            } else {
                ChordQuality::Suspended(MajorSeventh::None)
            }
        } else if semitones.contains(&4) {
            if has_seventh {
                if semitones.contains(&11) {
                    ChordQuality::Major(MajorSeventh::Major7)
                } else {
                    ChordQuality::Major(MajorSeventh::Dominant7)
                }
            } else {
                ChordQuality::Major(MajorSeventh::None)
            }
        } else if has_seventh {
            // No 3rd but has 7th - this is a power chord with 7th
            if semitones.contains(&11) {
                ChordQuality::Major(MajorSeventh::Major7)
            } else {
                ChordQuality::Major(MajorSeventh::Dominant7)
            }
        } else {
            return None; // Unrecognized chord
        };
        
        // Track if this is a suspended chord (has sus2 or sus4)
        // A chord is suspended if it has a 2nd or 4th but no 3rd
        // But not if it has both 2nd and 4th (that's an 11th chord)
        let is_suspended = !has_third && (semitones.contains(&5) || semitones.contains(&2)) && !(semitones.contains(&5) && semitones.contains(&2));
        
        let mut chord_name = ChordName::new(quality);
        
        // Add extensions based on what's actually present
        // Only natural ninths contribute to chord quality - altered ninths are just alterations
        // Don't add 9th as extension if it's being added as an addition (for 6/9 chords)
        if has_ninth && has_seventh {
            chord_name = chord_name.with_ninth();
        }
        
        if has_eleventh {
            chord_name = chord_name.with_eleventh();
        }
        
        if has_thirteenth {
            chord_name = chord_name.with_thirteenth();
        }
        
        // Add sharp 11th alteration
        if has_sharp_eleventh {
            chord_name = chord_name.with_alteration(crate::Alteration::Sharp11);
        }
        
        // Add flat 9th alteration
        if has_flat_ninth {
            chord_name = chord_name.with_alteration(crate::Alteration::Flat9);
        }
        
        // Add sharp 9th alteration
        if has_sharp_ninth {
            chord_name = chord_name.with_alteration(crate::Alteration::Sharp9);
        }
        
        // Add sharp 5th alteration
        if has_sharp_fifth {
            chord_name = chord_name.with_alteration(crate::Alteration::Sharp5);
        }
        
        // Add flat 5th alteration
        if has_flat_fifth {
            chord_name = chord_name.with_alteration(crate::Alteration::Flat5);
        }
        
        // Add 6th as addition if no 7th
        if has_sixth && !has_seventh {
            chord_name = chord_name.with_addition("6");
        }
        
        // Add 9th as addition if no 7th (for 6/9 chords)
        if has_ninth && !has_seventh {
            chord_name = chord_name.with_addition("9");
        }
        
        // Add omissions only for truly missing intervals
        // Don't add omissions for "add" chords where the missing interval is implied
        // Don't add omissions when alterations replace those intervals
        // Don't add omit3 for suspended chords (sus2, sus4) as the suspension is intentional
        if !has_third && !is_suspended {
            chord_name = chord_name.omit_third();
        }
        
        if !has_fifth && !has_sharp_fifth && !has_flat_fifth {
            chord_name = chord_name.omit_fifth();
        }
        
        // Add suspension information for suspended chords
        if is_suspended {
            if semitones.contains(&5) {
                chord_name = chord_name.with_addition("sus4");
            } else if semitones.contains(&2) {
                chord_name = chord_name.with_addition("sus2");
            }
        }
        
        // Add omitted 9th for 11th chords when 9th is not present
        if has_eleventh && !has_ninth && !has_flat_ninth && !has_sharp_ninth {
            chord_name = chord_name.omit_ninth();
        }
        
        // Don't add "omit3" for "maj9" chords - the 3rd is present
        
        Some(chord_name)
    }

    /// Print a detailed analysis of the semitone sequence
    pub fn print_analysis(&self) {
        println!("=== SEMITONE SEQUENCE ANALYSIS ===");
        println!("Semitones: {:?}", self.semitones);
        println!("Root note: {}", self.root_note);
        println!();
        
        println!("Note names: {}", self.get_note_names().join(" "));
        println!();
        
        println!("Intervals from root:");
        for (i, interval) in self.get_root_intervals().iter().enumerate() {
            println!("  {} -> {}", self.semitones[i], interval);
        }
        println!();
        
        if self.semitones.len() > 1 {
            println!("Stacked intervals:");
            for (i, interval) in self.get_stacked_intervals().iter().enumerate() {
                println!("  {} to {} -> {}", 
                    self.semitones[i], 
                    self.semitones[i+1], 
                    interval);
            }
            println!();
        }
        
        if let Some(chord) = self.analyze_chord() {
            println!("Recognized chord: {}", chord.expanded_name_colored());
        } else {
            println!("Could not recognize chord structure");
        }
    }
}

/// Helper function to create and analyze a semitone sequence
pub fn analyze_semitone_sequence(semitones: Vec<u32>, root_note: Option<Note>) {
    let sequence = SemitoneSequence::new(semitones, root_note);
    sequence.print_analysis();
}

/// Helper function to analyze with C root
pub fn analyze_with_c_root(semitones: Vec<u32>) {
    analyze_semitone_sequence(semitones, None);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_major_triad() {
        let sequence = SemitoneSequence::with_c_root(vec![0, 4, 7]);
        assert_eq!(sequence.get_note_names(), vec!["C", "E", "G"]);
        assert_eq!(sequence.get_root_intervals(), vec!["Unison", "Major 3rd", "Perfect 5th"]);
    }

    #[test]
    fn test_major_7th() {
        let sequence = SemitoneSequence::with_c_root(vec![0, 4, 7, 11]);
        assert_eq!(sequence.get_note_names(), vec!["C", "E", "G", "B"]);
        assert_eq!(sequence.get_root_intervals(), vec!["Unison", "Major 3rd", "Perfect 5th", "Major 7th"]);
    }

    #[test]
    fn test_major_11th() {
        let sequence = SemitoneSequence::with_c_root(vec![0, 4, 7, 11, 14, 17]);
        assert_eq!(sequence.get_note_names(), vec!["C", "E", "G", "B", "D", "F"]);
        assert_eq!(sequence.get_root_intervals(), vec!["Unison", "Major 3rd", "Perfect 5th", "Major 7th", "Major 9th", "Perfect 11th"]);
    }
}
