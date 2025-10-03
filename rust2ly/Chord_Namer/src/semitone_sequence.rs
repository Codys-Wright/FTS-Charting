use crate::{NoteName, Note, ChordName, ChordQuality, MajorSeventh};

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

    /// Analyze the chord structure and return a ChordName
    pub fn analyze_chord(&self) -> Option<ChordName> {
        if self.semitones.is_empty() {
            return None;
        }

        // Convert to 0-based semitones for analysis
        let semitones: Vec<u32> = self.semitones.iter().map(|&s| s).collect();
        
        // Check for basic chord qualities
        let has_third = semitones.contains(&4) || semitones.contains(&3);
        let has_fifth = semitones.contains(&7);
        let has_seventh = semitones.contains(&10) || semitones.contains(&11);
        let has_ninth = semitones.contains(&14) || semitones.contains(&15);
        let has_eleventh = semitones.contains(&17);
        let has_thirteenth = semitones.contains(&21) || semitones.contains(&22);
        let has_sixth = semitones.contains(&9);
        
        // Determine chord quality
        let quality = if semitones.contains(&4) {
            if has_seventh {
                if semitones.contains(&11) {
                    ChordQuality::Major(MajorSeventh::Major7)
                } else {
                    ChordQuality::Major(MajorSeventh::Dominant7)
                }
            } else {
                ChordQuality::Major(MajorSeventh::None)
            }
        } else if semitones.contains(&3) {
            ChordQuality::Major(MajorSeventh::None) // Minor - simplified for now
        } else {
            return None; // Unrecognized chord
        };
        
        let mut chord_name = ChordName::new(quality);
        
        // Add extensions based on what's actually present
        if has_ninth {
            chord_name = chord_name.with_ninth();
        }
        
        if has_eleventh {
            chord_name = chord_name.with_eleventh();
        }
        
        if has_thirteenth {
            chord_name = chord_name.with_thirteenth();
        }
        
        // Add 6th as addition if no 7th
        if has_sixth && !has_seventh {
            chord_name = chord_name.with_addition("6");
        }
        
        // Add omissions only for truly missing intervals
        // Don't add omissions for "add" chords where the missing interval is implied
        if !has_third {
            chord_name = chord_name.omit_third();
        }
        
        if !has_fifth {
            chord_name = chord_name.omit_fifth();
        }
        
        // Don't add "omit9" for "add11" chords - the missing 9th is implied
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
