// Simple interval definitions and mappings
// Maps semitone counts to interval names

use std::fmt;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum IntervalQuality {
    Perfect,
    Major,
    Minor,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum IntervalNumber {
    Unison,     // 1
    Second,     // 2
    Third,      // 3
    Fourth,     // 4
    Fifth,      // 5
    Sixth,      // 6
    Seventh,    // 7
    Octave,     // 8
    Ninth,      // 9
    Eleventh,   // 11
    Thirteenth, // 13
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Interval {
    pub quality: IntervalQuality,
    pub number: IntervalNumber,
    pub semitones: u8,
    pub octaves_up: u8, // How many octaves up from the base interval
}

impl Interval {
    pub fn new(quality: IntervalQuality, number: IntervalNumber, semitones: u8, octaves_up: u8) -> Self {
        Self {
            quality,
            number,
            semitones,
            octaves_up,
        }
    }
}

impl fmt::Display for IntervalQuality {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            IntervalQuality::Perfect => write!(f, "P"),
            IntervalQuality::Major => write!(f, "M"),
            IntervalQuality::Minor => write!(f, "m"),
        }
    }
}

impl fmt::Display for IntervalNumber {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            IntervalNumber::Unison => write!(f, "1"),
            IntervalNumber::Second => write!(f, "2"),
            IntervalNumber::Third => write!(f, "3"),
            IntervalNumber::Fourth => write!(f, "4"),
            IntervalNumber::Fifth => write!(f, "5"),
            IntervalNumber::Sixth => write!(f, "6"),
            IntervalNumber::Seventh => write!(f, "7"),
            IntervalNumber::Octave => write!(f, "8"),
            IntervalNumber::Ninth => write!(f, "9"),
            IntervalNumber::Eleventh => write!(f, "11"),
            IntervalNumber::Thirteenth => write!(f, "13"),
        }
    }
}

impl fmt::Display for Interval {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.octaves_up > 0 {
            write!(f, "{}{} ({} octave{} up)", 
                   self.quality, 
                   self.number, 
                   self.octaves_up,
                   if self.octaves_up == 1 { "" } else { "s" })
        } else {
            write!(f, "{}{}", self.quality, self.number)
        }
    }
}

impl Interval {
    /// Compact musical notation display (m3, 5, 7, etc.)
    pub fn to_compact(&self) -> String {
        let quality_symbol = match self.quality {
            IntervalQuality::Perfect => "",  // No prefix for perfect intervals
            IntervalQuality::Major => "",    // No prefix for major intervals
            IntervalQuality::Minor => "m",   // Only minor intervals get "m" prefix
        };
        
        let number_symbol = match self.number {
            IntervalNumber::Unison => "1st",
            IntervalNumber::Second => "2nd",
            IntervalNumber::Third => "3rd",
            IntervalNumber::Fourth => "4th",
            IntervalNumber::Fifth => "5th",
            IntervalNumber::Sixth => "6th",
            IntervalNumber::Seventh => "7th",
            IntervalNumber::Octave => "8th",
            IntervalNumber::Ninth => "9th",
            IntervalNumber::Eleventh => "11th",
            IntervalNumber::Thirteenth => "13th",
        };
        
        if self.octaves_up > 0 {
            format!("{}{} (+{} octave{})", 
                   quality_symbol, 
                   number_symbol, 
                   self.octaves_up,
                   if self.octaves_up == 1 { "" } else { "s" })
        } else {
            format!("{}{}", quality_symbol, number_symbol)
        }
    }
}

// Helper function to get a nice interval name
impl Interval {
    pub fn to_nice_name(&self) -> String {
        let base_name = match (self.quality, self.number) {
            (IntervalQuality::Perfect, IntervalNumber::Unison) => if self.octaves_up == 0 { "Unison" } else { "Perfect Unison" },
            (IntervalQuality::Minor, IntervalNumber::Second) => "Minor 2nd",
            (IntervalQuality::Major, IntervalNumber::Second) => "Major 2nd",
            (IntervalQuality::Minor, IntervalNumber::Third) => "Minor 3rd",
            (IntervalQuality::Major, IntervalNumber::Third) => "Major 3rd",
            (IntervalQuality::Perfect, IntervalNumber::Fourth) => "Perfect 4th",
            (IntervalQuality::Perfect, IntervalNumber::Fifth) => "Perfect 5th",
            (IntervalQuality::Minor, IntervalNumber::Sixth) => "Minor 6th",
            (IntervalQuality::Major, IntervalNumber::Sixth) => "Major 6th",
            (IntervalQuality::Minor, IntervalNumber::Seventh) => "Minor 7th",
            (IntervalQuality::Major, IntervalNumber::Seventh) => "Major 7th",
            (IntervalQuality::Perfect, IntervalNumber::Octave) => "Perfect Octave",
            (IntervalQuality::Perfect, IntervalNumber::Ninth) => "Perfect 9th",
            (IntervalQuality::Minor, IntervalNumber::Ninth) => "Minor 9th",
            (IntervalQuality::Major, IntervalNumber::Ninth) => "Major 9th",
            (IntervalQuality::Perfect, IntervalNumber::Eleventh) => "Perfect 11th",
            (IntervalQuality::Minor, IntervalNumber::Thirteenth) => "Minor 13th",
            (IntervalQuality::Major, IntervalNumber::Thirteenth) => "Major 13th",
            _ => return format!("{}{}", self.quality, self.number),
        };
        
        if self.octaves_up == 0 {
            base_name.to_string()
        } else if self.octaves_up == 1 {
            // For compound intervals, show the relationship
            match self.number {
                IntervalNumber::Ninth => format!("{} (+1 octave)", base_name),
                IntervalNumber::Eleventh => format!("{} (+1 octave)", base_name),
                IntervalNumber::Thirteenth => format!("{} (+1 octave)", base_name),
                _ => format!("{} (+1 octave)", base_name),
            }
        } else {
            // For multiple octaves, show the count
            format!("{} (+{} octaves)", base_name, self.octaves_up)
        }
    }
}

// Map semitone counts to base intervals (what's in the first octave)
pub fn semitones_to_base_interval(semitones: u32) -> Option<Interval> {
    // Handle the first octave (0-11 semitones)
    let base_interval = match semitones % 12 {
        0 => Some((IntervalQuality::Perfect, IntervalNumber::Unison)),
        1 => Some((IntervalQuality::Minor, IntervalNumber::Second)),
        2 => Some((IntervalQuality::Major, IntervalNumber::Second)),
        3 => Some((IntervalQuality::Minor, IntervalNumber::Third)),
        4 => Some((IntervalQuality::Major, IntervalNumber::Third)),
        5 => Some((IntervalQuality::Perfect, IntervalNumber::Fourth)),
        6 => Some((IntervalQuality::Perfect, IntervalNumber::Fourth)), // Tritone (A4/d5)
        7 => Some((IntervalQuality::Perfect, IntervalNumber::Fifth)),
        8 => Some((IntervalQuality::Minor, IntervalNumber::Sixth)),
        9 => Some((IntervalQuality::Major, IntervalNumber::Sixth)),
        10 => Some((IntervalQuality::Minor, IntervalNumber::Seventh)),
        11 => Some((IntervalQuality::Major, IntervalNumber::Seventh)),
        _ => None,
    };
    
    if let Some((quality, number)) = base_interval {
        let octaves_up = (semitones / 12) as u8;
        Some(Interval::new(quality, number, semitones as u8, octaves_up))
    } else {
        None
    }
}

// Map semitone counts to proper compound interval names (9th, 11th, 13th)
pub fn semitones_to_compound_interval(semitones: u32) -> Option<Interval> {
    let base_semitones = semitones % 12;
    let octaves_up = semitones / 12;
    
    if octaves_up == 0 {
        // Within first octave, use base intervals
        semitones_to_base_interval(semitones)
    } else if octaves_up == 1 {
        // For first compound interval, map to proper compound interval numbers
        let compound_interval = match base_semitones {
            0 => Some((IntervalQuality::Perfect, IntervalNumber::Ninth)), // P9
            1 => Some((IntervalQuality::Minor, IntervalNumber::Ninth)), // m9
            2 => Some((IntervalQuality::Major, IntervalNumber::Ninth)), // M9
            3 => Some((IntervalQuality::Minor, IntervalNumber::Third)), // m3 (1 octave up)
            4 => Some((IntervalQuality::Major, IntervalNumber::Third)), // M3 (1 octave up)
            5 => Some((IntervalQuality::Perfect, IntervalNumber::Eleventh)), // P11
            6 => Some((IntervalQuality::Perfect, IntervalNumber::Fourth)), // P4 (1 octave up)
            7 => Some((IntervalQuality::Perfect, IntervalNumber::Fifth)), // P5 (1 octave up)
            8 => Some((IntervalQuality::Minor, IntervalNumber::Thirteenth)), // m13
            9 => Some((IntervalQuality::Major, IntervalNumber::Thirteenth)), // M13
            10 => Some((IntervalQuality::Minor, IntervalNumber::Seventh)), // m7 (1 octave up)
            11 => Some((IntervalQuality::Major, IntervalNumber::Seventh)), // M7 (1 octave up)
            _ => None,
        };
        
        if let Some((quality, number)) = compound_interval {
            Some(Interval::new(quality, number, semitones as u8, octaves_up as u8))
        } else {
            None
        }
    } else {
        // For multiple octaves up, use base interval with octave count
        let base_interval = match base_semitones {
            0 => Some((IntervalQuality::Perfect, IntervalNumber::Unison)),
            1 => Some((IntervalQuality::Minor, IntervalNumber::Second)),
            2 => Some((IntervalQuality::Major, IntervalNumber::Second)),
            3 => Some((IntervalQuality::Minor, IntervalNumber::Third)),
            4 => Some((IntervalQuality::Major, IntervalNumber::Third)),
            5 => Some((IntervalQuality::Perfect, IntervalNumber::Fourth)),
            6 => Some((IntervalQuality::Perfect, IntervalNumber::Fourth)), // Tritone
            7 => Some((IntervalQuality::Perfect, IntervalNumber::Fifth)),
            8 => Some((IntervalQuality::Minor, IntervalNumber::Sixth)),
            9 => Some((IntervalQuality::Major, IntervalNumber::Sixth)),
            10 => Some((IntervalQuality::Minor, IntervalNumber::Seventh)),
            11 => Some((IntervalQuality::Major, IntervalNumber::Seventh)),
            _ => None,
        };
        
        if let Some((quality, number)) = base_interval {
            Some(Interval::new(quality, number, semitones as u8, octaves_up as u8))
        } else {
            None
        }
    }
}

// Get interval name with proper description - handles any number of semitones
pub fn get_interval_name(semitones: u32) -> String {
    if let Some(interval) = semitones_to_compound_interval(semitones) {
        let base_name = match (interval.quality, interval.number) {
            (IntervalQuality::Perfect, IntervalNumber::Unison) => "Perfect Unison (P1)",
            (IntervalQuality::Minor, IntervalNumber::Second) => "Minor 2nd (m2)",
            (IntervalQuality::Major, IntervalNumber::Second) => "Major 2nd (M2)",
            (IntervalQuality::Minor, IntervalNumber::Third) => "Minor 3rd (m3)",
            (IntervalQuality::Major, IntervalNumber::Third) => "Major 3rd (M3)",
            (IntervalQuality::Perfect, IntervalNumber::Fourth) => "Perfect 4th (P4)",
            (IntervalQuality::Perfect, IntervalNumber::Fifth) => "Perfect 5th (P5)",
            (IntervalQuality::Minor, IntervalNumber::Sixth) => "Minor 6th (m6)",
            (IntervalQuality::Major, IntervalNumber::Sixth) => "Major 6th (M6)",
            (IntervalQuality::Minor, IntervalNumber::Seventh) => "Minor 7th (m7)",
            (IntervalQuality::Major, IntervalNumber::Seventh) => "Major 7th (M7)",
            (IntervalQuality::Perfect, IntervalNumber::Octave) => "Perfect Octave (P8)",
            (IntervalQuality::Perfect, IntervalNumber::Ninth) => "Perfect 9th (P9)",
            (IntervalQuality::Minor, IntervalNumber::Ninth) => "Minor 9th (m9)",
            (IntervalQuality::Major, IntervalNumber::Ninth) => "Major 9th (M9)",
            (IntervalQuality::Perfect, IntervalNumber::Eleventh) => "Perfect 11th (P11)",
            (IntervalQuality::Minor, IntervalNumber::Thirteenth) => "Minor 13th (m13)",
            (IntervalQuality::Major, IntervalNumber::Thirteenth) => "Major 13th (M13)",
            _ => "Unknown Interval",
        };
        
        if interval.octaves_up == 0 {
            base_name.to_string()
        } else if interval.octaves_up == 1 {
            // Special compound interval names
            match interval.number {
                IntervalNumber::Ninth => format!("{} = {} + 1 octave", base_name, base_name.replace("9th", "2nd")),
                IntervalNumber::Eleventh => format!("{} = {} + 1 octave", base_name, base_name.replace("11th", "4th")),
                IntervalNumber::Thirteenth => format!("{} = {} + 1 octave", base_name, base_name.replace("13th", "6th")),
                _ => format!("{} (1 octave up)", base_name),
            }
        } else {
            format!("{} ({} octaves up)", base_name, interval.octaves_up)
        }
    } else {
        format!("Unknown interval ({} semitones)", semitones)
    }
}

// Test function to show all intervals
pub fn test_all_intervals() {
    println!("=== INTERVAL MAPPINGS ===");
    println!("Semitones -> Interval Names");
    println!();
    
    // Show first two octaves to demonstrate the looping
    for semitones in 0..=24 {
        if let Some(interval) = semitones_to_compound_interval(semitones) {
            let nice_name = interval.to_nice_name();
            println!("{:2} semitones: {}", semitones, nice_name);
        }
    }
    
    println!("\n=== EXTENDED RANGE TEST ===");
    println!("Testing higher semitone counts:");
    
    // Test some higher semitone counts to show the looping works
    let test_semitones = vec![36, 48, 60, 72];
    for semitones in test_semitones {
        if let Some(interval) = semitones_to_compound_interval(semitones) {
            let nice_name = interval.to_nice_name();
            println!("{:2} semitones: {}", semitones, nice_name);
        }
    }
}