// Independent Chord Namer - Custom Types
// This will be completely independent of lilypond-rs

use std::fmt;
use colored::*;

mod intervals;
pub mod semitone_sequence;
use intervals::{IntervalQuality, IntervalNumber, Interval};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum NoteName {
    A, B, C, D, E, F, G,
}

impl fmt::Display for NoteName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            NoteName::A => write!(f, "A"),
            NoteName::B => write!(f, "B"),
            NoteName::C => write!(f, "C"),
            NoteName::D => write!(f, "D"),
            NoteName::E => write!(f, "E"),
            NoteName::F => write!(f, "F"),
            NoteName::G => write!(f, "G"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum AccidentalType {
    Sharp,
    Flat,
}

impl fmt::Display for AccidentalType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            AccidentalType::Sharp => write!(f, "♯"),
            AccidentalType::Flat => write!(f, "♭"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Accidental {
    pub accidental_type: AccidentalType,
    pub count: u8, // 1 = single, 2 = double, 3 = triple, etc.
}

impl Accidental {
    pub fn new(accidental_type: AccidentalType, count: u8) -> Self {
        Self {
            accidental_type,
            count: count.max(1), // Ensure at least 1
        }
    }
    
    pub fn sharp(count: u8) -> Self {
        Self::new(AccidentalType::Sharp, count)
    }
    
    pub fn flat(count: u8) -> Self {
        Self::new(AccidentalType::Flat, count)
    }
    
    pub fn natural() -> Option<Self> {
        None
    }
}

impl fmt::Display for Accidental {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for _ in 0..self.count {
            write!(f, "{}", self.accidental_type)?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Note {
    pub name: NoteName,
    pub accidental: Option<Accidental>,
}

impl Note {
    pub fn new(name: NoteName) -> Self {
        Self {
            name,
            accidental: None,
        }
    }
    
    pub fn with_accidental(mut self, accidental: Accidental) -> Self {
        self.accidental = Some(accidental);
        self
    }
    
    pub fn sharp(self, count: u8) -> Self {
        self.with_accidental(Accidental::sharp(count))
    }
    
    pub fn flat(self, count: u8) -> Self {
        self.with_accidental(Accidental::flat(count))
    }
    
    // Transpose a note by a number of semitones
    pub fn transpose(&self, semitones: i8) -> Self {
        let current_semitones = note_name_to_semitones(self.name);
        let accidental_adjustment = match &self.accidental {
            Some(acc) => match acc.accidental_type {
                AccidentalType::Sharp => acc.count as i8,
                AccidentalType::Flat => -(acc.count as i8),
            },
            None => 0,
        };
        
        let total_semitones = current_semitones + accidental_adjustment + semitones;
        let normalized_semitones = ((total_semitones % 12) + 12) % 12;
        
        // Convert back to note name and accidental
        let (new_name, new_accidental) = semitones_to_note_name(normalized_semitones);
        
        Self {
            name: new_name,
            accidental: new_accidental,
        }
    }
}

impl fmt::Display for Note {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name)?;
        if let Some(accidental) = &self.accidental {
            write!(f, "{}", accidental)?;
        }
        Ok(())
    }
}

// Scale degree system - simplified with modifiers
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ScaleDegreeNumber {
    One, Two, Three, Four, Five, Six, Seven, Nine, Eleven, Thirteen,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ScaleDegree {
    pub number: ScaleDegreeNumber,
    pub accidental: Option<Accidental>,
}

impl ScaleDegree {
    pub fn new(number: ScaleDegreeNumber) -> Self {
        Self {
            number,
            accidental: None,
        }
    }
    
    pub fn with_accidental(mut self, accidental: Accidental) -> Self {
        self.accidental = Some(accidental);
        self
    }
    
    pub fn sharp(self, count: u8) -> Self {
        self.with_accidental(Accidental::sharp(count))
    }
    
    pub fn flat(self, count: u8) -> Self {
        self.with_accidental(Accidental::flat(count))
    }
    
    // Convenience constructors
    pub fn one() -> Self { Self::new(ScaleDegreeNumber::One) }
    pub fn two() -> Self { Self::new(ScaleDegreeNumber::Two) }
    pub fn three() -> Self { Self::new(ScaleDegreeNumber::Three) }
    pub fn four() -> Self { Self::new(ScaleDegreeNumber::Four) }
    pub fn five() -> Self { Self::new(ScaleDegreeNumber::Five) }
    pub fn six() -> Self { Self::new(ScaleDegreeNumber::Six) }
    pub fn seven() -> Self { Self::new(ScaleDegreeNumber::Seven) }
    pub fn nine() -> Self { Self::new(ScaleDegreeNumber::Nine) }
    pub fn eleven() -> Self { Self::new(ScaleDegreeNumber::Eleven) }
    pub fn thirteen() -> Self { Self::new(ScaleDegreeNumber::Thirteen) }
    
    // Common chord degrees
    pub fn flat_three() -> Self { Self::three().flat(1) }
    pub fn sharp_four() -> Self { Self::four().sharp(1) }
    pub fn flat_five() -> Self { Self::five().flat(1) }
    pub fn sharp_five() -> Self { Self::five().sharp(1) }
    pub fn flat_seven() -> Self { Self::seven().flat(1) }
    pub fn flat_nine() -> Self { Self::nine().flat(1) }
    pub fn sharp_eleven() -> Self { Self::eleven().sharp(1) }
    pub fn flat_thirteen() -> Self { Self::thirteen().flat(1) }
}

impl fmt::Display for ScaleDegree {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Add accidental prefix if present
        if let Some(accidental) = &self.accidental {
            write!(f, "{}", accidental)?;
        }
        
        // Add the number
        match self.number {
            ScaleDegreeNumber::One => write!(f, "1"),
            ScaleDegreeNumber::Two => write!(f, "2"),
            ScaleDegreeNumber::Three => write!(f, "3"),
            ScaleDegreeNumber::Four => write!(f, "4"),
            ScaleDegreeNumber::Five => write!(f, "5"),
            ScaleDegreeNumber::Six => write!(f, "6"),
            ScaleDegreeNumber::Seven => write!(f, "7"),
            ScaleDegreeNumber::Nine => write!(f, "9"),
            ScaleDegreeNumber::Eleven => write!(f, "11"),
            ScaleDegreeNumber::Thirteen => write!(f, "13"),
        }
    }
}

// Chord Quality System - Hierarchical enums for extensibility
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ChordType {
    Major(MajorChord),
    Minor(MinorChord),
    Diminished(DiminishedChord),
    Augmented(AugmentedChord),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum MajorChord {
    Triad,
    Major7,
    Dominant7,
    // Future extensions: 9, 11, 13, b9, #9, #11, b13, etc.
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum MinorChord {
    Triad,
    Minor7,
    MinorMajor7,
    // Future extensions: 9, 11, 13, b9, #9, #11, b13, etc.
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum DiminishedChord {
    Triad,
    HalfDiminished, // m7♭5
    FullyDiminished, // dim7
    // Future extensions: 9, 11, 13, b9, #9, #11, b13, etc.
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum AugmentedChord {
    Triad,
    Augmented7,
    // Future extensions: 9, 11, 13, b9, #9, #11, b13, etc.
}

impl ChordType {
    // Major chord constructors
    pub fn major() -> Self { ChordType::Major(MajorChord::Triad) }
    pub fn major7() -> Self { ChordType::Major(MajorChord::Major7) }
    pub fn dominant7() -> Self { ChordType::Major(MajorChord::Dominant7) }
    
    // Minor chord constructors
    pub fn minor() -> Self { ChordType::Minor(MinorChord::Triad) }
    pub fn minor7() -> Self { ChordType::Minor(MinorChord::Minor7) }
    pub fn minor_major7() -> Self { ChordType::Minor(MinorChord::MinorMajor7) }
    
    // Diminished chord constructors
    pub fn diminished() -> Self { ChordType::Diminished(DiminishedChord::Triad) }
    pub fn half_diminished() -> Self { ChordType::Diminished(DiminishedChord::HalfDiminished) }
    pub fn fully_diminished() -> Self { ChordType::Diminished(DiminishedChord::FullyDiminished) }
    
    // Augmented chord constructors
    pub fn augmented() -> Self { ChordType::Augmented(AugmentedChord::Triad) }
    pub fn augmented7() -> Self { ChordType::Augmented(AugmentedChord::Augmented7) }
    
    // Get the base quality
    pub fn base_quality(&self) -> BaseQuality {
        match self {
            ChordType::Major(_) => BaseQuality::Major,
            ChordType::Minor(_) => BaseQuality::Minor,
            ChordType::Diminished(_) => BaseQuality::Diminished,
            ChordType::Augmented(_) => BaseQuality::Augmented,
        }
    }
    
    // Check if this is a seventh chord
    pub fn has_seventh(&self) -> bool {
        match self {
            ChordType::Major(MajorChord::Triad) => false,
            ChordType::Minor(MinorChord::Triad) => false,
            ChordType::Diminished(DiminishedChord::Triad) => false,
            ChordType::Augmented(AugmentedChord::Triad) => false,
            _ => true,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BaseQuality {
    Major,
    Minor,
    Diminished,
    Augmented,
}

impl fmt::Display for BaseQuality {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BaseQuality::Major => write!(f, "Major"),
            BaseQuality::Minor => write!(f, "Minor"),
            BaseQuality::Diminished => write!(f, "Diminished"),
            BaseQuality::Augmented => write!(f, "Augmented"),
        }
    }
}

impl fmt::Display for ChordType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ChordType::Major(major_type) => match major_type {
                MajorChord::Triad => write!(f, "maj"),
                MajorChord::Major7 => write!(f, "maj7"),
                MajorChord::Dominant7 => write!(f, "7"),
            },
            ChordType::Minor(minor_type) => match minor_type {
                MinorChord::Triad => write!(f, "m"),
                MinorChord::Minor7 => write!(f, "m7"),
                MinorChord::MinorMajor7 => write!(f, "m/maj7"),
            },
            ChordType::Diminished(dim_type) => match dim_type {
                DiminishedChord::Triad => write!(f, "dim"),
                DiminishedChord::HalfDiminished => write!(f, "m7♭5"),
                DiminishedChord::FullyDiminished => write!(f, "dim7"),
            },
            ChordType::Augmented(aug_type) => match aug_type {
                AugmentedChord::Triad => write!(f, "aug"),
                AugmentedChord::Augmented7 => write!(f, "aug7"),
            },
        }
    }
}

// Extensions that can be added to chords
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Extension {
    Ninth,      // 9
    Eleventh,   // 11
    Thirteenth, // 13
}

// Alterations that can be applied to chord tones
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Alteration {
    Flat5,    // b5
    Sharp5,   // #5
    Flat9,    // b9
    Sharp9,   // #9
    Sharp11,  // #11
    Flat13,   // b13
}

// Omitted intervals in chords (only when higher extensions are present)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum OmittedInterval {
    Third,     // omit3
    Fifth,     // omit5
    Seventh,   // omit7
    Ninth,     // omit9
    Eleventh,  // omit11
    Thirteenth, // omit13
}

impl fmt::Display for Extension {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Extension::Ninth => write!(f, "9"),
            Extension::Eleventh => write!(f, "11"),
            Extension::Thirteenth => write!(f, "13"),
        }
    }
}

impl fmt::Display for Alteration {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Alteration::Flat5 => write!(f, "^b5"),
            Alteration::Sharp5 => write!(f, "^#5"),
            Alteration::Flat9 => write!(f, "^b9"),
            Alteration::Sharp9 => write!(f, "^#9"),
            Alteration::Sharp11 => write!(f, "^#11"),
            Alteration::Flat13 => write!(f, "^b13"),
        }
    }
}

impl fmt::Display for OmittedInterval {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            OmittedInterval::Third => write!(f, "omit3"),
            OmittedInterval::Fifth => write!(f, "omit5"),
            OmittedInterval::Seventh => write!(f, "omit7"),
            OmittedInterval::Ninth => write!(f, "omit9"),
            OmittedInterval::Eleventh => write!(f, "omit11"),
            OmittedInterval::Thirteenth => write!(f, "omit13"),
        }
    }
}

// Chord structure with omitted intervals
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Chord {
    pub root: Note,
    pub chord_type: ChordType,
    pub notes: Vec<Note>,
    pub omitted_intervals: Vec<OmittedInterval>,
}

impl Chord {
    pub fn new(root: Note, chord_type: ChordType) -> Self {
        let notes = Self::build_notes(&root, &chord_type);
        Self {
            root,
            chord_type,
            notes,
            omitted_intervals: Vec::new(),
        }
    }
    
    pub fn with_omitted_intervals(mut self, omitted: Vec<OmittedInterval>) -> Self {
        self.omitted_intervals = omitted;
        self.notes = Self::build_notes_with_omissions(&self.root, &self.chord_type, &self.omitted_intervals);
        self
    }
    
    pub fn omit_third(self) -> Self {
        self.with_omitted_intervals(vec![OmittedInterval::Third])
    }
    
    pub fn omit_fifth(self) -> Self {
        self.with_omitted_intervals(vec![OmittedInterval::Fifth])
    }
    
    pub fn omit_seventh(self) -> Self {
        self.with_omitted_intervals(vec![OmittedInterval::Seventh])
    }
    
    pub fn omit_ninth(self) -> Self {
        self.with_omitted_intervals(vec![OmittedInterval::Ninth])
    }
    
    pub fn omit_eleventh(self) -> Self {
        self.with_omitted_intervals(vec![OmittedInterval::Eleventh])
    }
    
    pub fn omit_thirteenth(self) -> Self {
        self.with_omitted_intervals(vec![OmittedInterval::Thirteenth])
    }
    
    pub fn omit_multiple(self, omitted: Vec<OmittedInterval>) -> Self {
        self.with_omitted_intervals(omitted)
    }
    
    fn build_notes(root: &Note, chord_type: &ChordType) -> Vec<Note> {
        Self::build_notes_with_omissions(root, chord_type, &[])
    }
    
    fn build_notes_with_omissions(root: &Note, chord_type: &ChordType, omitted: &[OmittedInterval]) -> Vec<Note> {
        let mut notes = vec![root.clone()];
        
        // Add third and fifth based on base quality (unless omitted)
        let (third_semitones, fifth_semitones) = match chord_type.base_quality() {
            BaseQuality::Major => (4, 7), // Major third, perfect fifth
            BaseQuality::Minor => (3, 7), // Minor third, perfect fifth
            BaseQuality::Diminished => (3, 6), // Minor third, diminished fifth
            BaseQuality::Augmented => (4, 8), // Major third, augmented fifth
        };
        
        if !omitted.contains(&OmittedInterval::Third) {
            notes.push(root.transpose(third_semitones));
        }
        
        if !omitted.contains(&OmittedInterval::Fifth) {
            notes.push(root.transpose(fifth_semitones));
        }
        
        // Add seventh if present (unless omitted)
        if chord_type.has_seventh() && !omitted.contains(&OmittedInterval::Seventh) {
            let seventh_semitones = match chord_type {
                ChordType::Major(MajorChord::Major7) => 11, // Major seventh
                ChordType::Major(MajorChord::Dominant7) => 10, // Minor seventh
                ChordType::Minor(MinorChord::Minor7) => 10, // Minor seventh
                ChordType::Minor(MinorChord::MinorMajor7) => 11, // Major seventh
                ChordType::Diminished(DiminishedChord::HalfDiminished) => 10, // Minor seventh
                ChordType::Diminished(DiminishedChord::FullyDiminished) => 9, // Diminished seventh
                ChordType::Augmented(AugmentedChord::Augmented7) => 10, // Minor seventh
                _ => unreachable!(), // Triads should not reach here
            };
            notes.push(root.transpose(seventh_semitones));
        }
        
        // Add extensions (9th, 11th, 13th) if not omitted
        // Note: This is a simplified version - in practice, you'd need more complex logic
        // to determine which extensions are appropriate for each chord type
        
        if !omitted.contains(&OmittedInterval::Ninth) {
            // Add 9th (2 semitones up from root, but an octave higher)
            notes.push(root.transpose(14)); // 9th = 2 + 12 = 14 semitones
        }
        
        if !omitted.contains(&OmittedInterval::Eleventh) {
            // Add 11th (5 semitones up from root, but an octave higher)
            notes.push(root.transpose(17)); // 11th = 5 + 12 = 17 semitones
        }
        
        if !omitted.contains(&OmittedInterval::Thirteenth) {
            // Add 13th (9 semitones up from root, but an octave higher)
            notes.push(root.transpose(21)); // 13th = 9 + 12 = 21 semitones
        }
        
        notes
    }
}

impl fmt::Display for Chord {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}{}", self.root, self.chord_type)?;
        
        if !self.omitted_intervals.is_empty() {
            let omitted_str = self.omitted_intervals.iter()
                .map(|interval| format!("{}", interval))
                .collect::<Vec<_>>()
                .join(" ");
            write!(f, " ({})", omitted_str)?;
        }
        
        Ok(())
    }
}

// Trait for multiple naming levels - can be implemented on any type
pub trait NamingLevels {
    fn expanded(&self) -> String;
    fn detail(&self) -> String;
    fn common(&self) -> String;
    fn short(&self) -> String;
    fn minimized(&self) -> String;
}

// Chord quality with multiple naming levels
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct QualityNames {
    pub expanded: String,    // "maj"
    pub detail: String,      // "major"
    pub common: String,      // "M"
    pub short: String,       // "M"
    pub minimized: String,   // "M"
}

impl NamingLevels for QualityNames {
    fn expanded(&self) -> String { self.expanded.clone() }
    fn detail(&self) -> String { self.detail.clone() }
    fn common(&self) -> String { self.common.clone() }
    fn short(&self) -> String { self.short.clone() }
    fn minimized(&self) -> String { self.minimized.clone() }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ChordQuality {
    Major(MajorSeventh),
    Minor(MinorSeventh),
    Diminished,
    Augmented,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MajorSeventh {
    None,        // Just the triad
    Major7,      // Major 7th (11 semitones)
    Dominant7,   // Minor 7th (10 semitones)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MinorSeventh {
    None,        // Just the triad
    Minor7,      // Minor 7th (10 semitones)
    MinorMajor7, // Major 7th (11 semitones)
}

impl NamingLevels for ChordQuality {
    fn expanded(&self) -> String {
        match self {
            ChordQuality::Major(seventh) => match seventh {
                MajorSeventh::None => "maj".to_string(),
                MajorSeventh::Major7 => "maj7".to_string(),
                MajorSeventh::Dominant7 => "7".to_string(),
            },
            ChordQuality::Minor(seventh) => match seventh {
                MinorSeventh::None => "m".to_string(),
                MinorSeventh::Minor7 => "m7".to_string(),
                MinorSeventh::MinorMajor7 => "m/maj7".to_string(),
            },
            ChordQuality::Diminished => "dim".to_string(),
            ChordQuality::Augmented => "aug".to_string(),
        }
    }
    
    fn detail(&self) -> String {
        match self {
            ChordQuality::Major(seventh) => match seventh {
                MajorSeventh::None => "major".to_string(),
                MajorSeventh::Major7 => "major7".to_string(),
                MajorSeventh::Dominant7 => "dominant7".to_string(),
            },
            ChordQuality::Minor(seventh) => match seventh {
                MinorSeventh::None => "minor".to_string(),
                MinorSeventh::Minor7 => "minor7".to_string(),
                MinorSeventh::MinorMajor7 => "minor/major7".to_string(),
            },
            ChordQuality::Diminished => "diminished".to_string(),
            ChordQuality::Augmented => "augmented".to_string(),
        }
    }
    
    fn common(&self) -> String {
        match self {
            ChordQuality::Major(seventh) => match seventh {
                MajorSeventh::None => "M".to_string(),
                MajorSeventh::Major7 => "M7".to_string(),
                MajorSeventh::Dominant7 => "7".to_string(),
            },
            ChordQuality::Minor(seventh) => match seventh {
                MinorSeventh::None => "m".to_string(),
                MinorSeventh::Minor7 => "m7".to_string(),
                MinorSeventh::MinorMajor7 => "mM7".to_string(),
            },
            ChordQuality::Diminished => "dim".to_string(),
            ChordQuality::Augmented => "aug".to_string(),
        }
    }
    
    fn short(&self) -> String {
        match self {
            ChordQuality::Major(seventh) => match seventh {
                MajorSeventh::None => "M".to_string(),
                MajorSeventh::Major7 => "M7".to_string(),
                MajorSeventh::Dominant7 => "7".to_string(),
            },
            ChordQuality::Minor(seventh) => match seventh {
                MinorSeventh::None => "m".to_string(),
                MinorSeventh::Minor7 => "m7".to_string(),
                MinorSeventh::MinorMajor7 => "mM7".to_string(),
            },
            ChordQuality::Diminished => "dim".to_string(),
            ChordQuality::Augmented => "aug".to_string(),
        }
    }
    
    fn minimized(&self) -> String {
        match self {
            ChordQuality::Major(seventh) => match seventh {
                MajorSeventh::None => "M".to_string(),
                MajorSeventh::Major7 => "M7".to_string(),
                MajorSeventh::Dominant7 => "7".to_string(),
            },
            ChordQuality::Minor(seventh) => match seventh {
                MinorSeventh::None => "m".to_string(),
                MinorSeventh::Minor7 => "m7".to_string(),
                MinorSeventh::MinorMajor7 => "mM7".to_string(),
            },
            ChordQuality::Diminished => "dim".to_string(),
            ChordQuality::Augmented => "aug".to_string(),
        }
    }
}

impl ChordQuality {
    pub fn names(&self) -> QualityNames {
        QualityNames {
            expanded: self.expanded(),
            detail: self.detail(),
            common: self.common(),
            short: self.short(),
            minimized: self.minimized(),
        }
    }
}

// Dynamic chord naming system
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ChordName {
    pub quality: ChordQuality,
    pub extensions: Vec<Extension>,     // [Ninth, Eleventh, Thirteenth]
    pub alterations: Vec<Alteration>,   // [Flat5, Sharp9, Sharp11, etc.]
    pub omitted: Vec<OmittedInterval>,  // [Omit3, Omit5, Omit9, etc.]
    pub additions: Vec<String>,         // ["6", "sus4", "add9"] - for special cases
}

impl ChordName {
    pub fn new(quality: ChordQuality) -> Self {
        Self {
            quality,
            extensions: Vec::new(),
            alterations: Vec::new(),
            omitted: Vec::new(),
            additions: Vec::new(),
        }
    }
    
    // Convenience constructors for Major chords
    pub fn major() -> Self {
        Self::new(ChordQuality::Major(MajorSeventh::None))
    }
    
    pub fn major7() -> Self {
        Self::new(ChordQuality::Major(MajorSeventh::Major7))
    }
    
    pub fn dominant7() -> Self {
        Self::new(ChordQuality::Major(MajorSeventh::Dominant7))
    }
    
    // Convenience constructors for Minor chords
    pub fn minor() -> Self {
        Self::new(ChordQuality::Minor(MinorSeventh::None))
    }
    
    pub fn minor7() -> Self {
        Self::new(ChordQuality::Minor(MinorSeventh::Minor7))
    }
    
    pub fn minor_major7() -> Self {
        Self::new(ChordQuality::Minor(MinorSeventh::MinorMajor7))
    }
    
    // Convenience constructors for other qualities
    pub fn diminished() -> Self {
        Self::new(ChordQuality::Diminished)
    }
    
    pub fn augmented() -> Self {
        Self::new(ChordQuality::Augmented)
    }
    
    // Typesafe extension methods
    pub fn with_extension(mut self, ext: Extension) -> Self {
        self.extensions.push(ext);
        self
    }
    
    pub fn with_extensions(mut self, exts: Vec<Extension>) -> Self {
        self.extensions.extend(exts);
        self
    }
    
    // Typesafe alteration methods
    pub fn with_alteration(mut self, alt: Alteration) -> Self {
        self.alterations.push(alt);
        self
    }
    
    pub fn with_alterations(mut self, alts: Vec<Alteration>) -> Self {
        self.alterations.extend(alts);
        self
    }
    
    // Typesafe omission methods
    pub fn with_omission(mut self, omit: OmittedInterval) -> Self {
        self.omitted.push(omit);
        self
    }
    
    pub fn with_omissions(mut self, omits: Vec<OmittedInterval>) -> Self {
        self.omitted.extend(omits);
        self
    }
    
    // Special additions (for cases like sus4, add9, etc.)
    pub fn with_addition(mut self, add: &str) -> Self {
        self.additions.push(add.to_string());
        self
    }
    
    // Convenience methods for common extensions
    pub fn with_ninth(self) -> Self {
        self.with_extension(Extension::Ninth)
    }
    
    pub fn with_eleventh(self) -> Self {
        self.with_extension(Extension::Eleventh)
    }
    
    pub fn with_thirteenth(self) -> Self {
        self.with_extension(Extension::Thirteenth)
    }
    
    // Convenience methods for common alterations
    pub fn with_flat_five(self) -> Self {
        self.with_alteration(Alteration::Flat5)
    }
    
    pub fn with_sharp_five(self) -> Self {
        self.with_alteration(Alteration::Sharp5)
    }
    
    pub fn with_flat_nine(self) -> Self {
        self.with_alteration(Alteration::Flat9)
    }
    
    pub fn with_sharp_nine(self) -> Self {
        self.with_alteration(Alteration::Sharp9)
    }
    
    pub fn with_sharp_eleven(self) -> Self {
        self.with_alteration(Alteration::Sharp11)
    }
    
    pub fn with_flat_thirteenth(self) -> Self {
        self.with_alteration(Alteration::Flat13)
    }
    
    // Convenience methods for common omissions
    pub fn omit_third(self) -> Self {
        self.with_omission(OmittedInterval::Third)
    }
    
    pub fn omit_fifth(self) -> Self {
        self.with_omission(OmittedInterval::Fifth)
    }
    
    pub fn omit_seventh(self) -> Self {
        self.with_omission(OmittedInterval::Seventh)
    }
    
    pub fn omit_ninth(self) -> Self {
        self.with_omission(OmittedInterval::Ninth)
    }
    
    pub fn omit_eleventh(self) -> Self {
        self.with_omission(OmittedInterval::Eleventh)
    }
    
    pub fn omit_thirteenth(self) -> Self {
        self.with_omission(OmittedInterval::Thirteenth)
    }
    
    // Helper method to get the highest extension
    fn get_highest_extension(&self) -> Extension {
        if self.extensions.contains(&Extension::Thirteenth) {
            Extension::Thirteenth
        } else if self.extensions.contains(&Extension::Eleventh) {
            Extension::Eleventh
        } else if self.extensions.contains(&Extension::Ninth) {
            Extension::Ninth
        } else {
            // This shouldn't happen, but return Ninth as default
            Extension::Ninth
        }
    }
    
    // Helper method to check if extensions form a complete sequence
    pub fn has_complete_extension_sequence(&self) -> bool {
        let has_seventh = self.quality_has_seventh();
        let has_ninth = self.extensions.contains(&Extension::Ninth);
        let has_eleventh = self.extensions.contains(&Extension::Eleventh);
        let has_thirteenth = self.extensions.contains(&Extension::Thirteenth);
        
        if !has_seventh {
            // Without seventh, any extension is an "add"
            return false;
        }
        
        // With seventh, check if we have a complete sequence up to the highest extension
        if has_thirteenth {
            // For 13th, we need 9th and 11th to be complete
            has_ninth && has_eleventh
        } else if has_eleventh {
            // For 11th, we need 9th to be complete
            has_ninth
        } else if has_ninth {
            // For 9th, we just need the 7th (which we have)
            true
        } else {
            // No extensions
            true
        }
    }
    
    // Helper method to check if quality has seventh
    pub fn quality_has_seventh(&self) -> bool {
        match &self.quality {
            ChordQuality::Major(seventh) => !matches!(seventh, MajorSeventh::None),
            ChordQuality::Minor(seventh) => !matches!(seventh, MinorSeventh::None),
            ChordQuality::Diminished => false, // Basic diminished is triad
            ChordQuality::Augmented => false,  // Basic augmented is triad
        }
    }
    
    // Check if this is a suspended chord
    pub fn is_suspended(&self) -> bool {
        self.has_sus2() || self.has_sus4()
    }
    
    // Check if this has sus2 (third replaced with second)
    pub fn has_sus2(&self) -> bool {
        // This would need to be determined from the actual chord structure
        // For now, we'll add a field to track this
        self.additions.contains(&"sus2".to_string())
    }
    
    // Check if this has sus4 (third replaced with fourth)
    pub fn has_sus4(&self) -> bool {
        // This would need to be determined from the actual chord structure
        // For now, we'll add a field to track this
        self.additions.contains(&"sus4".to_string())
    }
    
    // Convenience methods for suspended chords
    pub fn with_sus2(self) -> Self {
        self.with_addition("sus2")
    }
    
    pub fn with_sus4(self) -> Self {
        self.with_addition("sus4")
    }
    
    // Generate colored expanded name with syntax highlighting
    pub fn expanded_name_colored(&self) -> String {
        let mut result = String::new();
        let name = self.expanded_name();
        let parts: Vec<&str> = name.split_whitespace().collect();
        
        for (i, part) in parts.iter().enumerate() {
            if i > 0 {
                result.push(' ');
            }
            
            // Color each part based on what it represents
            let colored_part = if part.starts_with("maj") {
                // Quality part - yellow bold
                part.bright_yellow().bold().to_string()
            } else if part.starts_with("sus") {
                // Suspended chords - blue
                part.bright_blue().to_string()
            } else if part.starts_with("omit") {
                // Omissions - red
                part.red().to_string()
            } else if part.starts_with("add") {
                // Additions - blue
                part.bright_blue().to_string()
            } else if *part == "6" {
                // 6th addition - blue
                part.bright_blue().to_string()
            } else if *part == "9" || *part == "11" || *part == "13" {
                // Extensions - magenta
                part.bright_magenta().to_string()
            } else if part.starts_with("^") {
                // Alterations - red
                part.bright_red().to_string()
            } else {
                // Default - cyan
                part.bright_cyan().to_string()
            };
            
            result.push_str(&colored_part);
        }
        
        result
    }

    // Generate expanded name (e.g., "maj7 omit3")
    pub fn expanded_name(&self) -> String {
        let mut name = self.quality.expanded();
        
        // Don't handle suspended chords here - we'll add them at the end
        
        // Handle 6th chords - if we have 6th addition and no 7th, use "6" or "6/9" instead of "maj"
        if self.additions.contains(&"6".to_string()) && !self.quality_has_seventh() {
            // Check if we also have 9th extension for 6/9 chord
            if self.extensions.contains(&Extension::Ninth) {
                name = name.replace("maj", "6/9");
            } else {
                name = name.replace("maj", "6");
            }
        }
        
        // Handle extensions - use "add" for incomplete sequences
        if !self.extensions.is_empty() {
            // Skip 9th if we already handled it in 6/9 chord naming
            let is_6_9_chord = self.additions.contains(&"6".to_string()) && 
                              !self.quality_has_seventh() && 
                              self.extensions.contains(&Extension::Ninth);
            
            if !is_6_9_chord {
                let highest_extension = self.get_highest_extension();
                let has_complete_sequence = self.has_complete_extension_sequence();
                
                if has_complete_sequence {
                    // Complete sequence - show just the highest extension (e.g., "Maj13" not "Maj713")
                    // Remove the "7" from the quality name since it's implied in the extension
                    if name.ends_with("7") {
                        name = name.trim_end_matches("7").to_string();
                    }
                    name.push_str(&format!("{}", highest_extension));
                } else {
                    // Incomplete sequence - use "add" for the highest extension
                    name.push_str(&format!(" add{}", highest_extension));
                }
            }
        }
        
        // Add alterations
        for alt in &self.alterations {
            name.push_str(&format!("{}", alt));
        }
        
        // Add additions (but skip "6" if we already converted to "maj6", and skip sus2/sus4 if we converted the base name)
        for add in &self.additions {
            if add != "6" || self.quality_has_seventh() {
                if !self.is_suspended() || (add != "sus2" && add != "sus4") {
                    // Use "add" prefix for additions to chords that already have extensions
                    if !self.extensions.is_empty() || self.quality_has_seventh() {
                        name.push_str(&format!(" add{}", add));
                    } else {
                        name.push_str(&format!(" {}", add));
                    }
                }
            }
        }
        
        // Add omissions
        for omit in &self.omitted {
            name.push_str(&format!(" {}", omit));
        }
        
        // Handle suspended chords - replace the entire name logic
        if self.is_suspended() {
            let sus_type = if self.has_sus4() { "sus4" } else { "sus2" };
            
            // Check if we have extensions that would make this a complex suspended chord
            if self.extensions.contains(&Extension::Thirteenth) {
                // 13sus chord
                name = format!("13{}", sus_type);
            } else if self.extensions.contains(&Extension::Ninth) {
                // 9sus chord
                name = format!("9{}", sus_type);
            } else if self.quality_has_seventh() {
                // Check if it's major 7th or dominant 7th
                match &self.quality {
                    ChordQuality::Major(MajorSeventh::Major7) => {
                        name = format!("maj7{}", sus_type);
                    }
                    ChordQuality::Major(MajorSeventh::Dominant7) => {
                        name = format!("7{}", sus_type);
                    }
                    _ => {
                        name = format!("7{}", sus_type);
                    }
                }
            } else {
                // Basic suspended chord - no "maj" prefix
                name = sus_type.to_string();
            }
        }
        
        name
    }
    
    // Generate detail name (e.g., "major7 omit3")
    pub fn detail_name(&self) -> String {
        let mut name = self.quality.detail();
        
        // Handle extensions - use "add" for incomplete sequences
        if !self.extensions.is_empty() {
            let highest_extension = self.get_highest_extension();
            let has_complete_sequence = self.has_complete_extension_sequence();
            
            if has_complete_sequence {
                // Complete sequence - show just the highest extension (e.g., "major13" not "major713")
                // Remove the "7" from the quality name since it's implied in the extension
                if name.ends_with("7") {
                    name = name.trim_end_matches("7").to_string();
                }
                name.push_str(&format!("{}", highest_extension));
            } else {
                // Incomplete sequence - use "add" for the highest extension
                name.push_str(&format!(" add{}", highest_extension));
            }
        }
        
        // Add alterations
        for alt in &self.alterations {
            name.push_str(&format!("{}", alt));
        }
        
        // Add additions
        for add in &self.additions {
            name.push_str(&format!(" {}", add));
        }
        
        // Add omissions
        for omit in &self.omitted {
            name.push_str(&format!(" {}", omit));
        }
        
        name
    }
    
    // Generate common name (e.g., "M7(no3)")
    pub fn common_name(&self) -> String {
        let mut name = self.quality.common();
        
        // Handle extensions - use "add" for incomplete sequences
        if !self.extensions.is_empty() {
            let highest_extension = self.get_highest_extension();
            let has_complete_sequence = self.has_complete_extension_sequence();
            
            if has_complete_sequence {
                // Complete sequence - show just the highest extension (e.g., "M13" not "M713")
                // Remove the "7" from the quality name since it's implied in the extension
                if name.ends_with("7") {
                    name = name.trim_end_matches("7").to_string();
                }
                name.push_str(&format!("{}", highest_extension));
            } else {
                // Incomplete sequence - use "add" for the highest extension
                name.push_str(&format!("add{}", highest_extension));
            }
        }
        
        // Add alterations
        for alt in &self.alterations {
            name.push_str(&format!("{}", alt));
        }
        
        // Add additions
        for add in &self.additions {
            name.push_str(&format!("{}", add));
        }
        
        // Add omissions in parentheses
        if !self.omitted.is_empty() {
            let omissions: Vec<String> = self.omitted.iter().map(|o| format!("{}", o)).collect();
            name.push_str(&format!("({})", omissions.join(" ")));
        }
        
        name
    }
    
    // Generate short name (e.g., "M7")
    pub fn short_name(&self) -> String {
        let mut name = self.quality.short();
        
        // Handle extensions - use "add" for incomplete sequences
        if !self.extensions.is_empty() {
            let highest_extension = self.get_highest_extension();
            let has_complete_sequence = self.has_complete_extension_sequence();
            
            if has_complete_sequence {
                // Complete sequence - show just the highest extension (e.g., "M13" not "M713")
                // Remove the "7" from the quality name since it's implied in the extension
                if name.ends_with("7") {
                    name = name.trim_end_matches("7").to_string();
                }
                name.push_str(&format!("{}", highest_extension));
            } else {
                // Incomplete sequence - use "add" for the highest extension
                name.push_str(&format!("add{}", highest_extension));
            }
        }
        
        // Add alterations
        for alt in &self.alterations {
            name.push_str(&format!("{}", alt));
        }
        
        // Add additions
        for add in &self.additions {
            name.push_str(&format!("{}", add));
        }
        
        name
    }
    
    // Generate minimized name (e.g., "M")
    pub fn minimized_name(&self) -> String {
        self.quality.minimized()
    }
}

// Implement NamingLevels trait directly on ChordName
impl NamingLevels for ChordName {
    fn expanded(&self) -> String { self.expanded_name() }
    fn detail(&self) -> String { self.detail_name() }
    fn common(&self) -> String { self.common_name() }
    fn short(&self) -> String { self.short_name() }
    fn minimized(&self) -> String { self.minimized_name() }
}

// Chord recognition and naming system
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ChordPattern {
    pub semitone_pattern: Vec<u32>,
    pub chord_name: ChordName,
}

// Comprehensive chord analysis from semitone patterns
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ChordAnalysis {
    pub root_relative_intervals: Vec<Interval>,
    pub stacked_intervals: Vec<Interval>,
    pub semitone_pattern: Vec<u32>,
    pub recognized_chord: Option<ChordPattern>,
}

impl ChordAnalysis {
    pub fn from_semitone_pattern(pattern: Vec<u32>) -> Self {
        let root_relative_intervals = pattern.iter()
            .map(|&semitones| {
                intervals::semitones_to_compound_interval(semitones as u32)
                    .unwrap_or_else(|| {
                        // Fallback for invalid intervals
                        intervals::Interval::new(
                            intervals::IntervalQuality::Perfect,
                            intervals::IntervalNumber::Unison,
                            semitones as u8,
                            0
                        )
                    })
            })
            .collect();
        
        let stacked_intervals = Self::calculate_stacked_intervals(&pattern);
        
        // Try to recognize the chord
        let recognized_chord = Self::recognize_chord(&pattern);
        
        Self {
            root_relative_intervals,
            stacked_intervals,
            semitone_pattern: pattern,
            recognized_chord,
        }
    }
    
    // Chord recognition logic
    fn recognize_chord(pattern: &[u32]) -> Option<ChordPattern> {
        if pattern.is_empty() {
            return None;
        }
        
        // Convert to 0-based semitones for analysis
        let semitones: Vec<u32> = pattern.iter().map(|&p| p - 1).collect();
        
        // Analyze the chord structure
        let chord_name = Self::analyze_chord_structure(&semitones)?;
        
        Some(ChordPattern {
            semitone_pattern: pattern.to_vec(),
            chord_name,
        })
    }
    
    pub fn analyze_chord_structure(semitones: &[u32]) -> Option<ChordName> {
        if semitones.is_empty() {
            return None;
        }
        
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
            ChordQuality::Major(MajorSeventh::None)  // Major third present
        } else if semitones.contains(&3) {
            ChordQuality::Minor(MinorSeventh::None)  // Minor third present
        } else if semitones.contains(&6) {
            ChordQuality::Diminished  // Diminished (minor third + diminished fifth)
        } else if semitones.contains(&8) {
            ChordQuality::Augmented  // Augmented (major third + augmented fifth)
        } else {
            return None; // Unrecognized chord
        };
        
        let mut chord_name = ChordName::new(quality);
        
        // Add extensions (seventh is handled by the quality system)
        if has_ninth {
            chord_name = chord_name.with_ninth();
        }
        
        if has_eleventh {
            chord_name = chord_name.with_eleventh();
        }
        
        if has_thirteenth {
            chord_name = chord_name.with_thirteenth();
        }
        
        if has_sixth {
            chord_name = chord_name.with_addition("6");
        }
        
        // Check for alterations
        if semitones.contains(&6) && !semitones.contains(&7) {
            chord_name = chord_name.with_flat_five();
        }
        if semitones.contains(&8) && !semitones.contains(&7) {
            chord_name = chord_name.with_sharp_five();
        }
        if semitones.contains(&13) && !semitones.contains(&14) {
            chord_name = chord_name.with_flat_nine();
        }
        if semitones.contains(&15) && !semitones.contains(&14) {
            chord_name = chord_name.with_sharp_nine();
        }
        if semitones.contains(&18) && !semitones.contains(&17) {
            chord_name = chord_name.with_sharp_eleven();
        }
        if semitones.contains(&20) && !semitones.contains(&21) {
            chord_name = chord_name.with_flat_thirteenth();
        }
        
        // Check for omissions (only when intervals are completely missing, not altered)
        // Third: check for both major (4) and minor (3) third
        if !semitones.contains(&4) && !semitones.contains(&3) && (has_fifth || has_seventh || has_ninth || has_eleventh || has_thirteenth) {
            chord_name = chord_name.omit_third();
        }
        
        // Fifth: check for perfect fifth (7) - alterations like b5 (6) or #5 (8) still count as having a fifth
        if !semitones.contains(&7) && (has_third || has_seventh || has_ninth || has_eleventh || has_thirteenth) {
            chord_name = chord_name.omit_fifth();
        }
        
        // Ninth: check for both natural (14) and altered (13, 15) ninth
        if !semitones.contains(&14) && !semitones.contains(&13) && !semitones.contains(&15) && (has_eleventh || has_thirteenth) {
            chord_name = chord_name.omit_ninth();
        }
        
        Some(chord_name)
    }
    
    fn calculate_stacked_intervals(pattern: &[u32]) -> Vec<Interval> {
        if pattern.len() < 2 {
            return Vec::new();
        }
        
        let mut stacked = Vec::new();
        for i in 1..pattern.len() {
            let interval_semitones = pattern[i] - pattern[i-1];
            if let Some(interval) = intervals::semitones_to_compound_interval(interval_semitones as u32) {
                stacked.push(interval);
            }
        }
        stacked
    }
    
    pub fn display_analysis(&self) -> String {
        let root_intervals: Vec<String> = self.root_relative_intervals.iter()
            .map(|i| i.to_compact())
            .collect();
        
        let stacked_intervals: Vec<String> = self.stacked_intervals.iter()
            .map(|i| i.to_compact())
            .collect();
        
        format!(
            "Pattern: {} | Root-relative: {} | Stacked: {}",
            self.semitone_pattern.iter().map(|n| n.to_string()).collect::<Vec<_>>().join(" "),
            root_intervals.join(" + "),
            stacked_intervals.join(" + ")
        )
    }
}

// Helper function to get interval pattern from notes
pub fn get_interval_pattern(notes: &[Note]) -> String {
    if notes.is_empty() {
        return String::new();
    }
    
    // Convert all notes to semitones from C
    let mut semitones: Vec<i8> = notes.iter()
        .map(|note| {
            let base_semitones = note_name_to_semitones(note.name);
            let accidental_adjustment = match &note.accidental {
                Some(acc) => match acc.accidental_type {
                    AccidentalType::Sharp => acc.count as i8,
                    AccidentalType::Flat => -(acc.count as i8),
                },
                None => 0,
            };
            base_semitones + accidental_adjustment
        })
        .collect();
    
    // Sort and normalize to 0-based intervals
    semitones.sort();
    let root = semitones[0];
    let intervals: Vec<String> = semitones.iter()
        .map(|&s| ((s - root) % 12 + 12) % 12 + 1) // Convert to 1-based intervals
        .map(|i| i.to_string())
        .collect();
    
    intervals.join(" ")
}

// Helper function to get semitone distance between note names
pub fn note_name_to_semitones(note: NoteName) -> i8 {
    match note {
        NoteName::C => 0,
        NoteName::D => 2,
        NoteName::E => 4,
        NoteName::F => 5,
        NoteName::G => 7,
        NoteName::A => 9,
        NoteName::B => 11,
    }
}

// Helper function to convert semitones back to note name and accidental
fn semitones_to_note_name(semitones: i8) -> (NoteName, Option<Accidental>) {
    match semitones {
        0 => (NoteName::C, None),
        1 => (NoteName::C, Some(Accidental::sharp(1))),
        2 => (NoteName::D, None),
        3 => (NoteName::D, Some(Accidental::sharp(1))),
        4 => (NoteName::E, None),
        5 => (NoteName::F, None),
        6 => (NoteName::F, Some(Accidental::sharp(1))),
        7 => (NoteName::G, None),
        8 => (NoteName::G, Some(Accidental::sharp(1))),
        9 => (NoteName::A, None),
        10 => (NoteName::A, Some(Accidental::sharp(1))),
        11 => (NoteName::B, None),
        _ => unreachable!(), // Should be normalized to 0-11
    }
}

// Calculate the scale degree distance between two notes
impl Note {
    pub fn scale_degree_from(&self, root: &Note) -> Option<ScaleDegree> {
        // Calculate semitone distance
        let root_semitones = note_name_to_semitones(root.name);
        let note_semitones = note_name_to_semitones(self.name);
        
        // Add accidental adjustments
        let root_adjustment = match &root.accidental {
            Some(acc) => match acc.accidental_type {
                AccidentalType::Sharp => acc.count as i8,
                AccidentalType::Flat => -(acc.count as i8),
            },
            None => 0,
        };
        
        let note_adjustment = match &self.accidental {
            Some(acc) => match acc.accidental_type {
                AccidentalType::Sharp => acc.count as i8,
                AccidentalType::Flat => -(acc.count as i8),
            },
            None => 0,
        };
        
        let total_semitones = (note_semitones + note_adjustment) - (root_semitones + root_adjustment);
        
        // Normalize to positive range (0-11)
        let normalized_semitones = ((total_semitones % 12) + 12) % 12;
        
        // Convert semitones to scale degree
        match normalized_semitones {
            0 => Some(ScaleDegree::one()),
            1 => Some(ScaleDegree::one().sharp(1)), // #1
            2 => Some(ScaleDegree::two()),
            3 => Some(ScaleDegree::two().sharp(1)), // #2
            4 => Some(ScaleDegree::three()),
            5 => Some(ScaleDegree::four()),
            6 => Some(ScaleDegree::four().sharp(1)), // #4
            7 => Some(ScaleDegree::five()),
            8 => Some(ScaleDegree::five().sharp(1)), // #5
            9 => Some(ScaleDegree::six()),
            10 => Some(ScaleDegree::six().sharp(1)), // #6
            11 => Some(ScaleDegree::seven()),
            _ => None,
        }
    }
}

// Test cases based on Lil Chordbox.lua chord dictionary
fn run_chord_tests() {
    println!("=== CHORD RECOGNITION TEST CASES ===");
    println!("Based on Lil Chordbox.lua chord dictionary");
    
    // Test all basic chord types we currently support
    let _test_roots = vec![
        Note::new(NoteName::C),
        Note::new(NoteName::D),
        Note::new(NoteName::E),
        Note::new(NoteName::F),
        Note::new(NoteName::G),
        Note::new(NoteName::A),
        Note::new(NoteName::B),
    ];
    
    let chord_types = vec![
        ("Major Triad", ChordType::major()),
        ("Major 7th", ChordType::major7()),
        ("Dominant 7th", ChordType::dominant7()),
        ("Minor Triad", ChordType::minor()),
        ("Minor 7th", ChordType::minor7()),
        ("Minor/Major 7th", ChordType::minor_major7()),
        ("Diminished Triad", ChordType::diminished()),
        ("Half Diminished", ChordType::half_diminished()),
        ("Fully Diminished", ChordType::fully_diminished()),
        ("Augmented Triad", ChordType::augmented()),
        ("Augmented 7th", ChordType::augmented7()),
    ];
    
    println!("\n--- Testing All Chord Types ---");
    for (name, chord_type) in &chord_types {
        println!("\n{} Chords:", name);
        // Show the interval pattern for this chord type
        let c_chord = Chord::new(Note::new(NoteName::C), *chord_type);
        let intervals = get_interval_pattern(&c_chord.notes);
        println!("  Pattern: {} = [{}]", intervals, c_chord.notes.iter().map(|n| n.to_string()).collect::<Vec<_>>().join(", "));
    }
    
    // Test specific chord patterns from Lil Chordbox.lua
    println!("\n--- Testing Lil Chordbox.lua Patterns ---");
    
    // Major chords (1 5 8 pattern)
    println!("\nMajor Triads:");
    let c_maj = Chord::new(Note::new(NoteName::C), ChordType::major());
    let maj_intervals = get_interval_pattern(&c_maj.notes);
    println!("  Pattern: {} = [{}]", maj_intervals, c_maj.notes.iter().map(|n| n.to_string()).collect::<Vec<_>>().join(", "));
    
    // Major 7th chords (1 5 8 12 pattern)
    println!("\nMajor 7th Chords:");
    let c_maj7 = Chord::new(Note::new(NoteName::C), ChordType::major7());
    let maj7_intervals = get_interval_pattern(&c_maj7.notes);
    println!("  Pattern: {} = [{}]", maj7_intervals, c_maj7.notes.iter().map(|n| n.to_string()).collect::<Vec<_>>().join(", "));
    
    // Dominant 7th chords (1 5 8 11 pattern)
    println!("\nDominant 7th Chords:");
    let c_dom7 = Chord::new(Note::new(NoteName::C), ChordType::dominant7());
    let dom7_intervals = get_interval_pattern(&c_dom7.notes);
    println!("  Pattern: {} = [{}]", dom7_intervals, c_dom7.notes.iter().map(|n| n.to_string()).collect::<Vec<_>>().join(", "));
    
    // Minor chords (1 4 8 pattern)
    println!("\nMinor Triads:");
    let c_min = Chord::new(Note::new(NoteName::C), ChordType::minor());
    let min_intervals = get_interval_pattern(&c_min.notes);
    println!("  Pattern: {} = [{}]", min_intervals, c_min.notes.iter().map(|n| n.to_string()).collect::<Vec<_>>().join(", "));
    
    // Minor 7th chords (1 4 8 11 pattern)
    println!("\nMinor 7th Chords:");
    let c_min7 = Chord::new(Note::new(NoteName::C), ChordType::minor7());
    let min7_intervals = get_interval_pattern(&c_min7.notes);
    println!("  Pattern: {} = [{}]", min7_intervals, c_min7.notes.iter().map(|n| n.to_string()).collect::<Vec<_>>().join(", "));
    
    // Minor/Major 7th chords (1 4 8 12 pattern)
    println!("\nMinor/Major 7th Chords:");
    let c_min_maj7 = Chord::new(Note::new(NoteName::C), ChordType::minor_major7());
    let min_maj7_intervals = get_interval_pattern(&c_min_maj7.notes);
    println!("  Pattern: {} = [{}]", min_maj7_intervals, c_min_maj7.notes.iter().map(|n| n.to_string()).collect::<Vec<_>>().join(", "));
    
    // Diminished triads (1 4 7 pattern)
    println!("\nDiminished Triads:");
    let c_dim = Chord::new(Note::new(NoteName::C), ChordType::diminished());
    let dim_intervals = get_interval_pattern(&c_dim.notes);
    println!("  Pattern: {} = [{}]", dim_intervals, c_dim.notes.iter().map(|n| n.to_string()).collect::<Vec<_>>().join(", "));
    
    // Half diminished chords (1 4 7 11 pattern) - m7♭5
    println!("\nHalf Diminished Chords:");
    let c_half_dim = Chord::new(Note::new(NoteName::C), ChordType::half_diminished());
    let half_dim_intervals = get_interval_pattern(&c_half_dim.notes);
    println!("  Pattern: {} = [{}]", half_dim_intervals, c_half_dim.notes.iter().map(|n| n.to_string()).collect::<Vec<_>>().join(", "));
    
    // Fully diminished chords (1 4 7 10 pattern) - dim7
    println!("\nFully Diminished Chords:");
    let c_full_dim = Chord::new(Note::new(NoteName::C), ChordType::fully_diminished());
    let full_dim_intervals = get_interval_pattern(&c_full_dim.notes);
    println!("  Pattern: {} = [{}]", full_dim_intervals, c_full_dim.notes.iter().map(|n| n.to_string()).collect::<Vec<_>>().join(", "));
    
    // Augmented triads (1 5 9 pattern)
    println!("\nAugmented Triads:");
    let c_aug = Chord::new(Note::new(NoteName::C), ChordType::augmented());
    let aug_intervals = get_interval_pattern(&c_aug.notes);
    println!("  Pattern: {} = [{}]", aug_intervals, c_aug.notes.iter().map(|n| n.to_string()).collect::<Vec<_>>().join(", "));
    
    // Augmented 7th chords (1 5 9 11 pattern)
    println!("\nAugmented 7th Chords:");
    let c_aug7 = Chord::new(Note::new(NoteName::C), ChordType::augmented7());
    let aug7_intervals = get_interval_pattern(&c_aug7.notes);
    println!("  Pattern: {} = [{}]", aug7_intervals, c_aug7.notes.iter().map(|n| n.to_string()).collect::<Vec<_>>().join(", "));
    
    // Test with different roots
    println!("\n--- Testing Different Roots ---");
    let roots = vec![
        ("C", Note::new(NoteName::C)),
        ("D", Note::new(NoteName::D)),
        ("E", Note::new(NoteName::E)),
        ("F", Note::new(NoteName::F)),
        ("G", Note::new(NoteName::G)),
        ("A", Note::new(NoteName::A)),
        ("B", Note::new(NoteName::B)),
        ("F♯", Note::new(NoteName::F).sharp(1)),
        ("B♭", Note::new(NoteName::B).flat(1)),
    ];
    
    for (name, root) in &roots {
        let major = Chord::new(root.clone(), ChordType::major());
        let minor = Chord::new(root.clone(), ChordType::minor());
        let dom7 = Chord::new(root.clone(), ChordType::dominant7());
        
        println!("  {}: {} | {} | {}", name, major, minor, dom7);
    }
    
    // Test chord properties
    println!("\n--- Chord Properties ---");
    let test_chords = vec![
        ("C major", Chord::new(Note::new(NoteName::C), ChordType::major())),
        ("C major7", Chord::new(Note::new(NoteName::C), ChordType::major7())),
        ("C minor7", Chord::new(Note::new(NoteName::C), ChordType::minor7())),
        ("C diminished", Chord::new(Note::new(NoteName::C), ChordType::diminished())),
        ("C augmented", Chord::new(Note::new(NoteName::C), ChordType::augmented())),
    ];
    
    for (name, chord) in &test_chords {
        println!("  {}: has_seventh={}, base_quality={}", 
                 name, 
                 chord.chord_type.has_seventh(),
                 chord.chord_type.base_quality());
    }
}

// Library exports - no main function needed
