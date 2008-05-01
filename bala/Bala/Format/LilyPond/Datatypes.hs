
--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Format.LilyPond.Datatypes
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Datatypes for a subset of LilyPond format
--
--------------------------------------------------------------------------------

module Bala.Format.LilyPond.Datatypes  where


type LilyPondFile = [Command]

-- | Water has contents to preserve, but not to parse. 
type Water = String 


newtype Glyphs = Glyphs { unGlyphs :: [Glyph] }
  deriving (Eq,Show) 

data Glyph = GlyphEvent GlyphEvent
           | GlyphCommmand Command
           | GlyphMark Mark
  deriving (Eq,Show)          

data GlyphEvent = GEvtNote Note
                | GEvtChord Chord
                | GEvtRest Rest
  deriving (Eq,Show) 

data Note = Note Pitch (Maybe Duration) 
  deriving (Eq,Show)
  
  
data Rest = Rest Duration
  deriving (Eq,Show)
  
  
data Duration = Duration Int | Dotted Int
  deriving (Eq,Show)


data Pitch = Pitch Char (Maybe Accidental) (Maybe OctaveSpec)
  deriving (Eq, Show)
  
data Accidental = Sharp | Flat | DoubleSharp | DoubleFlat 
  deriving (Eq, Show)
  
data OctaveSpec = Raised Int | Lowered Int
  deriving (Eq, Show)


data Articulation = Articulation VerticalPlacement Char
                  | Fingering VerticalPlacement Int
  deriving (Eq, Show)       

-- ~ Placement of an articulation, slur ...
data VerticalPlacement = VAbove | VBelow | VDefault
  deriving (Eq,Show)
  
    
data MicroTone =  HalfFlat | HalfSharp 
  deriving (Eq, Show)


data Chord = Chord [Pitch] (Maybe Duration)
  deriving (Eq,Show)
  
   

data Command = CmdNew NewDecl  -- type music expression 
             | CmdKey Pitch Command
             | CmdVersion [Int]
             | CmdTimeSignature Int Int
             | CmdTempo Duration Int
             | CmdChordmode [Chordname]
             | CmdAddlyrics String
             | CmdDrums [Drumnote]
             | NullaryCommand String
             | ExprCommand String Water
             | UnaryCommand String String
             | BinaryCommand String String String
  deriving (Eq, Show)

data NewDecl = NewTabStaff [GuitarNote]
  deriving (Eq, Show)

data Mark = MarkTie
          | MarkSlur Slur
          | MarkBeam Beam
          | MarkBarCheck
  deriving (Eq,Show)

data Slur = SlurStart (Maybe VerticalPlacement)
          | SlurEnd
  deriving (Eq,Show)

data Beam = BeamStart
          | BeamEnd
  deriving (Eq,Show)    


--------------------------------------------------------------------------------
-- Chord
--------------------------------------------------------------------------------
    
data Chordname = Chordname Note (Maybe ChordSuffix)
  deriving (Eq,Show)

data ChordSuffix = ChordSuffix (Maybe AltInt) ChordModifier [ChordStep]
  deriving (Eq,Show)

type AltInt = (Int,Maybe ChordAlt)
     
data ChordModifier = CModM 
                   | CModMinor
                   | CModDim
                   | CModAug
                   | CModMaj7
                   | CModSus Int 
  deriving (Eq,Show)

data ChordStep = CStepAdd Int (Maybe ChordAlt)
               | CStepRemove Int 
  deriving (Eq,Show)
  
data ChordAlt = CAltPlus
              | CAltMinus
  deriving (Eq,Show)             
                       
--------------------------------------------------------------------------------
-- Guitar
--------------------------------------------------------------------------------

data GuitarNote = GuitarNote Note Int
  deriving (Eq,Show)
  
                           
--------------------------------------------------------------------------------
-- Lyrics
--------------------------------------------------------------------------------

data LyricContent = Syllable [Char] SyllableCont (Maybe Int)
                  | CenteredHyphen      -- "--"
                  | ExtenderLine        -- "__"
  deriving (Eq,Show)
  
data SyllableCont = SyllableCont | NoSyllableCont
  deriving (Eq,Show)

--------------------------------------------------------------------------------
-- Percussion
--------------------------------------------------------------------------------
type Drumname = String

data Drumnote = Drumnote Drumname (Maybe Duration)
  deriving (Eq,Show)
  
  