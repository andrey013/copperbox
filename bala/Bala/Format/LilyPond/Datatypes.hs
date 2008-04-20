
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


type Either3 a b c = Either a (Either b c)


data Glyph = GlyphEvent (Either3 Note Chord Rest)
           | GlyphCommmand Command
           | GlyphMark Mark
  deriving (Eq,Show)          


type Note = (Pitch,Maybe Duration) 

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


data Chord = Chord [Pitch] Duration
  deriving (Eq,Show)
  
   

data Command = CmdNew String  String String  -- type music expression 
             | CmdKey Pitch Command
             | CmdVersion [Int]
             | CmdTimeSignature Int Int
             | CmdTempo Duration Int
             | NullaryCommand String
             | ExprCommand String Water
             | UnaryCommand String String
             | BinaryCommand String String String
  deriving (Eq, Show)
             
data Mark = MarkTie
          | MarkSlur Slur
          | MarkBeam Beam
  deriving (Eq,Show)

data Slur = SlurStart (Maybe VerticalPlacement)
          | SlurEnd
  deriving (Eq,Show)

data Beam = BeamStart
          | BeamEnd
  deriving (Eq,Show)    
    
    
            
    
                       
  