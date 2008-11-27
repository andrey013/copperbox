{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}


--------------------------------------------------------------------------------
-- |
-- Module      :  HNotate.NoteListDatatypes
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Datatypes for NoteList format
--
--
--------------------------------------------------------------------------------

module HNotate.NoteListDatatypes where


import HNotate.CommonUtils
import HNotate.Document
import HNotate.Duration hiding ( _duration )
import HNotate.Pitch


import Control.Applicative hiding (empty)
import qualified Data.Foldable as F
import qualified Data.Map as Map
import Data.Monoid
import Data.Sequence
import Data.Traversable
import Prelude hiding (null)


data OutputFormat = Abc | Ly | Midi
  deriving (Eq,Show) 
  
--------------------------------------------------------------------------------
-- The base view of printable elements - glyphs and 'tiles' which group glyphs.

data Annotation = Annotation { _ly_anno   :: ODoc -> ODoc, 
                               _abc_anno  :: ODoc -> ODoc }  

           
instance Show Annotation where
  show (Annotation _ _) = "<Annotation>"
  
  
type Label = String

-- splitting!
-- chords split differently to beam-groups 

-- Marks might have duration (RhythmicMark) - e.g. 'drum pitches'
-- Or they may just be typographical symbols (Mark) - e.g. tie
data Glyph = Note Pitch Duration Annotation
           | Rest   Duration Annotation
           | Spacer Duration Annotation
           | forall a. RhythmicMark Label Duration (Mark a)
           | forall a. Mark Label (Mark a)
           | BeamStart
           | BeamEnd
           | Tie

instance Show Glyph where
  showsPrec i (Note p d a)          = constrS "Note" (showsPrecChain3 i p d a)     
  showsPrec i (Rest d a)            = constrS "Rest" (showsPrecChain2 i d a)
  showsPrec i (Spacer d a)          = constrS "Spacer" (showsPrecChain2 i d a)
  showsPrec i (RhythmicMark l d m)  = constrS "RhythmicMark" $ 
                                                     (showsPrecChain3 i l d m)
  showsPrec i (Mark l m)            = constrS "Mark" (showsPrecChain2 i l m)
  showsPrec i BeamStart             = showString "BeamStart"
  showsPrec i BeamEnd               = showString "BeamEnd"
  showsPrec i Tie                   = showString "Tie"
           
           
-- individual grace notes cannot be annotated
type GraceNote = (Pitch,Duration) 

-- If we wanted rhythmical calculations on grace notes we need
-- to know which note to subtract the grace note durations from.
-- This would be necesary for output to Midi, but for LilyPond or Abc
-- it is (probably) irrelevant as the collective duration of grace notes
-- doesn't count towards the duration of a bar.  

data GraceMode = UGrace  -- unaccented - subtract duration from preceeding note 
               | AGrace  -- accented   - subtract duration from following note   
  deriving (Eq,Show)

-- The Tile datatype imposes little structure on the 'music'. 
-- There are special sorts  for groups (chord, gracesnotes) only where 
-- they need special interpretation for duration (rhythmic value).
-- Other grouped elements (e.g. notes grouped by a slur) have no 'syntax',
-- instead the are represented 'lexically' - a slur_begin mark would indicate
-- the start of a slur and a slur_end mark the end.
-- HNotate generally views the notelist as a stream of lexemes rather than a
-- parse tree.

-- No annotation for a singleton - annotations are contained in the Glyph. 
data Tile = Singleton { element         :: Glyph }                  
          
          | Chord { chord_elements      :: Seq Pitch, 
                    rhythmic_value      :: Duration,
                    annotation          :: Annotation }
          
          | GraceNotes { grace_elements :: Seq GraceNote,
                         grace_mode     :: GraceMode,
                         annotation     :: Annotation }     
  deriving (Show)                             
                  
          -- TODO tuplets (generalized to n-plets)        
  

data Mark phantom = Marker { _ly_output   :: ODoc,
                             _abc_output  :: ODoc }

instance Show (Mark a) where
  show (Marker _ _) = "<Mark>"          






                                           
                                           
--------------------------------------------------------------------------------
-- The External view - EventList - events with no rhythmical grouping
type System = Map.Map String EventList


-- (Call it a _List_ even though it is really a Seq)
newtype EventListF evt = EventList { getEventList :: Seq evt }

type EventList = EventListF Evt  


-- Change the type, or how its used?
data Evt = Evt Tile
         | Poly [EventListF Evt]         
         
instance Monoid (EventListF evt) where
  mempty      = EventList mempty
  mappend a b = EventList $ (getEventList a) >< (getEventList b)

instance Show (EventListF Evt) where
  showsPrec i _ = showString "Show (EventListF Evt) - todo"

instance Show Evt where
  showsPrec i _ = showString "Show Evt - todo"
    
--------------------------------------------------------------------------------   
-- The internal view
    
-- A 'horizontal line' of music - of course it maybe printed on 
-- more than one line and might contain polyphony, but it will be played 
-- by a single instrument.
newtype NoteListF e   = NoteList { getNoteList :: Seq (BlockF e) }

type NoteList         = NoteListF Tile


-- Follow the Abc style when voice overlays are grouped in whole bars.
-- The Int holds the bar number
data BlockF e         = SingleBlock Int (BarF e)
                      | PolyBlock   Int (Seq (BarF e))

type Block            = BlockF Tile

newtype BarF e        = Bar { getBar :: Seq e }

type Bar              = BarF Tile
  

--------------------------------------------------------------------------------
-- Functor instances
  
instance Functor NoteListF where
  fmap f (NoteList se)            = NoteList (fmap (fmap f) se)
  
instance Functor BlockF where
  fmap f (SingleBlock i e)        = SingleBlock i (fmap f e)
  fmap f (PolyBlock i se)         = PolyBlock i (fmap (fmap f) se)
  
instance Functor BarF where
  fmap f (Bar se)                 = Bar (fmap f se)



--------------------------------------------------------------------------------
-- Foldable instances
  
instance F.Foldable NoteListF where
  foldMap f (NoteList se)         = F.foldMap (F.foldMap f) se
  
instance F.Foldable BlockF where
  foldMap f (SingleBlock i e)     = F.foldMap f e
  foldMap f (PolyBlock i se)      = F.foldMap (F.foldMap f) se
  
instance F.Foldable BarF where
  foldMap f (Bar se)              = F.foldMap f se
  

--------------------------------------------------------------------------------
-- Traversable instances

  
instance Traversable NoteListF where
  traverse f (NoteList se)        = NoteList <$> traverse (traverse f) se

instance Traversable BlockF where
  traverse f (SingleBlock i e)    = (SingleBlock i) <$> traverse f e
  traverse f (PolyBlock i se)     = (PolyBlock i) <$> traverse (traverse f) se
 
instance Traversable BarF where
  traverse f (Bar se)             = Bar <$> traverse f se

--------------------------------------------------------------------------------
-- RhythmicValue insatnces   
  
instance RhythmicValue Tile where
  rhythmicValue (Singleton e)           = rhythmicValue e
  rhythmicValue (Chord _ d _)           = d
  rhythmicValue (GraceNotes _ _ _)      = duration_zero
  
  modifyDuration (Singleton e)        d = Singleton (e `modifyDuration` d)
  modifyDuration (Chord se _ a)       d = Chord se d a
  modifyDuration (GraceNotes se m a)  d = GraceNotes se m a
  
  
instance RhythmicValue Glyph where
  rhythmicValue (Note _ d _)            = d
  rhythmicValue (Rest d _)              = d
  rhythmicValue (Spacer d _)            = d
  rhythmicValue (RhythmicMark _ d _)    = d
  rhythmicValue (Mark _ _)              = duration_zero
  rhythmicValue BeamStart               = duration_zero
  rhythmicValue BeamEnd                 = duration_zero
  rhythmicValue Tie                     = duration_zero
  
  modifyDuration (Note p _ a)         d = Note p d a
  modifyDuration (Rest _ a)           d = Rest d a
  modifyDuration (Spacer _ a)         d = Spacer d a
  modifyDuration (RhythmicMark l _ m) d = RhythmicMark l d m
  modifyDuration (Mark l m)           d = Mark l m
  modifyDuration BeamStart            d = BeamStart
  modifyDuration BeamEnd              d = BeamEnd
  modifyDuration Tie                  d = Tie

instance PitchValue Glyph where
  pitchValue (Note p _ _)            = Just p
  pitchValue (Rest _ _)              = Nothing
  pitchValue (Spacer _ _)            = Nothing
  pitchValue (RhythmicMark _ _ _)    = Nothing
  pitchValue (Mark _ _)              = Nothing
  pitchValue BeamStart               = Nothing
  pitchValue BeamEnd                 = Nothing
  pitchValue Tie                     = Nothing
  
  modifyPitch (Note _ d a)         p = Note p d a
  modifyPitch (Rest d a)           p = Rest d a
  modifyPitch (Spacer d a)         p = Spacer d a
  modifyPitch (RhythmicMark l d m) p = RhythmicMark l d m
  modifyPitch (Mark l m)           p = Mark l m
  modifyPitch BeamStart            p = BeamStart
  modifyPitch BeamEnd              p = BeamEnd
  modifyPitch Tie                  p = Tie

  
--------------------------------------------------------------------------------
-- Annotations and marks

applyLyAnno :: Annotation -> ODoc -> ODoc
applyLyAnno (Annotation {_ly_anno=f}) d = f d 

applyAbcAnno :: Annotation -> ODoc -> ODoc
applyAbcAnno (Annotation {_abc_anno=f}) d = f d   
  
lyOutput :: Mark a -> ODoc
lyOutput mark = _ly_output mark

abcOutput :: Mark a -> ODoc
abcOutput mark = _abc_output mark
  
--------------------------------------------------------------------------------
-- Shorthand constructors / builders for the external view


(|*>) :: EventList -> Evt -> EventList
(|*>) (EventList t) evt = EventList $ t |> evt


system :: System 
system = mempty

systemL :: [(String, EventList)] -> System
systemL = Map.fromList

system1 :: String -> EventList -> System
system1 k t = Map.insert k t mempty

root :: EventList
root = EventList empty

poly              :: [EventList] -> EventList -> EventList
poly xs t         = t |*> Poly xs


noAnno :: Annotation 
noAnno = Annotation { _ly_anno=id, _abc_anno=id }  

noteTile          :: Pitch -> Duration -> Tile 
noteTile p d      = Singleton $ Note p d noAnno

noteTile'         :: Pitch -> Duration -> Annotation -> Tile 
noteTile' p d a   = Singleton $ Note p d a 

note              :: Pitch -> Duration -> EventList -> EventList
note  p d t       = t |*> Evt (noteTile p d)

note'             :: Pitch -> Duration -> Annotation -> EventList -> EventList
note' p d a t     = t |*> Evt (noteTile' p d a)

restTile          :: Duration -> Tile 
restTile d        = Singleton $ Rest d noAnno 

restTile'         :: Duration -> Annotation -> Tile 
restTile' d a     = Singleton $ Rest d a


rest              :: Duration -> EventList -> EventList
rest d t          = t |*> Evt (restTile d)

rest'             :: Duration -> Annotation -> EventList -> EventList
rest' d a t       = t |*> Evt (restTile' d a)

spacerTile        :: Duration -> Tile 
spacerTile d      = Singleton $ Spacer d noAnno 

spacerTile'       :: Duration -> Annotation -> Tile 
spacerTile' d a   = Singleton $ Spacer d a


spacer            :: Duration -> EventList -> EventList
spacer d t        = t |*> Evt (spacerTile d)

spacer'           :: Duration -> Annotation -> EventList -> EventList
spacer' d a t     = t |*> Evt (spacerTile' d a)

chordTile         :: Seq Pitch -> Duration -> Tile
chordTile se d    = Chord se d noAnno

chordTile'        :: Seq Pitch -> Duration -> Annotation -> Tile
chordTile' se d a = Chord se d a

chord             :: Seq Pitch -> Duration -> EventList -> EventList
chord se d t      = t |*> Evt (chordTile se d)

chord'            :: Seq Pitch -> Duration -> Annotation -> EventList -> EventList
chord' se d a t   = t |*> Evt (chordTile' se d a)

chordTileL        :: [Pitch] -> Duration ->  Annotation -> Tile
chordTileL xs d a = Chord (fromList xs) d a

chordL            :: [Pitch] -> Duration -> EventList -> EventList
chordL xs d t     = t |*> Evt (chordTileL xs d noAnno)

chordL'           :: [Pitch] -> Duration -> Annotation -> EventList -> EventList
chordL' xs d a t  = t |*> Evt (chordTileL xs d a)


ugracesTile       :: Seq (Pitch,Duration) -> Tile
ugracesTile se    = GraceNotes se UGrace noAnno

ugracesTile'      :: Seq (Pitch,Duration) -> Annotation -> Tile
ugracesTile' se a = GraceNotes se UGrace a

ugraces           :: Seq (Pitch,Duration) -> EventList -> EventList
ugraces se t      = t |*> Evt (ugracesTile se)

ugraces'          :: Seq (Pitch,Duration) -> Annotation -> EventList -> EventList
ugraces' se a t   = t |*> Evt (ugracesTile' se a)

ugracesTileL      :: [(Pitch,Duration)] -> Tile
ugracesTileL xs   = ugracesTile (fromList xs)

ugracesTileL'     :: [(Pitch,Duration)] -> Annotation -> Tile
ugracesTileL' xs a = ugracesTile' (fromList xs) a


ugracesL          :: [(Pitch,Duration)] -> EventList -> EventList
ugracesL xs t     = t |*> Evt (ugracesTileL xs)

ugracesL'         :: [(Pitch,Duration)] -> Annotation -> EventList -> EventList
ugracesL' xs a t  = t |*> Evt (ugracesTileL' xs a)


agracesTile       :: Seq (Pitch,Duration) -> Tile
agracesTile se    = GraceNotes se AGrace noAnno

agracesTile'      :: Seq (Pitch,Duration) -> Annotation -> Tile
agracesTile' se a  = GraceNotes se AGrace a

agraces           :: Seq (Pitch,Duration) -> EventList -> EventList
agraces se t      = t |*> Evt (agracesTile se)

agraces'          :: Seq (Pitch,Duration) -> Annotation -> EventList -> EventList
agraces' se a t   = t |*> Evt (agracesTile' se a)

agracesTileL      :: [(Pitch,Duration)] -> Tile
agracesTileL xs   = agracesTile (fromList xs) 

agracesTileL'     :: [(Pitch,Duration)] -> Annotation -> Tile
agracesTileL' xs a = agracesTile' (fromList xs) a


agracesL          :: [(Pitch,Duration)] -> EventList -> EventList
agracesL xs t     = t |*> Evt (agracesTileL xs)

agracesL'         :: [(Pitch,Duration)] -> Annotation -> EventList -> EventList
agracesL' xs a t  = t |*> Evt (agracesTileL' xs a)
 
    
tieTile :: Tile
tieTile = Singleton Tie

tie             :: EventList -> EventList
tie t           = t |*> Evt tieTile


beamStart :: Tile
beamStart = Singleton BeamStart 

beamEnd :: Tile
beamEnd = Singleton BeamEnd



simpleEventlist        :: [Pitch] -> Duration -> EventList
simpleEventlist ps d   = foldl (\t p -> t # note p d) root ps



emptyTile :: Tile -> Bool
emptyTile (Singleton _)         = False
emptyTile (Chord se _ _)        = null se
emptyTile (GraceNotes se _ _)   = null se

  
        
composeAnnos :: Annotation -> Annotation -> Annotation
composeAnnos (Annotation ly1 abc1) (Annotation ly2 abc2) =
    Annotation (ly2 . ly1) (abc2 . abc1) 




