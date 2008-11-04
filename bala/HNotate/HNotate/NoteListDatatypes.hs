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


data OutputFormat = Abc | Ly  
  deriving (Eq,Show) 
  
--------------------------------------------------------------------------------
-- The base view of printable elements - glyphs and 'tiles' which group glyphs.

data Annotation = Annotation { _ly_anno   :: ODoc -> ODoc, 
                               _abc_anno  :: ODoc -> ODoc }  

data RestMode = Marked | Spacer 
  deriving (Eq,Show)

type Label = String

-- splitting!
-- chords split differently to beam-groups 

-- Marks might have duration - e.g. 'drum pitches'
-- or they may just be typographical symbols - e.g. tie
data Glyph = Note Pitch Duration Annotation
           | Rest RestMode Duration Annotation
           | RhythmicMark Label Duration Mark
           | Mark Label Mark




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
                  
          -- TODO tuplets (generalized to n-plets)        
  
  
-- s is some 'state' - this was useful in the prototype 
-- but it might be redundant now. 
data MarkF s = MarkF { _ly_output   :: s -> ODoc,
                       _abc_output  :: s -> ODoc }

data Mark = forall s . M (s -> MarkF s) s
           
instance Show Annotation where
  showsPrec i anno = constrS "Annotation" (showString "<fun> <fun>")

instance Show Tile where
  showsPrec i (Singleton e)       = constrS "Singleton" (showsPrec i e)
  
  showsPrec i (Chord se d a)      = 
      constrS "Chord" (showsPrecChain2 i se d . showString " <anno>")
      
  showsPrec i (GraceNotes se m a) = 
      constrS "GraceNotes" (showsPrecChain2 i se m . showString " <anno>")
  
  
instance Show Glyph where
  showsPrec i (Note p d a)          = 
      constrS "Note" (showsPrecChain2 i p d . showString " <anno>")
  
  showsPrec i (Rest m d a)          = 
      constrS "Rest" (showsPrecChain2 i m d . showString " <anno>")
  
  showsPrec i (RhythmicMark l d m)  = 
      constrS "RhythmicMark" (showString l . showSpace . shows d 
                                           . showSpace . showString "<mark>")
      
  showsPrec i (Mark l m)  = 
      constrS "Mark" (showString l . showSpace . showString "<mark>")
                                           
                                           
--------------------------------------------------------------------------------
-- The External view - EventList - events with no rhythmical grouping
type System = Map.Map String EventList


-- (Call it a _List_ even though it is really a Seq)
newtype EventListF evt = EventList { getEventList :: Seq evt }

type EventList = EventListF Evt  



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
-- RhythmicValue - the duration of a tile or glyph  

class RhythmicValue a where
  rhythmicValue   :: a -> Duration
  modifyDuration  :: a -> Duration -> a
  
instance RhythmicValue Tile where
  rhythmicValue (Singleton e)         = rhythmicValue e
  rhythmicValue (Chord _ d _)         = d
  rhythmicValue (GraceNotes _ _ _)    = duration_zero
  
  modifyDuration (Singleton e)        d = Singleton (e `modifyDuration` d)
  modifyDuration (Chord se _ a)       d = Chord se d a
  modifyDuration (GraceNotes se m a)  d = GraceNotes se m a
  
  
instance RhythmicValue Glyph where
  rhythmicValue (Note _ d _)          = d
  rhythmicValue (Rest _ d _)          = d
  rhythmicValue (RhythmicMark _ d _)  = d
  rhythmicValue (Mark _ _)            = duration_zero

  modifyDuration (Note p _ a)         d = Note p d a
  modifyDuration (Rest m _ a)         d = Rest m d a
  modifyDuration (RhythmicMark l _ m) d = RhythmicMark l d m
  modifyDuration (Mark l m)           _ = Mark l m

--------------------------------------------------------------------------------
-- Annotations and marks

applyLyAnno :: Annotation -> ODoc -> ODoc
applyLyAnno (Annotation {_ly_anno=f}) d = f d 

applyAbcAnno :: Annotation -> ODoc -> ODoc
applyAbcAnno (Annotation {_abc_anno=f}) d = f d   
  
lyOutput :: Mark -> ODoc
lyOutput (M fs s) = _ly_output (fs s) s

abcOutput :: Mark -> ODoc
abcOutput (M fs s) = _abc_output (fs s) s
  
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

noAnno :: Annotation 
noAnno = Annotation { _ly_anno=id, _abc_anno=id }  

note                :: Pitch -> Duration -> Tile
note p d            = Singleton (Note p d noAnno)


rest                :: Duration -> Tile
rest d              = Singleton (Rest Marked d noAnno)

spacer              :: Duration -> Tile
spacer d            = Singleton (Rest Spacer d noAnno)

chord               :: [Pitch] -> Duration -> Tile
chord es d          = Chord (fromList es) d noAnno
    
gracenotesU         :: [(Pitch,Duration)] -> Tile
gracenotesU es      = GraceNotes (fromList es) UGrace noAnno

gracenotesA         :: [(Pitch,Duration)] -> Tile
gracenotesA es      = GraceNotes (fromList es) AGrace noAnno





simpleEventlist        :: [Pitch] -> Duration -> EventList
simpleEventlist ps d   = foldl (\t p -> t |# note p d) root ps



emptyTile :: Tile -> Bool
emptyTile (Singleton _)         = False
emptyTile (Chord se _ _)        = null se
emptyTile (GraceNotes se _ _)   = null se

infixl 6 |#

class AddtoEventList a where 
  (|#) :: EventList -> a -> EventList
  
instance AddtoEventList Tile where   
  (|#) evts t     | emptyTile t = evts
                  | otherwise   = EventList $ getEventList evts |> (Evt t)  


instance AddtoEventList [EventList] where 
  (|#) evts []  = evts
  (|#) evts [x] = EventList $ getEventList evts >< getEventList x
  (|#) evts es  = EventList $ getEventList evts |> (Poly es)


-- forgetful annotation 

infixl 7 /@

(/@) :: Tile -> Annotation -> Tile 
(/@) (Chord se d _)       a = Chord se d a
(/@) (GraceNotes se m _)  a = GraceNotes se m a
(/@) (Singleton glyph)    a = Singleton $ anno glyph a
  where
    anno (Note p d _) a = Note p d a
    anno (Rest m d _) a = Rest m d a
    anno glyph        a = glyph

infixl 7 /@@

-- composing annotation
(/@@) :: Tile -> Annotation -> Tile 
(/@@) (Chord se d a)       a' = Chord se d (composeAnnos a a')
(/@@) (GraceNotes se m a)  a' = GraceNotes se m (composeAnnos a a')
(/@@) (Singleton glyph)    a' = Singleton $ anno glyph a'
  where
    anno (Note p d a) a' = Note p d (composeAnnos a a')
    anno (Rest m d a) a' = Rest m d (composeAnnos a a')
    anno glyph        a' = glyph
    
        
composeAnnos :: Annotation -> Annotation -> Annotation
composeAnnos (Annotation ly1 abc1) (Annotation ly2 abc2) =
    Annotation (ly2 . ly1) (abc2 . abc1) 




--------------------------------------------------------------------------------
-- Ties and Beams

-- Define Ties and Beams as they are used internally for rhythmic division
-- of the event list.


-- beamStart and beamEnd have an 'interpretation' for Abc: they indicate the
-- change and revert of the note concatenation op. 
-- So we need the two predicates.

isBeamStart :: Glyph -> Bool
isBeamStart (Mark "beamStart" _)  = True
isBeamStart _                     = False

isBeamEnd :: Glyph -> Bool
isBeamEnd (Mark "beamEnd" _)      = True
isBeamEnd _                       = False


beamStart :: Tile
beamStart = Singleton $ Mark "beamStart" (M fs ())
  where 
    fs = (\() -> MarkF { _ly_output = \() -> lbracket,
                         _abc_output = \() -> emptyDoc })  

beamEnd :: Tile
beamEnd = Singleton $ Mark "beamEnd" (M fs ())
  where 
    fs = (\() -> MarkF { _ly_output = \() -> rbracket,
                         _abc_output = \() -> emptyDoc }) 


    
tie :: Tile
tie = Singleton $ Mark "tie" (M fs ())
  where 
    fs = (\() -> MarkF { _ly_output = \() -> char '~',
                         _abc_output = \() -> char '~' })  
