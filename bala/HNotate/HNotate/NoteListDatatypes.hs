{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# OPTIONS -Wall #-}

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
import HNotate.Fits
import HNotate.Pitch
import HNotate.SequenceUtils

import Control.Applicative hiding (empty)
import qualified Data.Foldable as F
import qualified Data.Map as Map
import Data.Monoid
import Data.Ratio
import Data.Sequence hiding (length, null)
import qualified Data.Sequence as S
import Data.Traversable
import Numeric (fromRat)


data OutputFormat = Abc | Ly | Midi
  deriving (Eq,Show) 
  
--------------------------------------------------------------------------------
-- The base view of printable elements 
-- - atoms and 'groupings' which group atoms.

data Annotation = Annotation { _ly_anno   :: ODoc -> ODoc, 
                               _abc_anno  :: ODoc -> ODoc }  

           
instance Show Annotation where
  show (Annotation _ _) = "<Annotation>"
  

  
  
type Label = String

-- splitting!
-- chords split differently to beam-groups 

-- Marks might have duration (RhythmicMark) - e.g. 'drum pitches'
-- Or they may just be typographical symbols (Mark) - e.g. tie
data Atom = Note Pitch Duration Annotation
          | Rest   Duration Annotation
          | Spacer Duration Annotation
          | forall a. RhythmicMark Label Duration (Mark a)
          | forall a. Mark Label (Mark a)
          | BeamStart
          | BeamEnd
          | Tie

instance Show Atom where
  showsPrec i (Note p d a)          = constrS "Note" (showsPrecChain3 i p d a)     
  showsPrec i (Rest d a)            = constrS "Rest" (showsPrecChain2 i d a)
  showsPrec i (Spacer d a)          = constrS "Spacer" (showsPrecChain2 i d a)
  showsPrec i (RhythmicMark l d m)  = constrS "RhythmicMark" $ 
                                                     (showsPrecChain3 i l d m)
  showsPrec i (Mark l m)            = constrS "Mark" (showsPrecChain2 i l m)
  showsPrec _ BeamStart             = showString "BeamStart"
  showsPrec _ BeamEnd               = showString "BeamEnd"
  showsPrec _ Tie                   = showString "Tie"
           
           


-- If we wanted rhythmical calculations on grace notes we need
-- to know which note to subtract the grace note durations from.
-- This would be necesary for output to Midi, but for LilyPond or Abc
-- it is (probably) irrelevant as the collective duration of grace notes
-- doesn't count towards the duration of a bar.  

data GraceMode = UGrace  -- unaccented - subtract duration from preceeding note 
               | AGrace  -- accented   - subtract duration from following note   
  deriving (Eq,Show)

-- The Grouping datatype - represents elements with a 'unit duration'.
-- E.g a chord has a set of pitches but the unit duration is common to all 
-- of them. 

-- Note - not much structure is imposed on the 'music'. 
-- There are special constructors for Grouping only where 
-- they need special interpretation for duration:
-- chord - simultaneous, gracesnotes - subraction from preceeding or suceeding
-- note.
-- Other grouped elements (e.g. notes grouped by a slur) have no 'syntax',
-- instead the are represented 'lexically' - a slur_begin mark would indicate
-- the start of a slur and a slur_end mark the end.
-- HNotate generally views the notelist as a stream of lexemes rather than a
-- parse tree.

-- individual grace notes can be annotated
type GraceNote = (Pitch,Duration,Annotation) 


data Grouping = Singleton { element         :: Atom }                  
              
              | Chord { chord_elements      :: Seq (Pitch, Annotation), 
                        rhythmic_value      :: Duration,
                        annotation          :: Annotation }
          
              | GraceNotes { grace_elements :: Seq GraceNote,
                             grace_mode     :: GraceMode,
                             annotation     :: Annotation } 
                             
              | Nplet { nplet_multipier     :: Int,
                        unit_duration       :: Duration,
                        nplet_elements      :: Seq (Pitch, Annotation),
                        annotation          :: Annotation }                   
      deriving (Show)   
  

data Mark phantom = Marker { _ly_output   :: ODoc,
                             _abc_output  :: ODoc }

instance Show (Mark a) where
  show (Marker _ _) = "<Mark>"          

npletDuration :: Int -> Duration -> Duration
npletDuration len unit_d = (fromIntegral len % 1) * unit_d                                        
                                           
--------------------------------------------------------------------------------
-- The External view - EventList - events with no rhythmical grouping
type System = Map.Map String EventList


-- (Call it a _List_ even though it is really a Seq)
newtype EventListF evt = EventList { getEventList :: Seq evt }
    deriving (Show) 
    
type EventList = EventListF Event  


data Event = SingleE Grouping
           | OverlayE [EventListF Event]  
    deriving (Show)              
         


--------------------------------------------------------------------------------   
-- The internal view
    
-- A 'horizontal line' of music - of course it maybe printed on 
-- more than one line and might contain polyphony, but it will be played 
-- by a single instrument.
newtype NoteListF e   = NoteList { getNoteList :: Seq (BlockF e) }

type NoteList         = NoteListF Grouping


-- Follow the Abc style when voice overlays are grouped in whole bars.
data BlockF e         = SingleBlock (BarF e)
                      | OverlayBlock (Seq (BarF e))

type Block            = BlockF Grouping

newtype BarF e        = Bar { getBar :: Seq e }

type Bar              = BarF Grouping
  

--------------------------------------------------------------------------------
-- Functor instances


instance Functor NoteListF where
  fmap f (NoteList se)            = NoteList (fmap (fmap f) se)
  
instance Functor BlockF where
  fmap f (SingleBlock e)          = SingleBlock (fmap f e)
  fmap f (OverlayBlock se)        = OverlayBlock (fmap (fmap f) se)
  
instance Functor BarF where
  fmap f (Bar se)                 = Bar (fmap f se)



--------------------------------------------------------------------------------
-- Foldable instances
  
instance F.Foldable NoteListF where
  foldMap f (NoteList se)         = F.foldMap (F.foldMap f) se
  
instance F.Foldable BlockF where
  foldMap f (SingleBlock e)       = F.foldMap f e
  foldMap f (OverlayBlock se)     = F.foldMap (F.foldMap f) se
  
instance F.Foldable BarF where
  foldMap f (Bar se)              = F.foldMap f se
  

--------------------------------------------------------------------------------
-- Traversable instances

  
instance Traversable NoteListF where
  traverse f (NoteList se)        = NoteList <$> traverse (traverse f) se

instance Traversable BlockF where
  traverse f (SingleBlock e)      = SingleBlock  <$> traverse f e
  traverse f (OverlayBlock se)    = OverlayBlock <$> traverse (traverse f) se
 
instance Traversable BarF where
  traverse f (Bar se)             = Bar <$> traverse f se

--------------------------------------------------------------------------------
-- RhythmicValue insatnces   
  
instance RhythmicValue Grouping where
  rhythmicValue (Singleton e)           = rhythmicValue e
  rhythmicValue (Chord _ d _)           = d
  rhythmicValue (GraceNotes _ _ _)      = duration_zero
  rhythmicValue (Nplet i d _ _)         = npletDuration i d
 
  
  updateDuration d (Singleton e)        = Singleton $ updateDuration d e
  updateDuration d (Chord se _ a)       = Chord se d a
  updateDuration _ (GraceNotes se m a)  = GraceNotes se m a
  updateDuration d (Nplet i _ se a)     = Nplet i ud se a
    where ud = reunit d i se
    
reunit :: Duration -> Int -> Seq a -> Duration
reunit tot i se = let l = S.length se in 
                  tot * (makeDuration l i) * (makeDuration 1 l)


  
instance RhythmicValue Atom where
  rhythmicValue (Note _ d _)            = d
  rhythmicValue (Rest d _)              = d
  rhythmicValue (Spacer d _)            = d
  rhythmicValue (RhythmicMark _ d _)    = d
  rhythmicValue (Mark _ _)              = duration_zero
  rhythmicValue BeamStart               = duration_zero
  rhythmicValue BeamEnd                 = duration_zero
  rhythmicValue Tie                     = duration_zero
  
  updateDuration d (Note p _ a)         = Note p d a
  updateDuration d (Rest _ a)           = Rest d a
  updateDuration d (Spacer _ a)         = Spacer d a
  updateDuration d (RhythmicMark l _ m) = RhythmicMark l d m
  updateDuration _ (Mark l m)           = Mark l m
  updateDuration _ BeamStart            = BeamStart
  updateDuration _ BeamEnd              = BeamEnd
  updateDuration _ Tie                  = Tie

instance PitchValue Grouping where
  pitchValue (Singleton e)            = pitchValue e
  pitchValue (Chord se _ _)           = pitchValue se
  pitchValue (GraceNotes se _ _)      = pitchValue se
  pitchValue (Nplet _ _ se _)         = pitchValue se
 
  
  updatePitch pc (Singleton e)        = Singleton (updatePitch pc e)
  updatePitch pc (Chord se d a)       = Chord (updatePitch pc se) d a
  updatePitch pc (GraceNotes se m a)  = GraceNotes (updatePitch pc se) m a
  updatePitch pc (Nplet i ud se a)    = Nplet i ud (updatePitch pc se) a
    
instance PitchValue Atom where
  pitchValue (Note p _ _)            = pitchValue p
  pitchValue (Rest _ _)              = noPitchContent
  pitchValue (Spacer _ _)            = noPitchContent
  pitchValue (RhythmicMark _ _ _)    = noPitchContent
  pitchValue (Mark _ _)              = noPitchContent
  pitchValue BeamStart               = noPitchContent
  pitchValue BeamEnd                 = noPitchContent
  pitchValue Tie                     = noPitchContent
  
  updatePitch pc (Note p d a)         = Note (updatePitch pc p) d a
  updatePitch _  (Rest d a)           = Rest d a
  updatePitch _  (Spacer d a)         = Spacer d a
  updatePitch _  (RhythmicMark l d m) = RhythmicMark l d m
  updatePitch _  (Mark l m)           = Mark l m
  updatePitch _  BeamStart            = BeamStart
  updatePitch _  BeamEnd              = BeamEnd
  updatePitch _  Tie                  = Tie

instance PitchValue (Seq GraceNote) where
  pitchValue = map f . F.toList where f (a,_,_) = a
  
  updatePitch pc se 
      | S.length se == length pc  = sziplWith (\(_,d,a) p -> (p,d,a)) se pc
      | otherwise                 = error "modifyPitch (Seq GraceNote) unmatched"
  
instance PitchValue (Seq (Pitch,Annotation)) where
  pitchValue = map fst . F.toList
  
  updatePitch pc se 
      | S.length se == length pc  = sziplWith (\(_,a) p -> (p,a)) se pc
      | otherwise                 = error $ 
            "modifyPitch (Seq (Pitch,Annotation)) unmatched" 
--------------------------------------------------------------------------------
-- Fits instances
    
instance Fits Atom Duration where
  measure  e  = rhythmicValue e
  
  split l e   = let (el,er) = splitRV l e in (el, noAnno er)
  
  hyphenate (Note _ _ _)            = Just Tie
  hyphenate (Rest _ _)              = Nothing
  hyphenate (Spacer _ _)            = Nothing
  hyphenate (RhythmicMark _ _ _)    = Just Tie
  hyphenate (Mark _ _)              = Nothing
  hyphenate BeamStart               = Nothing
  hyphenate BeamEnd                 = Nothing
  hyphenate Tie                     = Nothing
  
instance Fits Grouping Duration where
  measure  e  = rhythmicValue e
  
  split l (Singleton e)       = 
      let (a,b) = split l e in (Singleton a, Singleton $ noAnno b)
      
  split l (Chord se d a)      = 
      let (da,db) = split l d in (Chord se da a, Chord se db mempty)
   
  -- grace notes shouldn't be split, if they are then the calling 
  -- function is wrong...  
  -- It might of course be better to represent gracenotes along
  -- with the note they grace, Bala does this.   
  split _ (GraceNotes _ _ _) = error $ "can't split gracenotes"
      
      
  split l (Nplet i ud se a)   = (Nplet i ud sa a, Nplet i ud sb mempty) where
      (da,db)   = npletDuration (S.length se) l `split` ud    
      sza       :: Double
      sza       = fromRat $ (da/db) * (fromIntegral (S.length se) % 1)
      (sa,sb)   = S.splitAt (floor  sza) se


  hyphenate (Chord _ _ _)     = Just (Singleton Tie)
  hyphenate _                 = Nothing
       
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

instance Monoid Annotation where
  mempty = Annotation { _ly_anno=id, _abc_anno=id } 
  Annotation f g `mappend` Annotation f' g' = Annotation (f . f') (g . g') 

-- need a class like PitchValue for Annotation....

class NoAnno a where noAnno :: a -> a

instance NoAnno GraceNote where
  noAnno (p,d,_) = (p,d,mempty)

instance NoAnno (Seq GraceNote) where
  noAnno = fmap noAnno 

instance NoAnno Atom where
  noAnno (Note p d _)   = Note p d mempty
  noAnno (Rest d _)     = Rest d mempty
  noAnno (Spacer d _)   = Spacer d mempty
  noAnno a              = a
  
--------------------------------------------------------------------------------
-- Shorthand constructors / builders for the external view


(|*>) :: EventList -> Event -> EventList
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
poly xs t         = t |*> OverlayE xs

event             :: Grouping -> EventList -> EventList
event e t         = t |*> SingleE e 


noteSgl           :: Pitch -> Duration -> Grouping 
noteSgl p d       = Singleton $ Note p d mempty

noteSgl'          :: Pitch -> Duration -> Annotation -> Grouping 
noteSgl' p d a    = Singleton $ Note p d a 

note              :: Pitch -> Duration -> EventList -> EventList
note  p d         = event (noteSgl p d)

note'             :: Pitch -> Duration -> Annotation -> EventList -> EventList
note' p d a       = event (noteSgl' p d a)

restSgl           :: Duration -> Grouping 
restSgl d         = Singleton $ Rest d mempty 

restSgl'          :: Duration -> Annotation -> Grouping 
restSgl' d a      = Singleton $ Rest d a

rest              :: Duration -> EventList -> EventList
rest d            = event (restSgl d)

rest'             :: Duration -> Annotation -> EventList -> EventList
rest' d a         = event (restSgl' d a)

spacerSgl         :: Duration -> Grouping 
spacerSgl d       = Singleton $ Spacer d mempty 

spacerSgl'        :: Duration -> Annotation -> Grouping 
spacerSgl' d a    = Singleton $ Spacer d a

spacer            :: Duration -> EventList -> EventList
spacer d          = event (spacerSgl d)

spacer'           :: Duration -> Annotation -> EventList -> EventList
spacer' d a       = event (spacerSgl' d a)

chordGrp          :: Seq Pitch -> Duration -> Grouping
chordGrp se d     = Chord (fmap f se) d mempty where
    f a  = (a,mempty)

chordGrp'         :: Seq (Pitch,Annotation) -> Duration -> Annotation -> Grouping
chordGrp' se d a  = Chord se d a

chordGrpL         :: [Pitch] -> Duration -> Grouping
chordGrpL xs d    = Chord (fromList $ fmap f xs) d mempty where
    f a = (a,mempty)

chordGrpL'         :: [(Pitch,Annotation)] -> Duration ->  Annotation -> Grouping
chordGrpL' xs d a  = Chord (fromList xs) d a


chord             :: Seq Pitch -> Duration -> EventList -> EventList
chord se d        = event (chordGrp se d)

chord'            :: Seq (Pitch,Annotation) -> Duration -> Annotation -> EventList -> EventList
chord' se d a     = event (chordGrp' se d a)



chordL            :: [Pitch] -> Duration -> EventList -> EventList
chordL xs d       = event (chordGrpL xs d)

chordL'           :: [(Pitch,Annotation)] -> Duration -> Annotation -> EventList -> EventList
chordL' xs d a    = event (chordGrpL' xs d a)

ugracesGrp        :: Seq (Pitch,Duration) -> Grouping
ugracesGrp se     = GraceNotes (fmap f se) UGrace mempty where
    f (a,b) = (a,b,mempty)

ugracesGrp'       :: Seq (Pitch,Duration,Annotation) -> Annotation -> Grouping
ugracesGrp' se a  = GraceNotes se UGrace a

ugraces           :: Seq (Pitch,Duration) -> EventList -> EventList
ugraces se        = event (ugracesGrp se)

ugraces'          :: Seq (Pitch,Duration,Annotation) -> Annotation -> EventList -> EventList
ugraces' se a     = event (ugracesGrp' se a)

ugracesGrpL       :: [(Pitch,Duration)] -> Grouping
ugracesGrpL xs    = ugracesGrp (fromList xs)

ugracesGrpL'      :: [(Pitch,Duration,Annotation)] -> Annotation -> Grouping
ugracesGrpL' xs a = ugracesGrp' (fromList xs) a

ugracesL          :: [(Pitch,Duration)] -> EventList -> EventList
ugracesL xs       = event (ugracesGrpL xs)

ugracesL'         :: [(Pitch,Duration,Annotation)] -> Annotation -> EventList -> EventList
ugracesL' xs a    = event (ugracesGrpL' xs a)

agracesGrp        :: Seq (Pitch,Duration) -> Grouping
agracesGrp se     = GraceNotes (fmap f se) AGrace mempty where
    f (a,b) = (a,b,mempty)

agracesGrp'       :: Seq GraceNote -> Annotation -> Grouping
agracesGrp' se a  = GraceNotes se AGrace a

agraces           :: Seq (Pitch,Duration) -> EventList -> EventList
agraces se        = event (agracesGrp se)

agraces'          :: Seq GraceNote -> Annotation -> EventList -> EventList
agraces' se a     = event (agracesGrp' se a)

agracesGrpL       :: [(Pitch,Duration)] -> Grouping
agracesGrpL xs    = agracesGrp (fromList xs) 

agracesGrpL'      :: [(Pitch,Duration,Annotation)] -> Annotation -> Grouping
agracesGrpL' xs a = agracesGrp' (fromList xs) a

agracesL          :: [(Pitch,Duration)] -> EventList -> EventList
agracesL xs       = event (agracesGrpL xs)

agracesL'         :: [(Pitch,Duration,Annotation)] -> Annotation -> EventList -> EventList
agracesL' xs a    = event (agracesGrpL' xs a)

npletGrp          :: Int -> Duration -> Seq Pitch -> Grouping
npletGrp i ud se  = Nplet i ud (fmap f se) mempty where
    f a = (a,mempty)  

npletGrp'         :: Int -> Duration -> Seq (Pitch,Annotation) -> Annotation -> Grouping
npletGrp' i ud se a   = Nplet i ud se a

nplet             :: Int -> Duration -> Seq Pitch -> EventList -> EventList
nplet i ud se     = event (npletGrp i ud se)

nplet' :: Int -> Duration -> Seq (Pitch,Annotation) ->  Annotation -> EventList -> EventList
nplet' i ud se a  = event (npletGrp' i ud se a)
    
tieSgl     :: Grouping
tieSgl     = Singleton Tie

tie             :: EventList -> EventList
tie             = event tieSgl


beamStartSgl  :: Grouping
beamStartSgl  = Singleton BeamStart 

beamStart     :: EventList -> EventList
beamStart     = event beamStartSgl

beamEndSgl    :: Grouping
beamEndSgl    = Singleton BeamEnd

beamEnd       :: EventList -> EventList
beamEnd       = event beamEndSgl

simpleEventlist        :: [Pitch] -> Duration -> EventList
simpleEventlist ps d   = foldl (\t p -> t # note p d) root ps



emptyGrouping :: Grouping -> Bool
emptyGrouping (Singleton _)         = False
emptyGrouping (Chord se _ _)        = S.null se
emptyGrouping (GraceNotes se _ _)   = S.null se
emptyGrouping (Nplet _ _ se _)      = S.null se
  
        
composeAnnos :: Annotation -> Annotation -> Annotation
composeAnnos (Annotation ly1 abc1) (Annotation ly2 abc2) =
    Annotation (ly2 . ly1) (abc2 . abc1) 




