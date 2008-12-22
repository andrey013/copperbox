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

import Control.Applicative hiding (empty)
import qualified Data.Foldable as F
import qualified Data.Map as Map
import Data.Monoid
import Data.Ratio
import Data.Sequence hiding (length, null)
import qualified Data.Sequence as S
import Data.Traversable
import Data.Typeable
import Numeric (fromRat)


data OutputFormat = OutputAbc | OutputLy
  deriving (Eq,Show) 
  
--------------------------------------------------------------------------------
-- The base view of printable elements 
-- /atoms/ and /elements/ which group atoms.

type Annotation = [WrappedAnno]

type AnnotationS =  Annotation ->  Annotation

data WrappedAnno = forall a. Typeable a => WrapAnno a

instance Show WrappedAnno where show _ = "<Anno>"


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
          | Tie

instance Show Atom where
  showsPrec i (Note p d a)          = constrS "Note" (showsPrecChain3 i p d a)     
  showsPrec i (Rest d a)            = constrS "Rest" (showsPrecChain2 i d a)
  showsPrec i (Spacer d a)          = constrS "Spacer" (showsPrecChain2 i d a)
  showsPrec i (RhythmicMark l d m)  = constrS "RhythmicMark" $ 
                                                     (showsPrecChain3 i l d m)
  showsPrec i (Mark l m)            = constrS "Mark" (showsPrecChain2 i l m)
  showsPrec _ Tie                   = showString "Tie"
           
           


-- The Element datatype - represents elements with a 'unit duration'.
-- E.g a chord has a set of pitches but the unit duration is common to all 
-- of them. 
data Element = Atom { element               :: Atom }                  
              
              | Chord { chord_elements      :: Seq (Pitch, Annotation), 
                        rhythmic_value      :: Duration,
                        annotation          :: Annotation }
          
              | GraceNotes { grace_elements :: Seq GraceNote,
                             annotation     :: Annotation } 
                             
              | Nplet { nplet_multipier     :: Int,
                        unit_duration       :: Duration,
                        nplet_elements      :: Seq (Pitch, Annotation),
                        annotation          :: Annotation }                   
      deriving (Show)   





-- individual grace notes can be annotated
type GraceNote = (Pitch,Duration,Annotation) 

data Mark phantom = Marker { _ly_output   :: ODoc,
                             _abc_output  :: ODoc }

instance Show (Mark a) where show _ = "<Mark>"          

-- Annotations are interpreted /outside/ HNotate. Thus every annotated 
-- constructor has a corresponding /tag/ which user code pattern matches on.
--  
-- There is no HnAtom - atoms are annotated individually (per constructor).
data HnElement = HnChord | HnGraceNotes | HnNplet
  deriving (Eq,Show)

-- Marks and RhythmicMarks are unannotated - annotations can be collected
-- in the mark itself.
data HnAtom = HnNote | HnRest | HnSpacer
  deriving (Eq,Show)
  
data AnnoEval = AnnoEval {
     annotate_element       :: HnElement -> Annotation -> ODocS,
     annotate_element_part  :: HnElement -> Annotation -> ODocS,
     annotate_atom          :: HnAtom -> Annotation -> ODocS
  }

instance Show AnnoEval where show _ = "<AnnoEval>" 
  
npletDuration :: Int -> Duration -> Duration
npletDuration len unit_d = (fromIntegral len % 1) * unit_d                                        
                                           
--------------------------------------------------------------------------------
-- The External view - EventList - events with no rhythmical grouping
newtype System = System { getSystem :: Map.Map String (EventList,AnnoEval) }
  deriving Show


-- (Call it a _List_ even though it is really a Seq)
newtype EventListF evt = EventList { getEventList :: Seq evt }
    deriving (Show) 
    
type EventList = EventListF Event  


data Event = SingleE Element
           | OverlayE [EventListF Event]  
    deriving (Show)              
         


--------------------------------------------------------------------------------   
-- The internal view
    
-- A 'horizontal line' of music - of course it maybe printed on 
-- more than one line and might contain polyphony, but it will be played 
-- by a single instrument.
newtype NoteListF e   = NoteList { getNoteList :: Seq (BlockF e) }

type NoteList         = NoteListF Element


-- Follow the Abc style when voice overlays are grouped in whole bars.
data BlockF e         = SingleBlock (BarF e)
                      | OverlayBlock (Seq (BarF e))

type Block            = BlockF Element

newtype BarF e        = Bar { getBar :: Seq (MetricalWordF e) }

type Bar              = BarF Element

data MetricalWordF a  = Singleton a 
                      | BeamGroup (Seq a)
  deriving (Show)

type MetricalWord     = MetricalWordF Element
  

--------------------------------------------------------------------------------
-- Functor instances


instance Functor NoteListF where
  fmap f (NoteList se)            = NoteList (fmap (fmap f) se)
  
instance Functor BlockF where
  fmap f (SingleBlock e)          = SingleBlock (fmap f e)
  fmap f (OverlayBlock se)        = OverlayBlock (fmap (fmap f) se)
  
instance Functor BarF where
  fmap f (Bar se)                 = Bar (fmap (fmap f) se)


instance Functor MetricalWordF where
  fmap f (Singleton e)            = Singleton (f e)
  fmap f (BeamGroup se)           = BeamGroup (fmap f se)

--------------------------------------------------------------------------------
-- Foldable instances
  
instance F.Foldable NoteListF where
  foldMap f (NoteList se)         = F.foldMap (F.foldMap f) se
  
instance F.Foldable BlockF where
  foldMap f (SingleBlock e)       = F.foldMap f e
  foldMap f (OverlayBlock se)     = F.foldMap (F.foldMap f) se
  
instance F.Foldable BarF where
  foldMap f (Bar se)              = F.foldMap (F.foldMap f)  se


instance F.Foldable MetricalWordF where
  foldMap f (Singleton e)         = f e
  foldMap f (BeamGroup se)        = F.foldMap f se
    

--------------------------------------------------------------------------------
-- Traversable instances

  
instance Traversable NoteListF where
  traverse f (NoteList se)        = NoteList <$> traverse (traverse f) se

instance Traversable BlockF where
  traverse f (SingleBlock e)      = SingleBlock  <$> traverse f e
  traverse f (OverlayBlock se)    = OverlayBlock <$> traverse (traverse f) se
 
instance Traversable BarF where
  traverse f (Bar se)             = Bar <$> traverse (traverse f) se

instance Traversable MetricalWordF where
  traverse f (Singleton e)        = Singleton <$> f e
  traverse f (BeamGroup se)       = BeamGroup <$> traverse f se
  
--------------------------------------------------------------------------------
-- RhythmicValue insatnces   
  
instance RhythmicValue Element where
  rhythmicValue (Atom e)                = rhythmicValue e
  rhythmicValue (Chord _ d _)           = d
  rhythmicValue (GraceNotes _ _)        = duration_zero
  rhythmicValue (Nplet i d _ _)         = npletDuration i d
 
  
  updateDuration d (Atom e)             = Atom $ updateDuration d e
  updateDuration d (Chord se _ a)       = Chord se d a
  updateDuration _ (GraceNotes se a)    = GraceNotes se a
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
  rhythmicValue Tie                     = duration_zero
  
  updateDuration d (Note p _ a)         = Note p d a
  updateDuration d (Rest _ a)           = Rest d a
  updateDuration d (Spacer _ a)         = Spacer d a
  updateDuration d (RhythmicMark l _ m) = RhythmicMark l d m
  updateDuration _ (Mark l m)           = Mark l m
  updateDuration _ Tie                  = Tie

instance PitchValue Element where
  pitchValue (Atom e)                 = pitchValue e
  pitchValue (Chord se _ _)           = pitchValue se
  pitchValue (GraceNotes se _)        = pitchValue se
  pitchValue (Nplet _ _ se _)         = pitchValue se
 
  
  updatePitch pc (Atom e)             = Atom (updatePitch pc e)
  updatePitch pc (Chord se d a)       = Chord (updatePitch pc se) d a
  updatePitch pc (GraceNotes se a)    = GraceNotes (updatePitch pc se) a
  updatePitch pc (Nplet i ud se a)    = Nplet i ud (updatePitch pc se) a
    
instance PitchValue Atom where
  pitchValue (Note p _ _)            = pitchValue p
  pitchValue (Rest _ _)              = noPitchContent
  pitchValue (Spacer _ _)            = noPitchContent
  pitchValue (RhythmicMark _ _ _)    = noPitchContent
  pitchValue (Mark _ _)              = noPitchContent
  pitchValue Tie                     = noPitchContent
  
  updatePitch pc (Note p d a)         = Note (updatePitch pc p) d a
  updatePitch _  (Rest d a)           = Rest d a
  updatePitch _  (Spacer d a)         = Spacer d a
  updatePitch _  (RhythmicMark l d m) = RhythmicMark l d m
  updatePitch _  (Mark l m)           = Mark l m
  updatePitch _  Tie                  = Tie

instance PitchValue (Seq GraceNote) where
  pitchValue = map f . F.toList where f (a,_,_) = a
  
  updatePitch pc se 
      | S.length se == length pc  = slzipsWith (\(_,d,a) p -> (p,d,a)) se pc
      | otherwise                 = error "modifyPitch (Seq GraceNote) unmatched"
  
instance PitchValue (Seq (Pitch,Annotation)) where
  pitchValue = map fst . F.toList
  
  updatePitch pc se 
      | S.length se == length pc  = slzipsWith (\(_,a) p -> (p,a)) se pc
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
  hyphenate Tie                     = Nothing
  
instance Fits Element Duration where
  measure  e  = rhythmicValue e
  
  split l (Atom e)            = (Atom a, Atom $ noAnno b) where
                                    (a,b) = split l e 
      
  split l (Chord se d a)      = (Chord se da a, Chord se db mempty) where
                                    (da,db) = split l d 
   
  -- grace notes shouldn't be split, if they are then the calling 
  -- function is wrong...  
  -- It might of course be better to represent gracenotes along
  -- with the note they grace, Bala does this.   
  split _ (GraceNotes _ _) = error $ "can't split gracenotes"
      
      
  split l (Nplet i ud se a)   = (Nplet i ud sa a, Nplet i ud sb mempty) where
      (da,db)   = npletDuration (S.length se) l `split` ud    
      sza       :: Double
      sza       = fromRat $ (da/db) * (fromIntegral (S.length se) % 1)
      (sa,sb)   = S.splitAt (floor  sza) se


  hyphenate (Chord _ _ _)     = Just (Atom Tie)
  hyphenate _                 = Nothing
       
--------------------------------------------------------------------------------
-- Annotations and marks
  
lyOutput :: Mark a -> ODoc
lyOutput mark = _ly_output mark

abcOutput :: Mark a -> ODoc
abcOutput mark = _abc_output mark


-- need a class like PitchValue for Annotation....

class NoAnno a where noAnno :: a -> a

instance NoAnno GraceNote where
  noAnno (p,d,_) = (p,d,[])

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


noAnnoEval :: AnnoEval
noAnnoEval = AnnoEval (\_ _ -> id) (\_ _ -> id) (\_ _ -> id) 

system :: System 
system = System $ mempty

systemL :: [(String, EventList)] -> System
systemL = systemL' . map (\(name,evs) -> (name,evs,noAnnoEval))

systemL' :: [(String, EventList, AnnoEval)] -> System
systemL' = System . foldr ins mempty where
  ins (name,evs,aeval) mp = Map.insert name (evs,aeval) mp

system1 :: String -> EventList -> System
system1 name evs = System $ Map.insert name (evs,noAnnoEval) mempty

system1' :: String -> EventList -> AnnoEval -> System
system1' name evs aeval = System $ Map.insert name (evs,aeval) mempty

root :: EventList
root = EventList empty

poly              :: [EventList] -> EventList -> EventList
poly xs t         = t |*> OverlayE xs

event             :: Element -> EventList -> EventList
event e t         = t |*> SingleE e 


noteSgl           :: Pitch -> Duration -> Element 
noteSgl p d       = Atom $ Note p d mempty

noteSgl'          :: Pitch -> Duration -> Annotation -> Element 
noteSgl' p d a    = Atom $ Note p d a 

note              :: Pitch -> Duration -> EventList -> EventList
note  p d         = event (noteSgl p d)

note'             :: Pitch -> Duration -> AnnotationS -> EventList -> EventList
note' p d f       = event $ noteSgl' p d (f [])

restSgl           :: Duration -> Element 
restSgl d         = Atom $ Rest d mempty 

restSgl'          :: Duration -> Annotation -> Element 
restSgl' d a      = Atom $ Rest d a

rest              :: Duration -> EventList -> EventList
rest d            = event (restSgl d)

rest'             :: Duration -> Annotation -> EventList -> EventList
rest' d a         = event (restSgl' d a)

spacerSgl         :: Duration -> Element 
spacerSgl d       = Atom $ Spacer d mempty 

spacerSgl'        :: Duration -> Annotation -> Element 
spacerSgl' d a    = Atom $ Spacer d a

spacer            :: Duration -> EventList -> EventList
spacer d          = event (spacerSgl d)

spacer'           :: Duration -> Annotation -> EventList -> EventList
spacer' d a       = event (spacerSgl' d a)

chordGrp          :: Seq Pitch -> Duration -> Element
chordGrp se d     = Chord (fmap f se) d mempty where
    f a  = (a,mempty)

chordGrp'         :: Seq (Pitch,Annotation) -> Duration -> Annotation -> Element
chordGrp' se d a  = Chord se d a

chordGrpL         :: [Pitch] -> Duration -> Element
chordGrpL xs d    = Chord (fromList $ fmap f xs) d mempty where
    f a = (a,mempty)

chordGrpL'         :: [(Pitch,Annotation)] -> Duration ->  Annotation -> Element
chordGrpL' xs d a  = Chord (fromList xs) d a


chord             :: Seq Pitch -> Duration -> EventList -> EventList
chord se d        = event (chordGrp se d)

chord'            :: Seq (Pitch,Annotation) -> Duration -> Annotation -> EventList -> EventList
chord' se d a     = event (chordGrp' se d a)



chordL            :: [Pitch] -> Duration -> EventList -> EventList
chordL xs d       = event (chordGrpL xs d)

chordL'           :: [(Pitch,Annotation)] -> Duration -> Annotation -> EventList -> EventList
chordL' xs d a    = event (chordGrpL' xs d a)

gracesGrp         :: Seq (Pitch,Duration) -> Element
gracesGrp se      = GraceNotes (fmap f se) mempty where
    f (a,b) = (a,b,mempty)

gracesGrp'        :: Seq (Pitch,Duration,Annotation) -> Annotation -> Element
gracesGrp' se a   = GraceNotes se a

graces            :: Seq (Pitch,Duration) -> EventList -> EventList
graces se         = event (gracesGrp se)

graces'           :: Seq (Pitch,Duration,Annotation) -> Annotation -> EventList -> EventList
graces' se a      = event (gracesGrp' se a)

gracesGrpL        :: [(Pitch,Duration)] -> Element
gracesGrpL xs     = gracesGrp (fromList xs)

gracesGrpL'       :: [(Pitch,Duration,Annotation)] -> Annotation -> Element
gracesGrpL' xs a  = gracesGrp' (fromList xs) a

gracesL           :: [(Pitch,Duration)] -> EventList -> EventList
gracesL xs        = event (gracesGrpL xs)

gracesL'          :: [(Pitch,Duration,Annotation)] -> Annotation -> EventList -> EventList
gracesL' xs a     = event (gracesGrpL' xs a)


npletGrp          :: Int -> Duration -> Seq Pitch -> Element
npletGrp i ud se  = Nplet i ud (fmap f se) [] where
    f a = (a,mempty)  

npletGrp'         :: Int -> Duration -> Seq (Pitch,Annotation) -> Annotation -> Element
npletGrp' i ud se a   = Nplet i ud se a

nplet             :: Int -> Duration -> Seq Pitch -> EventList -> EventList
nplet i ud se     = event (npletGrp i ud se)

nplet' :: Int -> Duration -> Seq (Pitch,Annotation) ->  Annotation -> EventList -> EventList
nplet' i ud se a  = event (npletGrp' i ud se a)
    
tieSgl            :: Element
tieSgl            = Atom Tie

tie               :: EventList -> EventList
tie               = event tieSgl


simpleEventlist        :: [Pitch] -> Duration -> EventList
simpleEventlist ps d   = foldl (\t p -> t # note p d) root ps



emptyElement :: Element -> Bool
emptyElement (Atom _)               = False
emptyElement (Chord se _ _)         = S.null se
emptyElement (GraceNotes se _)      = S.null se
emptyElement (Nplet _ _ se _)       = S.null se
  





