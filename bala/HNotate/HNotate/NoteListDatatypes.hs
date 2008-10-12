
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

module HNotate.NoteListDatatypes (
    -- * External view 
    System, EventList, EventListF(..), Evt(..),
    system, systemL, system1,
    
    root, note, rest, spacer,    
    chord, gracenotes, poly, 
    notelist,
    
    
    -- * Internal view 
    NoteListF(..), NoteList,
    BlockF(..), Block,
    BarF(..), Bar,
    
    Glyph(..),
    
    -- aliases
    
    
    
    
    glyphDuration,
    durationf, pitchf
    

  ) where


import HNotate.CommonUtils
import HNotate.Duration
import HNotate.Pitch

import Control.Applicative hiding (empty)
import qualified Data.Foldable as F
import qualified Data.Map as Map
import Data.Monoid
import Data.Sequence
import Data.Traversable


-- There are two views of note lists the external one (EventList) and
-- the internal one (NoteList).
-- Both share a common representaion of glyphs

data Glyph  = Note Pitch Duration
            | Rest Duration
            | Spacer Duration -- non-printed rest
            | Chord (Seq Pitch) Duration
            | GraceNotes (Seq Pitch)
            | BeamStart
            | BeamEnd
            | Tie
  deriving (Eq,Show)   
  
  

-- The External view
type System = Map.Map String EventList

type EventList = EventListF Evt  

-- (Call it a _List_ even though it is really a Seq)
newtype EventListF evt = EventList { getEventList :: Seq evt }
  deriving (Show)

data Evt = Evt Glyph
         | Poly [EventListF Evt]
  deriving (Show)         
         
instance Monoid (EventListF evt) where
  mempty      = EventList mempty
  mappend a b = EventList $ (getEventList a) >< (getEventList b)
           




--------------------------------------------------------------------------------   
-- The internal view
    
-- A 'horizontal line' of music - of course it maybe printed on 
-- more than one line and might contain polyphony, but it will be played 
-- by a single instrument.
newtype NoteListF e   = NoteList { getNoteList :: Seq (BlockF e) }

type NoteList         = NoteListF Glyph


-- Follow the Abc style when voice overlays are grouped measure-wise.
-- The Int holds the measure number
data BlockF e         = SingleBlock Int (BarF e)
                      | PolyBlock   Int (Seq (BarF e))

type Block            = BlockF Glyph

newtype BarF e        = Bar { getBar :: Seq e }

type Bar              = BarF Glyph
             


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
-- Functions on glyphs 
 
 

glyphDuration :: Glyph -> Duration
glyphDuration (Note _ d)          = d
glyphDuration (Rest d)            = d
glyphDuration (Spacer d)          = d
glyphDuration (Chord _ d)         = d
glyphDuration _                   = no_duration



durationf :: (Duration -> Duration) -> Glyph -> Glyph
durationf f (Note p d)            = Note p (f d)
durationf f (Rest d)              = Rest (f d)
durationf f (Spacer d)            = Spacer (f d)
durationf f (Chord se d)          = Chord se (f d)
durationf f e                     = e 


pitchf :: (Pitch -> Pitch) -> Glyph -> Glyph
pitchf f (Note p d)               = Note (f p) d
pitchf f (Rest d)                 = Rest d
pitchf f (Spacer d)               = Spacer d
pitchf f (Chord se d)             = Chord (fmap f se) d
pitchf f (GraceNotes se)          = GraceNotes (fmap f se)
pitchf f (BeamStart)              = BeamStart
pitchf f (BeamEnd)                = BeamEnd


--------------------------------------------------------------------------------
-- Functions for the external view


infixl 7 #

x # f = f x


infixl 7 #.

g #. f = f . g


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

note            :: Pitch -> Duration -> EventList -> EventList
note p d t      = t |*> Evt (Note p d)

rest            :: Duration -> EventList -> EventList
rest d t        = t |*> Evt (Rest d)

spacer          :: Duration -> EventList -> EventList
spacer d t       = t |*> Evt (Spacer d)


chord           :: [Pitch] -> Duration -> EventList -> EventList
chord [] d t    = t
chord es d t    = t |*> (Evt $ Chord (fromList es) d)

    
gracenotes           :: [Pitch] -> EventList -> EventList
gracenotes [] t      = t
gracenotes es t      = t |*> (Evt $ GraceNotes $ fromList es)

-- poly does some optimizing ...
poly            :: [EventList] -> EventList -> EventList
poly []  t      = t
poly [x] t      = EventList $ getEventList t >< getEventList x
poly ts  t      = t |*> (Poly ts)


notelist        :: [Pitch] -> Duration -> EventList
notelist ps d   = foldl (\t e -> note e d t) root ps