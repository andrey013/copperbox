
--------------------------------------------------------------------------------
-- |
-- Module      :  HNotate.NoteList
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

module HNotate.NoteList (
    -- * External view 
    System, EventList, EventSeq(..), Evt(..),
    system, systemL, system1,
    
    root, note, rest, spacer,    
    chord, gracenotes, poly, 
    notelist,
    
    
    -- * Internal view 
    ScNoteList(..),
    ScBlock(..),
    ScMeasure(..),
    
    ScoreGlyph(..),
    
    -- aliases
    ScoreNoteList,
    ScoreBlock,
    ScoreMeasure,
    
    glyphDuration,
    onDuration, onPitch
    

  ) where


import HNotate.CommonUtils (sepSeq)
import HNotate.Duration
import HNotate.Pitch

import qualified Control.Applicative as A
import qualified Data.Foldable as F
import qualified Data.Map as Map
import Data.Monoid
import Data.Sequence
import Data.Traversable


-- There are two views of note lists the external one (EventList) and
-- the internal one (ScNoteList).
-- Both share a common representaion of glyphs

data ScoreGlyph = SgNote Pitch Duration
                | SgRest Duration
                | SgSpacer Duration -- non-printed rest
                | SgChord (Seq Pitch) Duration
                | SgGraceNotes (Seq Pitch)
                | SgBeamStart
                | SgBeamEnd
  deriving Show   
  
  

-- The External view
type System = Map.Map String EventList

type EventList = EventSeq Evt  

newtype EventSeq evt = EventSeq { getEventSeq :: Seq evt }
  deriving Show

data Evt = Evt ScoreGlyph
         | Poly [EventSeq Evt]
         
instance Monoid (EventSeq evt) where
  mempty = EventSeq mempty
  mappend a b = EventSeq $ (getEventSeq a) >< (getEventSeq b)
           

infixl 7 #

x # f = f x


infixl 7 #.

g #. f = f . g


(|*>) :: EventList -> Evt -> EventList
(|*>) (EventSeq t) evt = EventSeq $ t |> evt



system :: System 
system = mempty

systemL :: [(String, EventList)] -> System
systemL = Map.fromList

system1 :: String -> EventList -> System
system1 k t = Map.insert k t mempty

root :: EventList
root = EventSeq empty

note            :: Pitch -> Duration -> EventList -> EventList
note p d t      = t |*> Evt (SgNote p d)

rest            :: Duration -> EventList -> EventList
rest d t        = t |*> Evt (SgRest d)

spacer          :: Duration -> EventList -> EventList
spacer d t       = t |*> Evt (SgSpacer d)


chord           :: [Pitch] -> Duration -> EventList -> EventList
chord [] d t    = t
chord es d t    = t |*> (Evt $ SgChord (fromList es) d)

    
gracenotes           :: [Pitch] -> EventList -> EventList
gracenotes [] t      = t
gracenotes es t      = t |*> (Evt $ SgGraceNotes $ fromList es)

-- poly does some optimizing ...
poly            :: [EventList] -> EventList -> EventList
poly []  t      = t
poly [x] t      = EventSeq $ getEventSeq t >< getEventSeq x
poly ts  t      = t |*> (Poly ts)


notelist        :: [Pitch] -> Duration -> EventList
notelist ps d   = foldl (\t e -> note e d t) root ps


--------------------------------------------------------------------------------   
-- The internal view
    
-- A 'horizontal line' of music - of course it maybe printed on 
-- more than one line and might contain polyphony, but it will be played 
-- by a single instrument.
newtype ScNoteList e = ScNoteList { geSctNoteList :: Seq (ScBlock e) }




-- Follow the Abc style when voice overlays are grouped measure-wise.
-- The Int holds the measure number
data ScBlock e = ScSingleBlock Int (ScMeasure e)
               | ScPolyBlock Int (Seq (ScMeasure e))


newtype ScMeasure e = ScMeasure { getMeasure :: Seq e }


             


type ScoreNoteList  = ScNoteList ScoreGlyph
type ScoreBlock     = ScBlock ScoreGlyph
type ScoreMeasure   = ScMeasure ScoreGlyph

                 

--------------------------------------------------------------------------------
-- Functor instances
  
instance Functor ScNoteList where
  fmap f (ScNoteList se)      = ScNoteList (fmap (fmap f) se)
  
instance Functor ScBlock where
  fmap f (ScSingleBlock i e)  = ScSingleBlock i (fmap f e)
  fmap f (ScPolyBlock i se)   = ScPolyBlock i (fmap (fmap f) se)
  
instance Functor ScMeasure where
  fmap f (ScMeasure se)       = ScMeasure (fmap f se)



--------------------------------------------------------------------------------
-- Foldable instances
  
instance F.Foldable ScNoteList where
  foldMap f (ScNoteList se)       = F.foldMap (F.foldMap f) se
  
instance F.Foldable ScBlock where
  foldMap f (ScSingleBlock i e)   = F.foldMap f e
  foldMap f (ScPolyBlock i se)    = F.foldMap (F.foldMap f) se
  
instance F.Foldable ScMeasure where
  foldMap f (ScMeasure se)        = F.foldMap f se
  

--------------------------------------------------------------------------------
-- Traversable instances

  
instance Traversable ScNoteList where
  traverse f (ScNoteList se)      = ScNoteList A.<$> traverse (traverse f) se

instance Traversable ScBlock where
  traverse f (ScSingleBlock i e)  = (ScSingleBlock i) A.<$> traverse f e
  traverse f (ScPolyBlock i se)   = 
      (ScPolyBlock i) A.<$> traverse (traverse f) se
 
instance Traversable ScMeasure where
  traverse f (ScMeasure se)       = ScMeasure A.<$> traverse f se
 

--------------------------------------------------------------------------------
-- Bifunctor instance for Glyphs
 

glyphDuration :: ScoreGlyph -> Duration
glyphDuration (SgNote _ d)        = d
glyphDuration (SgRest d)          = d
glyphDuration (SgSpacer d)        = d
glyphDuration (SgChord _ d)       = d
glyphDuration (SgGraceNotes _)    = durationZero
glyphDuration (SgBeamStart)       = durationZero
glyphDuration (SgBeamEnd)         = durationZero


onDuration :: (Duration -> Duration) -> ScoreGlyph -> ScoreGlyph
onDuration f (SgNote p d)         = SgNote p (f d)
onDuration f (SgRest d)           = SgRest (f d)
onDuration f (SgSpacer d)         = SgSpacer (f d)
onDuration f (SgChord se d)       = SgChord se (f d)
onDuration f (SgGraceNotes se)    = SgGraceNotes se
onDuration f (SgBeamStart)        = SgBeamStart
onDuration f (SgBeamEnd)          = SgBeamEnd

onPitch :: (Pitch -> Pitch) -> ScoreGlyph -> ScoreGlyph
onPitch f (SgNote p d)            = SgNote (f p) d
onPitch f (SgRest d)              = SgRest d
onPitch f (SgSpacer d)            = SgSpacer d
onPitch f (SgChord se d)          = SgChord (fmap f se) d
onPitch f (SgGraceNotes se)       = SgGraceNotes (fmap f se)
onPitch f (SgBeamStart)           = SgBeamStart
onPitch f (SgBeamEnd)             = SgBeamEnd


