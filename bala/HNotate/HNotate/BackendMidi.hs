{-# LANGUAGE MultiParamTypeClasses #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  HNotate.BackendMidi
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  mptc.
--
-- Emit Midi from Score.
--
--------------------------------------------------------------------------------

module HNotate.BackendMidi (
    Notate_Midi_Env(..), default_midi_env,
    generateMidi
  ) where

import HNotate.CommonUtils (sepSeq)
import HNotate.Duration
import HNotate.NotateMonad
import HNotate.OnsetQueue
import HNotate.OutputUtils
import HNotate.Pitch
import HNotate.ScoreRepresentation

import ZMidi

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Foldable as F
import Data.List (sort)
import Data.Monoid
import Data.Sequence
import Data.Word
import Prelude hiding (length)
import qualified Text.PrettyPrint.Leijen as PP

newtype MidiFileContent = MidiFileContent { 
    getMidiFileContent :: Seq MidiTrackContent 
  }
  
newtype MidiTrackContent = MidiTrackContent {
    getMidiTrackContent ::  Seq Message
  }

instance PP.Pretty MidiFileContent where
  pretty (MidiFileContent se) = sepSeq (PP.<$>) se

instance PP.Pretty MidiTrackContent where  
  pretty (MidiTrackContent se) = sepSeq (PP.<$>) se

type MSystem    = ScoreGlyph 
type MNoteList  = ScoreNoteList
type MChunk     = ScoreBlock
type MMeasure   = ScoreMeasure



type ProcessMidi a = NotateM Notate_Midi_State Notate_Midi_Env a



data Notate_Midi_State = Notate_Midi_State {
    global_time         :: Word32,
    channel             :: Word8,
    note_on_velocity    :: Word8,
    note_off_velocity   :: Word8
  }


data Notate_Midi_Env = Notate_Midi_Env {
    measure_length  :: Int
  }
  deriving (Show)

lilypond_ticks, abc_ticks, hnotate_ticks :: Int
abc_ticks       = 480
lilypond_ticks  = 384
hnotate_ticks   = lilypond_ticks

default_midi_env :: Notate_Midi_Env
default_midi_env = Notate_Midi_Env {
    measure_length  = 4 * hnotate_ticks  
  }

state0 :: Notate_Midi_State
state0 = Notate_Midi_State {
    global_time         = 0,
    channel             = 1,
    note_on_velocity    = 127,
    note_off_velocity   = 127
  }


generateMidi :: ScoreSystem -> Notate_Midi_Env -> MidiFileContent
generateMidi sys env = evalNotate (midiFileContent sys) state0 env


hnticks :: Duration -> Word32
hnticks = fromIntegral . midiTicks hnotate_ticks



finalizeTrack :: Seq Message -> Seq Message
finalizeTrack s = s |> end_of_track 0

bumpDuration :: Duration -> ((Word32,Word32) -> ProcessMidi a) -> ProcessMidi a
bumpDuration d f = let d' = hnticks d in do
    gt    <- gets global_time
    modify (\s -> s{ global_time = gt+d' })
    f (gt,d')

blockOnset :: Int -> ProcessMidi ()
blockOnset i = do
    ml <- asks measure_length
    modify (\s -> s{ global_time = fromIntegral $ ml * i })

midiFileContent :: ScoreSystem -> ProcessMidi MidiFileContent
midiFileContent (ScSystem se) = 
    MidiFileContent <$> F.foldlM fn mempty se
  where
    fn acc e = (acc |>) <$> midiTrackContent e

midiTrackContent :: ScoreNoteList -> ProcessMidi MidiTrackContent
midiTrackContent s = MidiTrackContent <$> snotelist s 

snotelist :: ScoreNoteList -> ProcessMidi (Seq Message)
snotelist (ScNoteList se) = 
    (finalizeTrack . deltaTransform) <$> F.foldlM sblock mempty se
   

sblock :: Seq Message -> ScoreBlock -> ProcessMidi (Seq Message)
sblock se (ScSingleBlock i e) = blockOnset (i-1) >> smeasure se e

sblock se (ScPolyBlock i sse) = F.foldlM fn se sse
  where fn se e = blockOnset (i-1) >> smeasure se e

smeasure :: Seq Message -> ScoreMeasure -> ProcessMidi (Seq Message)
smeasure se (ScMeasure sse)   = F.foldlM cglyph se sse


cglyph :: Seq Message -> ScoreGlyph -> ProcessMidi (Seq Message)    
cglyph se (GlyNote p d)       = bumpDuration d $ \(gt,dur) -> do
    on    <- mkNoteOn gt p
    off   <- mkNoteOff (gt+dur) p
    return $ se |> on |> off

   
cglyph se (GlyRest d)         = bumpDuration d $ \(gt,dur) -> return se

cglyph se (GlySpacer d)       = bumpDuration d $ \(gt,dur) -> return se
 
cglyph se (GlyChord sa d)     = bumpDuration d $ \(gt,dur) -> do
    ons   <- F.foldlM (fn (mkNoteOn gt)) mempty sa
    offs  <- F.foldlM (fn (mkNoteOff (gt+dur))) mempty sa
    return $ se >< ons >< offs
  where 
    fn f acc e = (acc |>) <$> f e                              

cglyph se (GlyGraceNotes _)   = return se


mkNoteOn :: Word32 -> Pitch ->  ProcessMidi Message
mkNoteOn onset pch = (noteon onset) <$> 
    gets channel <*> pure (midiPitch pch) <*> gets note_on_velocity

mkNoteOff :: Word32 -> Pitch -> ProcessMidi Message
mkNoteOff onset pch = (noteoff onset) <$> 
    gets channel <*> pure (midiPitch pch) <*> gets note_off_velocity
             
instance OnsetEvent Message Message where
  onset m@(Message (dt,_)) = (fromIntegral dt, m)


deltaTransform :: Seq Message -> Seq Message
deltaTransform = collapse . buildQueue
  where
    collapse :: OnsetQueue Message -> Seq Message
    collapse = step (0,mempty) . viewH

    step :: (Word32,Seq Message) -> ViewH Message -> Seq Message
    step (t,se) ((i,ms) :>> queue)   =
        step (fromIntegral i, se >< simultaneous t (sort ms)) (viewH queue)

    step (gt,se) EmptyQ                       = se

    -- List must be sorted to have NoteOffs before NoteOns
    simultaneous gt ((Message (ot,e)) : xs) =
        (mempty |> Message (ot - gt,e)) >< (fromList $ map zeromsg xs)
      where
        zeromsg (Message (_,e)) = Message (0,e)

    simultaneous gt []                      = mempty
    
                 
