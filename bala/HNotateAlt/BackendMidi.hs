{-# LANGUAGE MultiParamTypeClasses #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  BackendMidi
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

module BackendMidi (
    Notate_Midi_Env(..), default_midi_env,
    generateMidi
  ) where

import CommonUtils (sepSeq)
import Duration
import NotateMonad
import OnsetQueue
import Pitch
import ScoreRepresentation

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

type MSystem    = ScSystem Glyph Duration
type MStrata    = ScStrata Glyph Duration
type MChunk     = ScBlock Glyph Duration
type MMeasure   = ScMeasure Glyph Duration
type MGlyph     = ScGlyph Glyph Duration


type ProcessM a = NotateM Notate_Midi_State Notate_Midi_Env a



data Notate_Midi_State = Notate_Midi_State {
    global_time         :: Integer,
    channel             :: Word8,
    note_on_velocity    :: Word8,
    note_off_velocity   :: Word8
  }


data Notate_Midi_Env = Notate_Midi_Env {
    tick_value      :: Integer,
    measure_length  :: Integer
  }
  deriving (Show)

lilypond_ticks, abc_ticks :: Integer
abc_ticks       = 480
lilypond_ticks  = 384


default_midi_env :: Notate_Midi_Env
default_midi_env = Notate_Midi_Env {
    tick_value      = lilypond_ticks,
    measure_length  = 4 * lilypond_ticks  
  }

state0 :: Notate_Midi_State
state0 = Notate_Midi_State {
    global_time         = 0,
    channel             = 1,
    note_on_velocity    = 127,
    note_off_velocity   = 127
  }






midiHeader :: Int -> ProcessM Header
midiHeader nt = Header MF1 (fromIntegral nt)  <$> timeDivision

timeDivision :: ProcessM TimeDivision
timeDivision = TPB . fromIntegral             <$> asks tick_value

ticks :: Duration -> ProcessM Integer
ticks d = (flip midiTicks) d <$> asks tick_value

setMeasureOnset :: Int -> ProcessM ()
setMeasureOnset i = asks measure_length >>= \len ->
                    modify (\s -> s{ global_time = (fromIntegral i) * len })


bumpDuration :: Duration -> ProcessM ()
bumpDuration d = do
    d'    <- ticks d
    gt    <- gets global_time
    modify (\s -> s{ global_time = gt+d' })

finalizeTrack :: Seq Message -> Seq Message
finalizeTrack s = s |> end_of_track 0




generateMidi :: MSystem -> Notate_Midi_Env -> MidiFileContent
generateMidi sc env = evalNotate (renderScore sc) state0 env


-- | @LyScScore --> \\MidiFile@
renderScore :: MSystem -> ProcessM MidiFileContent
renderScore (ScSystem se) =
    MidiFileContent <$> F.foldlM fn mempty se
  where
    fn xs p = (xs |>) <$> renderTrack p

renderTrack :: MStrata -> ProcessM MidiTrackContent
renderTrack (ScStrata _ se) = undefined
{-
    buildTrack <$> F.foldlM renderMeasure mempty se
  where
    buildTrack = MidiTrackContent . finalizeTrack . deltaTransform
-}

-- renderMeasure must reset the global_time as MidiScore can have
-- consecutive measures with the same measure number.
renderMeasure :: Seq Message -> MMeasure -> ProcessM (Seq Message)
renderMeasure cxt (ScMeasure se) = do
    undefined
{-
    setMeasureOnset (i-1)           -- measures start at 1 rather than 0
    F.foldlM renderMessage cxt se
-}

-- track onset in the state monad

renderMessage :: Seq Message -> MGlyph -> ProcessM (Seq Message)
renderMessage cxt (ScGlyph gly dur) = renderMsg cxt gly dur
  
renderMsg cxt (CmnNote pch) dur =
    fn <$> mkNoteOn pch  <* bumpDuration dur <*> mkNoteOff pch
  where
    fn e e' = cxt |> e |> e'

renderMsg cxt (CmnRest)     dur =
    cxt <$  bumpDuration dur
    
renderMsg cxt (CmnSpacer)   dur =
    cxt <$  bumpDuration dur

renderMsg cxt (CmnChord se) dur = do
    ons   <- F.foldlM on mempty se
    bumpDuration dur
    offs  <- F.foldlM off mempty se
    return $ cxt >< ons >< offs
  where
    on se scp = (se |>) <$> mkNoteOn scp
    off se scp = (se |>) <$> mkNoteOff scp

-- drop grace notes for the time being
renderMsg cxt (CmnGraceNotes xs) dur = return cxt


mkNoteOn :: Pitch -> ProcessM Message
mkNoteOn pch =
    noteon    <$> gets global_time     <*> gets channel
              <*> pure (midiPitch pch) <*> gets note_on_velocity

mkNoteOff :: Pitch -> ProcessM Message
mkNoteOff pch =
    noteoff  <$> gets global_time     <*> gets channel
             <*> pure (midiPitch pch) <*> gets note_off_velocity

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

