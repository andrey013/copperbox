{-# LANGUAGE MultiParamTypeClasses #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Perform.Midi.MidiBackend
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Emit Midi from Score.
--
--------------------------------------------------------------------------------

module Bala.Perform.Midi.MidiBackend where

import Bala.Format.Midi
import Bala.Format.Midi.SyntaxElements
import Bala.Perform.Base.OnsetQueue
import Bala.Perform.Base.PerformMonad
import Bala.Perform.Midi.Class
import Bala.Perform.Midi.MidiScoreDatatypes

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Foldable as F
import Data.List (sort)
import Data.Monoid
import Data.Sequence
import Data.Word
import Prelude hiding (length)


type ProcessM a = PerformM Perform_Midi_State Perform_Midi_Env a



data Perform_Midi_State = Perform_Midi_State { 
    global_time         :: Integer,
    channel             :: Word8,
    note_on_velocity    :: Word8,
    note_off_velocity   :: Word8
  } 
  
    
data Perform_Midi_Env = Perform_Midi_Env {
    tick_value      :: Integer,
    measure_length  :: Integer,
    midi_tempo      :: Word32
  }
  deriving (Show)

lilypond_ticks, abc_ticks :: Integer
abc_ticks       = 480
lilypond_ticks  = 384


default_midi_env :: Perform_Midi_Env  
default_midi_env = Perform_Midi_Env {
    tick_value      = lilypond_ticks,
    measure_length  = 4 * lilypond_ticks,
    midi_tempo      = 500000
  }
  
state0 :: Perform_Midi_State
state0 = Perform_Midi_State { 
    global_time         = 0,
    channel             = 1,
    note_on_velocity    = 127,
    note_off_velocity   = 64
  } 
  
  
midiHeader :: Int -> ProcessM Header
midiHeader nt = Header MF1 (fromIntegral nt)  <$> timeDivision
    
timeDivision :: ProcessM TimeDivision    
timeDivision = TPB . fromIntegral             <$> asks tick_value

ticks :: DurationMidi dur => dur -> ProcessM Integer
ticks d = pure $ (midiTicks d) 

setMeasureOnset :: Int -> ProcessM ()
setMeasureOnset i = asks measure_length >>= \len ->
                    modify (\s -> s{ global_time = (fromIntegral i) * len })


bumpDuration :: DurationMidi dur => dur -> ProcessM ()
bumpDuration d = do
    d'    <- ticks d
    gt    <- gets global_time
    modify (\s -> s{ global_time = gt+d' })

finalizeTrack :: Seq Message -> Seq Message
finalizeTrack s = s |> endOfTrack_zero

setTempoMessage :: ProcessM Message
setTempoMessage = setTempo_zero <$> asks midi_tempo 


track0 :: ProcessM Track
track0 = do 
    stm <- setTempoMessage
    return $ Track (mempty |> stm |> endOfTrack_zero) 
    
        
generateMidi :: (PitchMidi pch, DurationMidi dur)
             => MidiScScore pch dur
             -> Perform_Midi_Env 
             -> MidiFile
generateMidi sc env = evalPerform (renderScore sc) state0 env

    
-- | @LyScScore --> \\MidiFile@ 
renderScore :: (PitchMidi pch, DurationMidi dur) 
            => MidiScScore pch dur 
            -> ProcessM MidiFile
renderScore (MidiScScore se) = do
    t0 <- track0 
    MidiFile <$> midiHeader (1 + length se) <*> F.foldlM fn (mempty |> t0) se
  where
    fn xs p = (xs |>) <$> renderTrack p

renderTrack :: (PitchMidi pch, DurationMidi dur) 
            => MidiScTrack pch dur 
            -> ProcessM Track   
renderTrack (MidiScTrack _ se) = 
    buildTrack <$> F.foldlM renderMeasure mempty se
  where
    buildTrack = Track . finalizeTrack . deltaTransform  


-- renderMeasure must reset the global_time as MidiScore can have 
-- consecutive measures with the same measure number.
renderMeasure :: (PitchMidi pch, DurationMidi dur)  
              => Seq Message 
              -> MidiScMeasure pch dur 
              -> ProcessM (Seq Message)
renderMeasure cxt (MidiScMeasure i voice se) = do
    setMeasureOnset (i-1)           -- measures start at 1 rather than 0
    F.foldlM renderMessage cxt se


-- track onset in the state monad

renderMessage :: (PitchMidi pch, DurationMidi dur) 
              => Seq Message 
              -> MidiScGlyph pch dur -> ProcessM (Seq Message)   
renderMessage cxt (MidiScNote pch dur)  = 
    fn <$> mkNoteOn pch  <* bumpDuration dur <*> mkNoteOff pch
  where
    fn e e' = cxt |> e |> e'
    
      
renderMessage cxt (MidiScSpacer dur)    = 
    cxt <$  bumpDuration dur



renderMessage cxt (MidiScChord xs)      = do
    ons   <- F.foldlM on mempty xs  
    bumpDuration (chordDur $ head xs) -- uncool
    offs  <- F.foldlM off mempty xs
    return $ cxt >< ons >< offs 
  where
    on se (MidiScNote scp d) = (se |>) <$> mkNoteOn scp
    on se _                 = pure se
    
    off se (MidiScNote scp d) = (se |>) <$> mkNoteOff scp
    off se _                 = pure se
    
    chordDur (MidiScNote scp d) = d

-- drop grace notes for the time being
renderMessage ctx (MidiScGraceNotes xs) = return ctx
    

mkNoteOn :: (PitchMidi pch) => pch -> ProcessM Message                
mkNoteOn pch = 
    noteOn_message <$> gets global_time     <*> gets channel 
                   <*> pure (midiPitch pch) <*> gets note_on_velocity

mkNoteOff :: (PitchMidi pch) => pch -> ProcessM Message    
mkNoteOff pch = 
    noteOff_message <$> gets global_time     <*> gets channel 
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
                    
    