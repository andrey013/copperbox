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

type MSystem    = DSystem
type MStrata    = DStrata
type MChunk     = DBlock
type MMeasure   = DMeasure
type MGlyph     = DGlyph


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


generateMidi :: DSystem -> Notate_Midi_Env -> MidiFileContent
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

midiFileContent :: DSystem -> ProcessMidi MidiFileContent
midiFileContent (ScSystem se) = 
    MidiFileContent <$> F.foldlM fn mempty se
  where
    fn acc e = (acc |>) <$> midiTrackContent e

midiTrackContent :: DStrata -> ProcessMidi MidiTrackContent
midiTrackContent s = MidiTrackContent <$> sstrata s 

sstrata :: DStrata -> ProcessMidi (Seq Message)
sstrata (ScStrata i se) = 
    (finalizeTrack . deltaTransform) <$> F.foldlM sblock mempty se
   

sblock :: Seq Message -> DBlock -> ProcessMidi (Seq Message)
sblock se (ScSingleBlock i e) = blockOnset (i-1) >> smeasure se e

sblock se (ScPolyBlock i sse) = F.foldlM fn se sse
  where fn se e = blockOnset (i-1) >> smeasure se e

smeasure :: Seq Message -> DMeasure -> ProcessMidi (Seq Message)
smeasure se (ScMeasure sse)   = F.foldlM sglyph se sse

sglyph :: Seq Message -> DGlyph -> ProcessMidi (Seq Message)
sglyph se (ScGlyph e)         = cglyph se e

cglyph :: Seq Message -> CommonGlyph -> ProcessMidi (Seq Message)    
cglyph se (CmnNote p d)       = bumpDuration d $ \(gt,dur) -> do
    on    <- mkNoteOn gt p
    off   <- mkNoteOff (gt+dur) p
    return $ se |> on |> off

   
cglyph se (CmnRest d)         = bumpDuration d $ \(gt,dur) -> return se

cglyph se (CmnSpacer d)       = bumpDuration d $ \(gt,dur) -> return se
 
cglyph se (CmnChord sa d)     = bumpDuration d $ \(gt,dur) -> do
    ons   <- F.foldlM (fn (mkNoteOn gt)) mempty sa
    offs  <- F.foldlM (fn (mkNoteOff (gt+dur))) mempty sa
    return $ se >< ons >< offs
  where 
    fn f acc e = (acc |>) <$> f e                              

cglyph se (CmnGraceNotes _)   = return se


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
    
                 
