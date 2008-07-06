--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Perform.RenderMidi
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Render an EventTree as MIDI.
--
--------------------------------------------------------------------------------

module Bala.Perform.RenderMidi where


import Bala.Perform.Mergesort

import Bala.Base.Base
import qualified Bala.Format.Midi.Midi as MIDI
import Bala.Format.Midi.SyntaxElements
import Bala.Perform.EventTree

import Control.Applicative hiding (empty)
import Control.Monad.State
import qualified Data.Foldable as F
import Data.Sequence hiding (reverse, length)
import Data.Word


lilypond_ticks, abc_ticks :: Integer
abc_ticks       = 480
lilypond_ticks  = 384


type ClockedEvent evt = (Integer, evt)
type ProcessM a = State ProcessSt a


data ProcessSt = ProcessSt { 
    tick_value        :: Integer,
    track_number      :: Word8,
    midi_tempo        :: Word32
  }  
  
default_st = ProcessSt lilypond_ticks 1 500000


runProcess  = evalState
  
duration :: Duration -> ProcessM Integer
duration d = gets tick_value >>= \t -> 
             return $ calculateTicks t d
             
channel ::  ProcessM Word8
channel = gets track_number   

newTrack :: ProcessM ()
newTrack = gets track_number >>= \t -> modify (\s -> s { track_number = t+1 })   
  
-- | Flatten the event tree into a sequence of events paired with global time.
oflat :: (Integer,Seq (ClockedEvent (Pitch, Integer))) -> 
         EventTree (Pitch, Duration) -> 
         ProcessM (Integer,Seq (ClockedEvent (Pitch, Integer)))  
oflat (i,ksq) EmptyTree     = return (i, ksq)

 
oflat (i,ksq) (Next t (p,d))    = do 
    (i',ksq') <- oflat (i,ksq) t
    d'        <- duration d
    return  (i'+d', ksq' |> (i',(p,d')))

 
oflat (i,ksq) (Par t (p,d))     = do 
    (i',ksq') <- oflat (i,ksq) t
    d'        <- duration d
    return (i', ksq' |> (i',(p,d'))) 


oflat (i,ksq) (Prefix (p,d) t)    = do 
    (i',ksq')   <- oflat (i,ksq) t
    d'          <- duration d
    return (i', ksq' |> (i'-d', (p,d')))
  
oflat (i,ksq) (Sequence t ts) = do
   (i',ksq') <- oflat (i,ksq) t
   xs <- mapM (oflat (i',empty)) ts
   return (merge (i',ksq') xs) 

merge :: (Integer, Seq a) -> [(Integer, Seq a)] -> (Integer, Seq a)
merge (i,sq) xs = (undefined, foldl (><) sq (map snd xs)) 



oflatPass :: EventTree (Pitch, Duration) -> ProcessM (Seq (ClockedEvent (Pitch, Integer)))  
oflatPass t = snd <$> oflat (0,empty) t



data MidiMessage = MidiMessage Integer MIDI.Event
  deriving (Eq,Show)

-- Usefully MIDI.Event's are ordered so that NoteOff is LT NoteOn
-- this is what we want in a sort, so that a repeated note gets 
-- (note-on, note-off, note-on ... ) in sequence  
instance Ord MidiMessage where
    compare (MidiMessage t e) (MidiMessage t' e') = (t,e) `compare` (t',e')
  
  
  
noteOn' :: Pitch -> ProcessM MIDI.Event
noteOn' p = let p' = fromPitch p in do
    ch <- channel 
    return $ noteOn ch p' 127

noteOff' :: Pitch -> ProcessM MIDI.Event
noteOff' p = let p' = fromPitch p in do
    ch <- channel 
    return $ noteOff ch p' 127

noteOnOff :: (ClockedEvent (Pitch, Integer)) -> ProcessM (MidiMessage, MidiMessage)
noteOnOff (gt,(n,d)) = do 
  no    <- noteOn' n
  noff  <- noteOff' n
  return $ (MidiMessage gt no, MidiMessage (gt+d) noff)

  
splitNoteStep  :: (Seq MidiMessage)
               -> ClockedEvent (Pitch, Integer) 
               -> ProcessM (Seq MidiMessage)
splitNoteStep sq e@(gt,(p,d)) = do 
    (on,off)  <- noteOnOff e 
    return $  sq |> on |> off

splitNotePass :: Seq (ClockedEvent (Pitch, Integer)) -> ProcessM (Seq MidiMessage)
splitNotePass = F.foldlM splitNoteStep empty 
    
    
    

sortPass :: Seq MidiMessage -> ProcessM (Seq MidiMessage)
sortPass sq = return (mergesort compareMessages sq)

 
compareMessages m m' = compare m m'

demo02 :: EventTree (Pitch, Duration) -> ProcessM (Seq MidiMessage)
demo02 = sortPass <=< splitNotePass <=< oflatPass


deltaStep 
    :: (Integer, Seq MIDI.Message) -> MidiMessage -> ProcessM (Integer, Seq MIDI.Message)
deltaStep (t,sq) (MidiMessage gt e) = return (gt, sq |> ((fromIntegral $ gt-t), e))

deltaPass ::  Seq MidiMessage -> ProcessM (Seq MIDI.Message)
deltaPass s = snd <$> F.foldlM deltaStep (0,empty) s



finalizeTrack :: Seq MIDI.Message -> ProcessM (Seq MIDI.Message)
finalizeTrack s = return $ s |> endOfTrack_zero

 
demo03 = deltaPass <=< sortPass <=< splitNotePass <=< oflatPass

toList :: Seq a -> [a]
toList = F.foldr (:) []

listPass :: Seq MIDI.Message -> ProcessM [MIDI.Message]
listPass = return . toList



processTrack :: EventTree (Pitch, Duration) -> ProcessM MIDI.Track
processTrack = (return . MIDI.Track) 
                    <=< listPass      <=< finalizeTrack 
                    <=< deltaPass     <=< sortPass  
                    <=< splitNotePass <=< oflatPass


processPerformance :: Performance (Pitch, Duration) -> ProcessM MIDI.MidiFile
processPerformance p@(Perf xs) = do 
    hdr <- midiHeader p
    t0 <- trackZero 
    ts <- foldM fn [] xs >>= \sx -> return (reverse sx)
    return $ MIDI.MidiFile hdr (t0:ts)
  where
    fn acc a = processTrack a >>= \t -> newTrack >> return (t : acc)


midiHeader :: Performance (Pitch, Duration) -> ProcessM MIDI.Header
midiHeader (Perf xs) = do 
    tpqn <- gets tick_value
    return $ format1_header (1 + length xs) (tpb tpqn) 

setTempoMessage :: ProcessM MIDI.Message
setTempoMessage = gets midi_tempo >>= return . setTempo_zero
 

trackZero :: ProcessM MIDI.Track
trackZero = do 
    stm <- setTempoMessage
    return $ MIDI.Track [stm, endOfTrack_zero] 

notes :: EventTree (Pitch,a) -> String
notes = afficherL . F.foldr note []
  where note (n,_) acc = n:acc

runDemo perf = MIDI.printMidi $ runProcess (processPerformance perf) default_st

runDemo' tree = runDemo (Perf [tree])

output perf filename = 
  let mf = runProcess (processPerformance perf) default_st
  in MIDI.writeMidi filename mf
  
output' tree filename = 
  let mf = runProcess (processPerformance (Perf [tree])) default_st
  in MIDI.writeMidi filename mf
  
  

      