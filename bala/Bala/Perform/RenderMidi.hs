{-# LANGUAGE FlexibleContexts #-}

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

import Bala.Base
import qualified Bala.Format.Midi as MIDI
import Bala.Format.Midi.SyntaxElements
import Bala.Perform.EventTree
import Bala.Perform.Mergesort
import Bala.Perform.PerformBase


import Control.Applicative hiding (empty)
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Foldable as F
import Data.Sequence hiding (reverse, length)
import Data.Word


type ClockedEvent evt = (Integer, evt)
type ProcessM a = PerformM Perform_Midi_State Perform_Midi_Env a


data Perform_Midi_State = Perform_Midi_State { 
    track_number      :: Word8,
    midi_tempo        :: Word32
  }  
  deriving (Show)
  
data Perform_Midi_Env = Perform_Midi_Env {
    tick_value    :: Integer
  }
  deriving (Show)




lilypond_ticks, abc_ticks :: Integer
abc_ticks       = 480
lilypond_ticks  = 384


intial_midi_state :: Perform_Midi_State  
intial_midi_state = Perform_Midi_State { track_number = 1,
                                         midi_tempo   = 500000 }

default_midi_env :: Perform_Midi_Env
default_midi_env = Perform_Midi_Env { tick_value = lilypond_ticks }




-- The initial flattening pass generates one of: 
-- a. Pitch with duration - later split to NoteOn and NoteOff events
-- b. Midi messages themselves
-- c. NoEvent - in MIDI a NoEvent might still have a duration (if its a rest)

data EventZero = ZeroPitch Pitch Integer 
               | ZeroMidi MIDI.Event Integer
               | ZeroNoEvent Integer

eventZero :: Perform evt Pitch Duration => evt -> ProcessM EventZero
eventZero e = case fst $ eventvalues e of
    Nothing -> ZeroNoEvent   <$> calcDuration_p e
    Just p  -> (ZeroPitch p) <$> calcDuration_p e

calcDuration_p :: Perform evt Pitch Duration => evt -> ProcessM Integer
calcDuration_p e = case snd $ eventvalues e of 
    Nothing -> return 0
    Just d  -> (flip calculateTicks) d <$> asks tick_value

ezDuration (ZeroPitch _ i)  = i 
ezDuration (ZeroMidi _ i)   = i
ezDuration (ZeroNoEvent i)  = i
                




---     
{-  
class Renderable a where 
    duration           :: a -> Duration
    generates          :: a -> Maybe (a -> ProcessM (Either Pitch MIDI.Event))
  
calcDuration :: Renderable a => a -> ProcessM Integer
calcDuration e = let d = duration e in 
                 gets tick_value >>= \t -> 
                 return $ calculateTicks t d


-}


    
             
channel ::  ProcessM Word8
channel = gets track_number   

newTrack :: ProcessM ()
newTrack = gets track_number >>= \t -> modify (\s -> s { track_number = t+1 })   

{-
generatesEvent :: Renderable evt => evt -> ProcessM (Maybe (Either Pitch MIDI.Event))
generatesEvent evt = 
  case generates evt of 
    Just f -> f evt >>= (return . Just) 
    Nothing -> return Nothing
-}


  
-- | Flatten the event tree into a sequence of events paired with global time.
{-
oflat :: Renderable evt =>
         (Integer,Seq (ClockedEvent ((Either Pitch MIDI.Event), Integer))) -> 
         EventTree evt -> 
         ProcessM (Integer,Seq (ClockedEvent ((Either Pitch MIDI.Event), Integer)))  
-}
 

oflat (i,ksq) EmptyL              = return (i, ksq)
{-
oflat (i,ksq) (Evt evt :< sq)     = do 
    d           <- calcDuration evt
    oe          <- generatesEvent evt
    case oe of 
      Nothing -> oflat (i+d, ksq) (viewl sq)
      Just e  -> oflat (i+d, ksq |> (i,(e,d))) (viewl sq)
-}


oflat (i,ksq) (Evt evt :< sq)     = do
    ez <- eventZero evt
    oflat (i + ezDuration ez, ksq |> (i,ez)) (viewl sq)

oflat (i,ksq) (StartPar :< sq)    =
    oflatPar (i,0,ksq) (viewl sq)
   
oflat (i,ksq) (StartPre :< sq)    =
    oflatPre (i,ksq) (viewl sq)
    
oflat (i,ksq) (Poly ts :< sq)     = do
    xs          <- mapM (oflat (i,empty)) (map (viewl . unET) ts)   
    oflat (merge (i,ksq) xs) (viewl sq)
    
oflat (i,ksq) x                   =
    error $ "Invalid EventTree - " ++ fn x
  where
    fn (EndPar :< _)              = "out of place EndPar" 
    fn (EndPre :< _)              = "out of place EndPre" 
    fn _                          = "unexpected element"
    
-- the last oflatPar should increase i, but it doesn't
oflatPar (i,_,ksq) (Evt evt :< sq)    = do
    ez <- eventZero evt
    oflatPar (i, ezDuration ez,ksq |> (i,ez)) (viewl sq)

  
oflatPar (i,d,ksq) (EndPar :< sq)     = 
    oflat (i+d,ksq) (viewl sq)
      
oflatPar (i,d,ksq) _                  = 
    error "unterminated Par"
    
    
oflatPre (i,ksq) (Evt evt :< sq)    = do
    -- to do - currently do nothing
    oflatPre (i,ksq) (viewl sq)
  
oflatPre (i,ksq) (EndPre :< sq)     = 
    oflat (i,ksq) (viewl sq)
    
oflatPre (i,ksq) _                  = 
    error "unterminated Pre"  

merge :: (Integer, Seq a) -> [(Integer, Seq a)] -> (Integer, Seq a)
merge (i,sq) xs = (undefined, foldl (><) sq (map snd xs)) 


{-
oflatPass :: Renderable evt 
          => EventTree evt 
          -> ProcessM (Seq (ClockedEvent ((Either Pitch MIDI.Event), Integer)))  
-}
oflatPass sq = snd <$> oflat (0,empty) (viewl sq)



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

noteOnOff :: Integer -> Pitch -> Integer -> ProcessM (MidiMessage, MidiMessage)
noteOnOff gt p d = do 
  no    <- noteOn' p
  noff  <- noteOff' p
  return $ (MidiMessage gt no, MidiMessage (gt+d) noff)

{-  
splitNoteStep  :: (Seq MidiMessage)
               -> ClockedEvent ((Either Pitch MIDI.Event), Integer) 
               -> ProcessM (Seq MidiMessage)
-}
splitNoteStep sq e@(gt,(ZeroPitch p d))   = do 
    (on,off)  <- noteOnOff gt p d 
    return $ sq |> on |> off

splitNoteStep sq e@(gt,(ZeroMidi msg d))  = do 
    return $ sq |> (MidiMessage gt msg)

splitNoteStep sq _                        = return $ sq 
        
    
-- splitNotePass :: Seq (ClockedEvent ((Either Pitch MIDI.Event), Integer)) -> ProcessM (Seq MidiMessage)
splitNotePass = F.foldlM splitNoteStep empty 
    
    
    

sortPass :: Seq MidiMessage -> ProcessM (Seq MidiMessage)
sortPass sq = return (mergesort compareMessages sq)

 
compareMessages m m' = compare m m'

{-
demo02 :: EventTree (Pitch, Duration) -> ProcessM (Seq MidiMessage)
demo02 = sortPass <=< splitNotePass <=< oflatPass
-}

deltaStep 
    :: (Integer, Seq MIDI.Message) -> MidiMessage -> ProcessM (Integer, Seq MIDI.Message)
deltaStep (t,sq) (MidiMessage gt e) = return (gt, sq |> ((fromIntegral $ gt-t), e))

deltaPass ::  Seq MidiMessage -> ProcessM (Seq MIDI.Message)
deltaPass s = snd <$> F.foldlM deltaStep (0,empty) s



finalizeTrack :: Seq MIDI.Message -> ProcessM (Seq MIDI.Message)
finalizeTrack s = return $ s |> endOfTrack_zero

 
toList :: Seq a -> [a]
toList = F.foldr (:) []

listPass :: Seq MIDI.Message -> ProcessM [MIDI.Message]
listPass = return . toList



processTrack :: Perform evt Pitch Duration 
             => EventTree evt 
             -> ProcessM MIDI.Track
processTrack tree = MIDI.Track <$> steps (unET tree)
  where 
    steps = listPass <=< finalizeTrack 
                     <=< deltaPass     <=< sortPass  
                     <=< splitNotePass <=< oflatPass


processPerformance :: Perform evt Pitch Duration 
                   => Performance evt 
                   -> ProcessM MIDI.MidiFile
processPerformance p@(Perf xs) = do 
    hdr <- midiHeader p
    t0 <- trackZero 
    ts <- foldM fn [] xs >>= \sx -> return (reverse sx)
    return $ MIDI.MidiFile hdr (t0:ts)
  where
    fn acc a = processTrack a >>= \t -> newTrack >> return (t : acc)


midiHeader :: Perform evt Pitch Duration 
           => Performance evt 
           -> ProcessM MIDI.Header
midiHeader (Perf xs) = do 
    tpqn <- asks tick_value
    return $ format1_header (1 + length xs) (tpb tpqn) 

setTempoMessage :: ProcessM MIDI.Message
setTempoMessage = gets midi_tempo >>= return . setTempo_zero
 

trackZero :: ProcessM MIDI.Track
trackZero = do 
    stm <- setTempoMessage
    return $ MIDI.Track [stm, endOfTrack_zero] 

notes :: EventTree (Pitch,a) -> String
notes = afficherL . F.foldr note [] . unET
  where note (Evt (n,_)) acc = n:acc
        note _           acc = acc

renderMidi1 :: (Perform evt Pitch Duration) 
            => EventTree evt -> Perform_Midi_Env -> MIDI.MidiFile
renderMidi1 tree env = 
    renderMidi (Perf [tree]) env


renderMidi :: (Perform evt Pitch Duration) 
           => Performance evt -> Perform_Midi_Env -> MIDI.MidiFile
renderMidi perf env = 
    evalPerform (processPerformance perf) intial_midi_state env





      