{-# LANGUAGE MultiParamTypeClasses      #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZMidi.Construction
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Save some of the tedium in creating Midi files.
--
--------------------------------------------------------------------------------


module ZMidi.Construction (
  Ticks, MidiPitch, OutputMidi,
  execConstruction,
  newTrack, nextChannel, drumTrack, clockToZero,
  
  spacer, note, chord, 
  
  programChange, sequenceName, majorKey, minorKey
  
 ) where


import ZMidi.Datatypes
import ZMidi.Mergesort

import Control.Monad.Identity
import Control.Monad.State

import Data.Int
import Data.Sequence
import Data.Word
import Prelude hiding (length)


type Ticks = Word32
type MidiPitch = Word8



data MidiOutput = MidiOutput { 
      _tracks           :: Seq Track, 
      _current_track    :: Seq Message 
    }

-- Note channel 10 is special - percussion.
-- The function drumTrack advances to channel 9, caching the last channel     
data OutputState = OutputState {
      _channel          :: Word8,
      _last_channel     :: Word8,       -- store     
      _abs_time         :: Word32
    }
    
    
newtype OutputT m a = OutputT { 
    getOutputT :: StateT OutputState (StateT MidiOutput m) a }


type OutputMidi a = OutputT Identity a


instance Monad m => Functor (OutputT m) where
    fmap f = OutputT . fmap f . getOutputT 
    
    
instance Monad m => Monad (OutputT m) where
  return a = OutputT $ return a
  ma >>= f = OutputT $ getOutputT ma >>= getOutputT . f

instance Monad m => MonadState OutputState (OutputT m) where
  get     = OutputT $ get 
  put f   = OutputT $ put f

execConstruction :: OutputT Identity a -> Word16 -> Int -> MidiFile  
execConstruction m tpb tempo = runIdentity $ execConstruction' m tpb tempo
  
  
execConstruction' :: Monad m => OutputT m a -> Word16 -> Int -> m MidiFile
execConstruction' m tpb tempo = do
    ((_,_),s') <- runStateT (runStateT (getOutputT m) st0) out0
    return $ finalize s'
  where
    out0 = MidiOutput  { _tracks=empty, _current_track=empty }
    st0  = OutputState { _channel=0, _last_channel=0,   _abs_time=0 }

    finalize :: MidiOutput -> MidiFile
    finalize (MidiOutput trks t1) = 
        let se = (control_trk <| trks) |> transformTrack t1
        in MidiFile (header se) se

    header se = Header MF1 (fromIntegral $ length se) (TPB tpb)
    
    control_trk = Track $ empty |> (0, tempo_evt) |> (0, MetaEvent EndOfTrack) 
                                  
    tempo_evt = MetaEvent $ SetTempo $ (fromIntegral tempo * 500000) `div` 60                             
                                  
--------------------------------------------------------------------------------
-- Inner state 

-- The inner state is more like a writer monad (with the convoluted 
-- newTrack operation and no monoid instance) than a state monad 
-- so it isn't an instance of MonadState


newTrack :: Monad m => OutputT m ()
newTrack = OutputT $ do
    t1  <- lift $ gets _current_track
    ts  <- lift $ gets _tracks
    lift $ modify (\s -> s { _current_track = empty, 
                             _tracks = ts |> (transformTrack t1) } )
    -- reset the state    
    modify (\s -> s { _channel=0, _abs_time =0 })                             


transformTrack :: Seq Message -> Track
transformTrack = Track . suffixEOT . deltaTransform . mergesort compare
  where
    deltaTransform :: Seq Message -> Seq Message
    deltaTransform = step 0 empty . viewl where
        step _ acc EmptyL         = acc
        step t acc ((gt,e) :< se) = step gt (acc |> (gt-t,e)) (viewl se)
        
    suffixEOT = (|> (0, MetaEvent EndOfTrack))    

        

--------------------------------------------------------------------------------
-- 





updateTime :: Monad m => Ticks -> OutputT m ()     
updateTime t = do 
    at <- gets _abs_time
    modify (\s -> s {_abs_time = at+t} )


-- a(ppend) a(t) t(his) i(nstant)
aati :: Monad m => (DeltaTime -> Word8 -> Message) -> OutputT m ()
aati f = do
    at <- gets _abs_time
    ch <- gets _channel
    appendEvent (f at ch)  
  where          
    appendEvent :: Monad m => Message -> OutputT m ()
    appendEvent e = OutputT $ do
        trk <- lift $ gets _current_track
        lift $ modify (\s -> s { _current_track = trk |> e} )                
               
-- p(refix) a(t) z(ero)
paz :: Monad m => Event -> OutputT m ()
paz e = let msg = (0,e) in OutputT $ do
    trk <- lift $ gets _current_track
    lift $ modify (\s -> s { _current_track = msg <| trk } ) 
     
noteon            :: Word8 -> Word8 -> Word8 -> Event
noteon ch p v     = VoiceEvent $ NoteOn ch p v

noteoff           :: Word8 -> Word8 -> Word8 -> Event
noteoff ch p v    = VoiceEvent $ NoteOff ch p v


--------------------------------------------------------------------------------
-- user level functions

-- Note changing channel resets the absolute time to 0 
nextChannel :: Monad m => OutputT m ()
nextChannel = do 
    ch    <- gets _channel
    lch   <- gets _last_channel
    modify (\s -> s { _channel      = (nextChan ch lch) , 
                      _last_channel = ch,
                      _abs_time     = 0 })
  where
    nextChan i last_chan | i == 8     = 10 -- skip channel 10 (percussion)
                         | i == 9     = last_chan + 1
                         | otherwise  = i + 1    

drumTrack :: Monad m => OutputT m ()
drumTrack = do 
    ch   <- gets _channel
    aati (\at _ -> (at, VoiceEvent $ ProgramChange 9 0))
    modify (\s -> s { _channel        = 9, 
                      _last_channel   = ch,
                      _abs_time       = 0 }) 

-- Resets the clock in the current track 
-- This allows us to do multiple passes writing to the same channel,
-- which while dangerous (possible conflicts with note on note off)
-- does make writing percussion tracks easy as we know there is no 
-- contention.
clockToZero :: Monad m => OutputT m ()
clockToZero = modify (\s -> s {_abs_time = 0})
    
spacer :: Ticks -> OutputMidi () 
spacer t = updateTime t       

note :: MidiPitch -> Ticks -> OutputMidi () 
note p t = do
    aati (\at ch -> (at, noteon ch p 127))
    updateTime t 
    aati (\at ch -> (at, noteoff ch p 64))

chord :: [MidiPitch] -> Ticks -> OutputMidi () 
chord ps t = do
  mapM_ (\p -> aati (\at ch -> (at, noteon ch p 127))) ps
  updateTime t     
  mapM_ (\p -> aati (\at ch -> (at, noteoff ch p 64))) ps
  
programChange :: Word8 -> OutputMidi ()
programChange i = aati (\at ch -> (at, VoiceEvent $ ProgramChange ch i))


sequenceName :: String -> OutputMidi ()
sequenceName s = paz (MetaEvent $ TextEvent SEQUENCE_NAME s)

majorKey :: Int8 -> OutputMidi ()
majorKey i = paz (MetaEvent $ KeySignature i MAJOR)

minorKey :: Int8 -> OutputMidi ()
minorKey i = paz (MetaEvent $ KeySignature i MINOR)

