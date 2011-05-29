{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZMidi.Basic.Kernel.Base.OutputMidi
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Output MIDI.
--
-- WARNING TO SELF - are you printing sequence numbers in the info track?
--
--------------------------------------------------------------------------------

module ZMidi.Basic.Kernel.Base.OutputMidi
  (
    writeHiMidi
  ) where

import ZMidi.Basic.Kernel.Base.RenderContext ( default_volume, default_pan )
import ZMidi.Basic.Kernel.Base.Syntax
import ZMidi.Basic.Utils.JoinList ( JoinList, ViewL(..), viewl, cons )
import qualified ZMidi.Basic.Utils.JoinList as JL

import ZMidi.Core                               -- package: zmidi-core

import Control.Applicative
import Control.Monad
import Data.Bits
import qualified Data.Foldable as F
import Data.List
import qualified Data.IntMap as IM
import Data.Monoid
import Data.Time
import qualified Data.Traversable as T
import Data.Word



--------------------------------------------------------------------------------
-- Render to MIDI, write to file.

-- | Output a MIDI file. The format is always a Type 1 MIDI file
-- even if the HiMidi object only has a single track.
--
-- Note - if no text (e.g. copyright notice, or lyrics) has been
-- added to the HiMidi object, then @ZMidi-Emit@ will print a 
-- time-stamp as @generic-text@ in track 0.
--
writeHiMidi :: FilePath -> HiMidi -> IO ()
writeHiMidi filename mct = 
    getZonedTime >>= \ztim -> writeMidi filename (renderMidi ztim mct)



--------------------------------------------------------------------------------

-- Note - this is a Double so it is the right type for calculating
-- durations, in MIDI files the size of the ticks-per-beat 
-- designator is actually a Word16.
--
ticks_per_quarternote :: Double
ticks_per_quarternote = 480

durationr :: Double -> Word32
durationr r = floor $ (4 * ticks_per_quarternote) * r


--------------------------------------------------------------------------------


-- | Function from channel-number to MidiVoice event.
--
type VoiceMsg = Word8 -> MidiVoiceEvent


-- | Monad for rendering.
--
newtype MidiMonad a = MidiMonad { 
          getMidiMonad :: DeltaState -> (a,DeltaState) }




-- Note - the state tracks absolute time, conversion to delta time 
-- is performed as a traversal afterwards

data DeltaState = DeltaState
      { st_track_num            :: Word8
      , st_channel_num          :: Word8
      , st_volume               :: Word16
      , st_balance              :: Word16
      , st_ellapsed_time        :: Int
      }

initDS :: DeltaState
initDS = DeltaState { st_track_num      = 0
                    , st_channel_num    = 0
                    , st_volume         = 127
                    , st_balance        = 0x7F7F `div` 2
                    , st_ellapsed_time  = 0
                    }




instance Functor MidiMonad where
  fmap f mf = MidiMonad $ \s -> let (a,s1) = getMidiMonad mf s in (f a,s1)

instance Applicative MidiMonad where
  pure a    = MidiMonad $ \s -> (a,s)
  mf <*> ma = MidiMonad $ \s -> let (f,s1) = getMidiMonad mf s
                                    (a,s2) = getMidiMonad ma s1
                                in (f a,s2)

instance Monad MidiMonad where
  return a  = MidiMonad $ \s -> (a,s)
  m >>= k   = MidiMonad $ \s -> let (a,s1) = getMidiMonad m s
                                in (getMidiMonad . k) a s1
                              

evalMidiMonad :: MidiMonad a -> a
evalMidiMonad mf = fst $ getMidiMonad mf initDS


getsDS :: (DeltaState -> a) -> MidiMonad a
getsDS fn = MidiMonad $ \s -> (fn s,s)


setsDS :: (DeltaState -> DeltaState) -> MidiMonad ()
setsDS fn = MidiMonad $ \s -> ((),fn s)




incrTrackNumber :: MidiMonad ()
incrTrackNumber = 
    setsDS (\s -> let i = st_track_num s in s { st_track_num = i+1})

getVolume           :: MidiMonad Word16
getVolume           = getsDS st_volume

setVolume           :: Word16 -> MidiMonad ()
setVolume i         = setsDS (\s -> s { st_volume = i } )

getEllapsedTime     :: MidiMonad Int
getEllapsedTime     = getsDS st_ellapsed_time

-- In the main, rendering uses @incr@ rtaher than @set@ for 
-- ellapsed time.
--
incrEllapsedTime :: Int -> MidiMonad ()
incrEllapsedTime n = 
    setsDS (\s -> let i = st_ellapsed_time s in s { st_ellapsed_time = i+n})

-- But @set@ is still needed for overlays which all must begin at the 
-- same time.
--
setEllapsedTime  :: Int -> MidiMonad ()
setEllapsedTime t = setsDS (\s -> s { st_ellapsed_time = t })


getChannelNumber    :: MidiMonad Word8
getChannelNumber    = getsDS st_channel_num

setChannelNumber    :: Int -> MidiMonad ()
setChannelNumber i  = setsDS (\s -> s { st_channel_num = fromIntegral i })

resetDeltas         :: MidiMonad ()
resetDeltas         = setsDS (\s -> s { st_volume         = default_volume
                                      , st_balance        = default_pan
                                      , st_ellapsed_time  = 0
                                      } )

 

--------------------------------------------------------------------------------

renderMidi :: ZonedTime -> HiMidi -> MidiFile
renderMidi ztim himidi = evalMidiMonad (outputMidiM ztim himidi)

outputMidiM :: ZonedTime -> HiMidi -> MidiMonad MidiFile
outputMidiM ztim (HiMidi info ts) = 
    (\xs -> MidiFile hdr $ track_zero : (JL.toList xs)) <$> T.mapM outputTrack ts
  where
    track_zero  = if JL.null info then timeInfoTrack ztim else infoTrack info
    hdr         = MidiHeader MF1 count tpb
    count       = 1 + fromIntegral (JL.length ts)
    tpb         = TPB $ floor ticks_per_quarternote





infoTrack :: JoinList MidiMetaEvent -> MidiTrack
infoTrack ms = MidiTrack $ F.foldr fn [ sequenceName "Track 0", end_of_track ] ms
  where
    fn e ac =  (0, MetaEvent e) : ac


timeInfoTrack :: ZonedTime -> MidiTrack
timeInfoTrack ztim = MidiTrack $ 
    [ genericText "Generated by ZMidi."
    , genericText $ midiTimeStamp ztim
    , sequenceName "Track 0"
    , end_of_track
    ]


genericText :: String -> MidiMessage
genericText ss = (0, MetaEvent $ TextEvent GENERIC_TEXT ss)

sequenceName :: String -> MidiMessage
sequenceName ss = (0, MetaEvent $ TextEvent SEQUENCE_NAME ss)


end_of_track :: MidiMessage
end_of_track = (0, MetaEvent $ EndOfTrack)




microseconds_per_minute :: Double
microseconds_per_minute = 60000000


-- programChange :: Word8 -> Word8 -> MidiMessage
-- programChange inst ch = (0, VoiceEvent $ ProgramChange ch inst)

outputTrack :: Track -> MidiMonad MidiTrack
outputTrack track = 
    (\x xs -> MidiTrack $ x : collapseChannels xs) 
      <$> (nextTrack *> info) <*> mapM outputVoice low16
  where 
    info    = (\i -> sequenceName $ "Track" ++ show i) <$> getsDS st_track_num
    low16   = limit16 $ IM.toAscList $ getVoices track

    -- Midi can only support upto 16 tracks, indexed [0..15].
    limit16 :: [(Int,Voice)] -> [(Int,Voice)]
    limit16 = takeWhile (\(n,_) -> n < 16)



nextTrack :: MidiMonad ()
nextTrack = incrTrackNumber

-- | The sections in a ChannelStream are rendered sequentially
-- one after the other.
--
outputVoice :: (Int, Voice) -> MidiMonad ChannelStream
outputVoice (ch,vce) = 
    setChannelNumber ch >> resetDeltas >> F.foldlM mf mempty (getVoice vce)
  where
    mf :: ChannelStream -> Measure -> MidiMonad ChannelStream
    mf ac e = (\a -> ac `mappend` a) <$> outputMeasure e



outputMeasure  :: Measure -> MidiMonad ChannelStream
outputMeasure (Measure tmpo ovs) =
    JL.cons <$> setTempo tmpo <*> outputOverlays ovs
    



-- | Overlays all start at the same time - as the are rendered 
-- one-by-one the start time needs resetting each pass.
--
-- The maximum end-time of the individual tracks is retained.
-- This becomes the actual end-time after all the overlays have 
-- been rendered.
--
outputOverlays :: JoinList Overlay -> MidiMonad ChannelStream
outputOverlays xs = do
    common_start         <- getEllapsedTime
    (track_data,max_end) <- F.foldlM (mf common_start) (mempty,0) xs
    setEllapsedTime max_end
    return track_data
  where
    cat                   = mergeOrdered compare
    mf ct (ac,max_end) sv = do { setEllapsedTime ct
                               ; body <- overlay sv
                               ; et   <- getEllapsedTime
                               ; return (ac `cat` body, max max_end et)
                               }
   



overlay :: Overlay -> MidiMonad ChannelStream
overlay (Overlay xs) = primitives xs 


primitives :: [Primitive] -> MidiMonad ChannelStream
primitives []     = return mempty
primitives (x:xs) = step x xs
  where
    step a []     = primitive a
    step a (b:bs) = liftA2 mappend (primitive a) (step b bs)


primitive :: Primitive -> MidiMonad ChannelStream
primitive (PNote d props p)   = 
    liftM2 mappend (deltaPrimProps props) (primNote (durationr d) props p)

primitive (PChord d props ps) = 
    liftM2 mappend (deltaPrimProps props) (primChord (durationr d) props ps)

primitive (PRest d)           = 
    incrEllapsedTime (fromIntegral $ durationr d) >> return mempty


deltaPrimProps :: PrimProps -> MidiMonad ChannelStream
deltaPrimProps (PrimProps { prim_volume = vol }) = do
    dvol <- getVolume 
    if vol == dvol then return mempty
                   else setVolume vol >> primVolumeCtrl vol

primVolumeCtrl :: Word16 -> MidiMonad ChannelStream
primVolumeCtrl vol = liftM2 JL.two (voiceMsg ctrl7) (voiceMsg ctrl38)
  where
    lsb     = fromIntegral $ vol .&. 0x7F
    msb     = fromIntegral $ (vol `shiftR` 7) .&. 0x7F
    ctrl7   = \ch -> Controller ch 7  msb
    ctrl38  = \ch -> Controller ch 38 lsb


primNote :: Word32 -> PrimProps -> Word8 -> MidiMonad ChannelStream
primNote d props p = 
    (\non nof -> JL.two non nof) 
       <$> noteOn  p (prim_velo_on props)
       <*> (incrEllapsedTime (fromIntegral d) *> noteOff p (prim_velo_off props))


primChord :: Word32 -> PrimProps -> [Word8] -> MidiMonad ChannelStream
primChord d props ps  = 
    (\nons nofs -> JL.fromList nons `mappend` JL.fromList nofs)
      <$> mapM (\p -> noteOn p (prim_velo_on props)) ps
      <*> (incrEllapsedTime (fromIntegral d) *> 
            mapM (\p -> noteOff p (prim_velo_on props)) ps)



voiceMsg :: VoiceMsg -> MidiMonad AbsMidiMessage
voiceMsg f = (\et ch -> AMM et (VoiceEvent $ f ch)) 
                 <$> getEllapsedTime <*> getChannelNumber
            


-- metaEvent :: MidiMetaEvent -> MidiMonad AbsMidiMessage
-- metaEvent evt = (\et -> AMM et (MetaEvent evt)) <$> getEllapsedTime


noteOn :: Word8 -> Word8 -> MidiMonad AbsMidiMessage
noteOn pch vel = (\et ch -> AMM et (VoiceEvent $ NoteOn ch pch vel)) 
                    <$> getEllapsedTime <*> getChannelNumber


noteOff :: Word8 -> Word8 -> MidiMonad AbsMidiMessage
noteOff pch vel = (\et ch -> AMM et (VoiceEvent $ NoteOff ch pch vel))
                    <$> getEllapsedTime <*> getChannelNumber


setTempo :: Double -> MidiMonad AbsMidiMessage
setTempo bpm = (\et -> AMM et (MetaEvent $ SetTempo mspqn)) <$> getEllapsedTime
  where
    mspqn = floor $ microseconds_per_minute / bpm


--------------------------------------------------------------------------------


-- | To be used with getZonedTime

midiTimeStamp :: ZonedTime -> String
midiTimeStamp zt = bodyS [] 
  where
    bodyS       = localTimeS . showChar ' ' . localDayS
    local_tim   = zonedTimeToLocalTime zt
    localTimeS  = timeOfDay  $ localTimeOfDay $ local_tim
    localDayS   = showString $ showGregorian  $ localDay local_tim

timeOfDay :: TimeOfDay -> ShowS
timeOfDay t = 
    fn todHour . showChar ':' . fn todMin . showChar ':' . fn (floori . todSec)
  where
    fn f = pad2 (f t) 


pad2 :: Int -> ShowS
pad2 i | i < 10    = ('0':) . shows i
       | otherwise = shows i  


floori :: RealFrac a => a -> Int
floori = floor





--------------------------------------------------------------------------------

type ChannelStream = JoinList AbsMidiMessage

data AbsMidiMessage = AMM !Int MidiEvent
  deriving (Eq,Ord,Show)

collapseChannels :: [ChannelStream] -> [MidiMessage]
collapseChannels = deltaTransform . concatMessages


concatMessages :: [ChannelStream] -> ChannelStream
concatMessages []     = mempty
concatMessages [x]    = x
concatMessages (x:xs) = foldl' (mergeOrdered cmp) x xs
  where
    cmp (AMM t1 _) (AMM t2 _) = t1 `compare` t2


mergeOrdered :: (a -> a -> Ordering) -> JoinList a -> JoinList a -> JoinList a
mergeOrdered cmp a b = step (viewl a) (viewl b)
  where
    step EmptyL    vl         = JL.unViewL vl
    step vl        EmptyL     = JL.unViewL vl
    step (x :< xs) (y :< ys)  = 
        case cmp x y of
          LT -> x `cons`          step (viewl xs)    (viewl $ y `cons` ys)
          EQ -> x `cons` y `cons` step (viewl xs)            (viewl ys)
          GT -> y `cons`          step (viewl $ x `cons` xs) (viewl ys)


-- | Upto this point, messages are labelled with absolute time 
-- rather than delta time.
--
-- This transforms the messages to use delta time and appends an 
-- end-of-track message to the tail of the list.
--
deltaTransform :: JoinList AbsMidiMessage -> [MidiMessage]
deltaTransform = step 0 . viewl
  where
    step _    EmptyL                 = [end_of_track]
    step abst ((AMM evt body) :< xs) = let dt = fromIntegral $ evt - abst
                                       in (dt,body) : step evt (viewl xs)



