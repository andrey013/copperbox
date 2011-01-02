{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZMidi.Emit.OutputMidi
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Output MIDI.
--
-- WARNING TO SELF - are you printing sequence numbers in the infor track?
--
--------------------------------------------------------------------------------

module ZMidi.Emit.OutputMidi
  (
    writeHiMidi
  ) where

import ZMidi.Emit.Datatypes
import ZMidi.Emit.Utils.JoinList ( JoinList, ViewL(..), viewl, cons )
import qualified ZMidi.Emit.Utils.JoinList as JL

import ZMidi.Core                               -- package: zmidi-core

import Control.Applicative
import Control.Monad
import Data.List
import qualified Data.IntMap as IM
import Data.Monoid
import Data.Time
import qualified Data.Foldable as F
import Data.Word





-- | Output a MIDI file. The format is always a Type 1 MIDI file
-- even if the HiMidi object only has a single track.
--
-- Note - if no text (e.g. copyright notice, or lyrics) has been
-- added to the HiMidi object, then @ZMidi-Emit@ will print a 
-- time-stamp as @generic-text@ in track 0.
--
writeHiMidi :: FilePath -> HiMidi -> IO ()
writeHiMidi filename mct = 
    getZonedTime >>= \ztim -> writeMidi filename (outputZMR ztim mct)



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

-- Note - the state tracks absolute time, conversion to delta time 
-- is performed as a traversal afterwards

data RState = RState
      { rs_volume               :: Word8
      , rs_ellapsed_time        :: Word32
      }

zeroRS :: RState
zeroRS = RState { rs_volume          = 127
                , rs_ellapsed_time   = 0
                }

newtype REnv = REnv { re_chan_number         :: Int }


newtype OutMonad a = OutMonad { 
          getOutMonad :: REnv -> RState -> (a,RState) }


instance Functor OutMonad where
  fmap f mf = OutMonad $ \r s -> let (a,s1) = getOutMonad mf r s in (f a,s1)

instance Applicative OutMonad where
  pure a    = OutMonad $ \_ s -> (a,s)
  mf <*> ma = OutMonad $ \r s -> let (f,s1) = getOutMonad mf r s
                                     (a,s2) = getOutMonad ma r s1
                                 in (f a,s2)

instance Monad OutMonad where
  return a  = OutMonad $ \_ s -> (a,s)
  m >>= k   = OutMonad $ \r s -> let (a,s1) = getOutMonad m r s
                                 in (getOutMonad . k) a r s1
                              

runOutMonad :: Int -> OutMonad a -> a
runOutMonad track_num mf = fst $ getOutMonad mf (REnv track_num) zeroRS


getsRS :: (RState -> a) -> OutMonad a
getsRS fn = OutMonad $ \_ s -> (fn s,s)


setsRS :: (RState -> RState) -> OutMonad ()
setsRS fn = OutMonad $ \_ s -> ((),fn s)

asksRE :: (REnv -> a) -> OutMonad a
asksRE fn = OutMonad $ \e s -> (fn e,s)



incrEllapsedTime :: Word32 -> OutMonad ()
incrEllapsedTime n = 
    setsRS (\s -> let i = rs_ellapsed_time s in s { rs_ellapsed_time = i+n})


getEllapsedTime     :: OutMonad Word32
getEllapsedTime     = getsRS rs_ellapsed_time

-- Note - this is needed for overlays which all must begin at the 
-- same time.
--
setEllapsedTime  :: Word32 -> OutMonad ()
setEllapsedTime t = setsRS (\s -> s { rs_ellapsed_time = t })

askChannelNumber    :: OutMonad Word8
askChannelNumber    = fmap fromIntegral $ asksRE re_chan_number




outputZMR :: ZonedTime -> HiMidi -> MidiFile
outputZMR ztim (HiMidi info ts) = 
    MidiFile hdr $ track_zero : JL.zipWithIntoList fn ts [1..]
  where
    fn          = flip outputAudioTrack
    track_zero  = if JL.null info then timeInfoTrack ztim else infoTrack info
    hdr         = MidiHeader MF1 len tpb
    len         = fromIntegral $ 1 + JL.length ts
    tpb         = TPB $ floor ticks_per_quarternote


infoTrack :: JoinList MidiMetaEvent -> MidiTrack
infoTrack mmes = MidiTrack $ body ++ [ sequenceName "Track 0", end_of_track ]
  where
    body = JL.toListF (\mme -> (0, MetaEvent mme)) mmes


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



microseconds_per_minute :: Double
microseconds_per_minute = 60000000


-- programChange :: Word8 -> Word8 -> MidiMessage
-- programChange inst ch = (0, VoiceEvent $ ProgramChange ch inst)

outputAudioTrack :: Int -> Track -> MidiTrack
outputAudioTrack track_num (Track im) = MidiTrack $ info : body
  where 
    info = sequenceName $ "Track" ++ show track_num
    body = collapseChannels $ map (uncurry outputChannelStream) 
                            $ limit16 $ IM.toAscList im


-- | Midi can only support upto 16 tracks, indexed [0..15].
--
limit16 :: [(Int,ChannelStream)] -> [(Int,ChannelStream)]
limit16 = takeWhile (\(n,_) -> n < 16)


type TrackData = JoinList MidiMessage

collapseChannels :: [TrackData] -> [MidiMessage]
collapseChannels = deltaTransform . concatMessages




-- | The sections in a ChannelStream are rendered sequentially
-- one after the other.
--
outputChannelStream :: Int -> ChannelStream -> TrackData
outputChannelStream ch strm = 
    runOutMonad ch $ F.foldlM mf mempty $ getSections strm
  where
    mf :: TrackData -> Section -> OutMonad TrackData
    mf ac e = (\a -> ac `mappend` a) <$> outputSection e



outputSection  :: Section -> OutMonad TrackData
outputSection (Section tmpo ovs) =
    JL.cons <$> setTempo tmpo <*> outputOverlays ovs
    



-- | Overlays all start at the same time - as the are rendered 
-- one-by-one the start time needs resetting each pass.
--
-- The maximum end-time of the individual tracks is retained.
-- This becomes the actual end-time after all the overlays have 
-- been rendered.
--
outputOverlays :: Overlays -> OutMonad TrackData
outputOverlays xs = do
    common_start <- getEllapsedTime
    (track_data,max_end) <- F.foldlM (mf common_start) (mempty,0) xs
    setEllapsedTime max_end
    return track_data
  where
    cat                   = mergeOrdered compare
    mf ct (ac,max_end) sv = do { setEllapsedTime ct
                               ; body <- sectionVoice sv
                               ; et   <- getEllapsedTime
                               ; return (ac `cat` body, max max_end et)
                               }
   


concatMessages :: [TrackData] -> TrackData
concatMessages []     = mempty
concatMessages [x]    = x
concatMessages (x:xs) = foldl' (mergeOrdered cmp) x xs
  where
    cmp (d1,_) (d2,_) = d1 `compare` d2


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
deltaTransform :: JoinList MidiMessage -> [MidiMessage]
deltaTransform = step 0 . viewl
  where
    step _    EmptyL             = [end_of_track]
    step abst ((evt,body) :< xs) = (evt - abst,body) : step evt (viewl xs)


end_of_track :: MidiMessage
end_of_track = (0, MetaEvent $ EndOfTrack)


sectionVoice :: SectionVoice -> OutMonad TrackData
sectionVoice (SectionVoice xs) = primitives xs 


primitives :: [Primitive] -> OutMonad TrackData
primitives []     = return mempty
primitives (x:xs) = step x xs
  where
    step a []     = primitive a
    step a (b:bs) = liftA2 mappend (primitive a) (step b bs)


primitive :: Primitive -> OutMonad TrackData
primitive (PNote d props p)   = primNote (durationr d) props p
primitive (PChord d props ps) = primChord (durationr d) props ps
primitive (PRest d)           = incrEllapsedTime (durationr d) >> return mempty
primitive (PMsg msg)          = fmap JL.one $ either voiceMsg metaEvent msg


primNote :: Word32 -> PrimProps -> Word8 -> OutMonad TrackData
primNote d props p = 
    (\non nof -> JL.two non nof) 
       <$> noteOn  p (velocity_on props)
       <*> (incrEllapsedTime d *> noteOff p (velocity_off props))




primChord :: Word32 -> PrimProps -> [Word8] -> OutMonad TrackData
primChord d props ps  = 
    (\nons nofs -> JL.fromList nons `mappend` JL.fromList nofs)
      <$> mapM (\p -> noteOn p (velocity_on props)) ps
      <*> (incrEllapsedTime d *> 
            mapM (\p -> noteOff p (velocity_on props)) ps)



voiceMsg :: VoiceMsg -> OutMonad MidiMessage
voiceMsg f = (\et ch -> (et, VoiceEvent $ getVoiceMsg f ch)) 
                <$> getEllapsedTime <*> askChannelNumber
             

metaEvent :: MidiMetaEvent -> OutMonad MidiMessage
metaEvent evt = (\et -> (et, MetaEvent evt)) <$> getEllapsedTime


noteOn :: Word8 -> Word8 -> OutMonad MidiMessage
noteOn pch vel = (\et ch -> (et, VoiceEvent $ NoteOn ch pch vel)) 
                    <$> getEllapsedTime <*> askChannelNumber


noteOff :: Word8 -> Word8 -> OutMonad MidiMessage
noteOff pch vel = (\et ch -> (et, VoiceEvent $ NoteOff ch pch vel))
                    <$> getEllapsedTime <*> askChannelNumber


setTempo :: Double -> OutMonad MidiMessage
setTempo bpm = (\et -> (et, MetaEvent $ SetTempo mspqn)) <$> getEllapsedTime
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