{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZMidi.Emit.Builder
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Building /high level/ MIDI.
-- 
-- TODO - is it useful to support a monad and a transformer?
--
--------------------------------------------------------------------------------

module ZMidi.Emit.Builder
  (

  -- * Type aliases
    MidiPitch
  , MidiDuration

  -- * Pitch constructors 
  , c_nat  
  , c_sharp
  , d_flat
  , d_nat
  , d_sharp
  , e_flat
  , e_nat
  , f_nat
  , f_sharp  
  , g_flat
  , g_nat
  , g_sharp
  , a_flat
  , a_nat
  , b_flat
  , b_nat

  -- * Duration constructors
  , dwhole
  , dhalf
  , dquarter
  , deighth
  , dsixteenth

  -- * Build monad
  , BuildEnv(..)
  , build_env_zero

  , Build
  , BuildT
  , BuildM(..)
  , runBuild
  , execBuild
  , runBuildT
  , asksEnv


  , instrument
  , note
  , chord
  , rest
  , drumNote
  , drumChord

  , singleSection
  , overlayVoices

  ) where


import ZMidi.Emit.Datatypes
import ZMidi.Emit.GeneralMidiInstruments
import ZMidi.Emit.Utils.HList
import qualified ZMidi.Emit.Utils.JoinList as JL

import ZMidi.Core                               -- package: zmidi-core

import Control.Applicative
import Control.Monad
import Data.Monoid
import Data.Word


type MidiPitch    = Word8
type MidiDuration = Double


--------------------------------------------------------------------------------

clampPch :: Int -> Word8
clampPch i | i < 0     = 0
           | i > 127   = 127
           | otherwise = fromIntegral i


c_nat       :: Int -> Word8
c_nat o     = clampPch $     12 * (o + 1)

c_sharp     :: Int -> Word8
c_sharp o   = clampPch $ 1 + 12 * (o + 1)

d_flat      :: Int -> Word8
d_flat      = c_sharp

d_nat       :: Int -> Word8
d_nat o     = clampPch $ 2 + 12 * (o + 1)

d_sharp     :: Int -> Word8
d_sharp o   = clampPch $ 3 + 12 * (o + 1)

e_flat      :: Int -> Word8
e_flat      = d_sharp

e_nat       :: Int -> Word8
e_nat o     = clampPch $ 4 + 12 * (o + 1)

f_nat       :: Int -> Word8
f_nat o     = clampPch $ 5 + 12 * (o + 1)

f_sharp     :: Int -> Word8
f_sharp o   = clampPch $ 6 + 12 * (o + 1)

g_flat      :: Int -> Word8
g_flat      = f_sharp

g_nat       :: Int -> Word8
g_nat o     = clampPch $ 7 + 12 * (o + 1)

g_sharp     :: Int -> Word8
g_sharp o   = clampPch $ 8 + 12 * (o + 1)

a_flat      :: Int -> Word8
a_flat     = a_sharp

a_nat       :: Int -> Word8
a_nat o     = clampPch $ 9 + 12 * (o + 1)

a_sharp     :: Int -> Word8
a_sharp o   = clampPch $ 10 + 12 * (o + 1)

b_flat      :: Int -> Word8
b_flat      = a_sharp

b_nat       :: Int -> Word8
b_nat o     = clampPch $ 11 + 12 * (o + 1)



dwhole      :: MidiDuration
dwhole      = 1.0

dhalf       :: MidiDuration
dhalf       = 0.5

dquarter    :: MidiDuration
dquarter    = 0.25

deighth     :: MidiDuration
deighth     = 0.125

dsixteenth  :: MidiDuration
dsixteenth   = 0.0625




--------------------------------------------------------------------------------




data BuildEnv = BuildEnv
      { note_on_velocity        :: Word8
      , note_off_velocity       :: Word8
      , note_volume_level       :: Word8
      }
  deriving (Eq,Ord,Show)


build_env_zero :: BuildEnv
build_env_zero = BuildEnv
      { note_on_velocity     = 127
      , note_off_velocity    = 64
      , note_volume_level    = 127
      }


newtype Build a = Build { getBuild :: BuildEnv -> (a, H Primitive) }

instance Functor Build where
  fmap f ma = Build $ \r -> let (a,w) = getBuild ma r in (f a, w)


instance Applicative Build where
  pure a    = Build $ \_ -> (a, mempty)
  mf <*> ma = Build $ \r -> let (f,w1) = getBuild mf r
                                (a,w2) = getBuild ma r
                            in (f a, w1 `mappend` w2)


instance Monad Build where
  return a  = Build $ \_ -> (a,mempty)
  m >>= k   = Build $ \r -> let (a,w1) = getBuild m r 
                                (b,w2) = getBuild (k a) r
                            in (b, w1 `mappend` w2)
                          

newtype BuildT m a = BuildT { getBuildT :: BuildEnv -> m (a, H Primitive) }


instance Monad m => Functor (BuildT m) where
  fmap f ma = BuildT $ \r -> getBuildT ma r >>= \(a,w) -> return (f a, w)


instance Monad m => Applicative (BuildT m) where
  pure a    = BuildT $ \_ -> return (a, mempty)
  mf <*> ma = BuildT $ \r -> getBuildT mf r >>= \(f,w1) -> 
                             getBuildT ma r >>= \(a,w2) -> 
                             return (f a, w1 `appendH` w2)
                       


instance Monad m => Monad (BuildT m) where
  return a  = BuildT $ \_ -> return (a, mempty)
  m >>= k   = BuildT $ \r -> getBuildT m r     >>= \(a,w1) -> 
                             getBuildT (k a) r >>= \(b,w2) -> 
                             return (b, w1 `mappend` w2)


runBuild :: BuildEnv -> Build a -> (a, [Primitive])
runBuild env ma = post $ getBuild ma env
  where
    post (a,f) = (a, toListH f)

execBuild :: BuildEnv -> Build a -> [Primitive]
execBuild env ma = post $ getBuild ma env
  where
    post (_,f) = toListH f


runBuildT :: Monad m => BuildEnv -> BuildT m a -> m (a, [Primitive])
runBuildT env ma = liftM post $ getBuildT ma env
  where
    post (a,f) = (a, toListH f)


class (Applicative m, Monad m) => BuildM m where
  localize :: (BuildEnv -> BuildEnv) -> m a -> m a
  askEnv   :: m BuildEnv
  tell     :: Primitive -> m ()


instance BuildM Build where
  localize f ma = Build $ \r -> getBuild ma (f r)
  askEnv        = Build $ \r -> (r, mempty)
  tell a        = Build $ \_ -> ((), wrapH a)

instance Monad m => BuildM (BuildT m) where
  localize f ma = BuildT $ \r -> getBuildT ma (f r)
  askEnv        = BuildT $ \r -> return (r, mempty)
  tell a        = BuildT $ \_ -> return ((), wrapH a)



asksEnv :: BuildM m => (BuildEnv -> a) -> m a
asksEnv extr = extr <$> askEnv


noteProps :: BuildM m => m PrimProps
noteProps = (\r -> PrimProps { velocity_on    = note_on_velocity r
                             , velocity_off   = note_off_velocity r
                             , note_volume    = note_volume_level r
                             })
              <$> askEnv


instrument :: BuildM m => GMInst -> m ()
instrument inst  = tell prog >> tell name
  where
    prog = vinstrument inst
    name = primMetaEvent $ TextEvent INSTRUMENT_NAME (instrumentName inst)


note :: BuildM m => MidiDuration -> MidiPitch -> m ()
note d p = noteProps >>= \props -> tell (PNote d props p)

chord :: BuildM m => MidiDuration -> [MidiPitch] -> m ()
chord d ps = noteProps >>= \props -> tell (PChord d props ps)

rest :: BuildM m => MidiDuration -> m ()
rest d = tell $ PRest d


drumNote :: BuildM m => MidiDuration -> GMDrum -> m ()
drumNote d p = note d (drumPitch p)


drumChord :: BuildM m => MidiDuration -> [GMDrum] -> m ()
drumChord d ps = chord d (map drumPitch ps)


-- These two are arguably at the wrong type - shouldn\'t be
-- (Build a)...

singleSection :: Double -> [Build a] -> ChannelStream
singleSection bpm phrases = 
    ChannelStream $ JL.one $ Section bpm $ JL.fromListF fn phrases
  where
    fn = SectionVoice . execBuild build_env_zero
   

-- note sure this is doing what I expect ...
-- (Plus I need more care with naming schemes).
--
overlayVoices :: Double -> [[Build a]] -> ChannelStream
overlayVoices bpm voices = 
    ChannelStream $ JL.fromList $ map top voices
  where
    top = Section bpm . JL.fromListF fn
    fn  = SectionVoice . execBuild build_env_zero

