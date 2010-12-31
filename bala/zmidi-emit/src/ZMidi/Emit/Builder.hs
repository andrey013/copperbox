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
-- Monadic building for /high level/ MIDI, with a reader writer monad. 
-- 
-- The reader monad makes some of the parameters implicit to the
-- build functions, which would otherwise require many arguments.
-- The writer monad accumulates notes efficiently. 
--
--------------------------------------------------------------------------------

module ZMidi.Emit.Builder
  (


  -- * Build monad
    BuildEnv(..)
  , build_env_zero

  , Build
  , runBuild
  , execBuild
 
  , localize 
  , report
  , askEnv
  , asksEnv


  , instrument
  , note
  , chord
  , rest

  , singleSection
  , overlayVoices

  , section
  , overlays

  ) where


import ZMidi.Emit.Datatypes
import ZMidi.Emit.Constructors
import ZMidi.Emit.Utils.HList
import qualified ZMidi.Emit.Utils.JoinList as JL

import ZMidi.Core                               -- package: zmidi-core

import Control.Applicative
import Control.Monad
import Data.Monoid
import Data.Word



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
                          

runBuild :: BuildEnv -> Build a -> (a, [Primitive])
runBuild env ma = post $ getBuild ma env
  where
    post (a,f) = (a, toListH f)

execBuild :: BuildEnv -> Build a -> [Primitive]
execBuild env ma = post $ getBuild ma env
  where
    post (_,f) = toListH f




localize :: (BuildEnv -> BuildEnv) -> Build a -> Build a
localize f ma = Build $ \r -> getBuild ma (f r)


askEnv   :: Build BuildEnv
askEnv   = Build $ \r -> (r, mempty)


report   :: Primitive -> Build ()
report a = Build $ \_ -> ((), wrapH a)


asksEnv :: (BuildEnv -> a) -> Build a
asksEnv extr = extr <$> askEnv


noteProps :: Build PrimProps
noteProps = (\r -> PrimProps { velocity_on    = note_on_velocity r
                             , velocity_off   = note_off_velocity r
                             , note_volume    = note_volume_level r
                             })
              <$> askEnv


instrument :: GMInst -> Build ()
instrument inst  = report prog {- >> report name -}
  where
    prog = primVoiceMessage $ \ch -> ProgramChange ch inst
--    name = primMetaEvent $ TextEvent INSTRUMENT_NAME (instrumentName inst)

    


note :: MidiDuration -> MidiPitch -> Build ()
note d p = noteProps >>= \props -> report (PNote d props p)

chord :: MidiDuration -> [MidiPitch] -> Build ()
chord d ps = noteProps >>= \props -> report (PChord d props ps)

rest :: MidiDuration -> Build ()
rest d = report $ PRest d



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

voice :: Build a -> SectionVoice
voice = SectionVoice . execBuild build_env_zero



section :: Double -> Build a -> ChannelStream
section bpm xs = ChannelStream $ JL.one $ Section bpm $ JL.one $ voice xs


overlays :: Double -> [Build a] -> ChannelStream
overlays bpm xs = 
    ChannelStream $ JL.one $ Section bpm $ JL.fromListF voice xs

