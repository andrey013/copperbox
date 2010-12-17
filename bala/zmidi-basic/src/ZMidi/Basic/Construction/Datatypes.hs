{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZMidi.Basic.Construction.Datatypes
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Syntax for /high level/ MIDI.
--
--------------------------------------------------------------------------------

module ZMidi.Basic.Construction.Datatypes
  ( 
    MultiChannelTrack
  , Section(..)
  , SectionVoice(..)
  , Primitive(..)
  , PrimProps(..)

  ) where



import Data.Word


type MultiChannelTrack = [Section]

data Section = Section 
      { section_tempo           :: Double
      , section_data            :: [SectionVoice]
      }
  deriving (Eq,Ord,Show)

data SectionVoice = SectionVoice 
      { voice_instrument        :: Word8
      , voice_notelist          :: [Primitive]
      }
  deriving (Eq,Ord,Show)


-- | Primitive is either a note, chord or rest.
--
-- Duration is a Double - 0.25 is a quarter note, 1 is a whole 
-- note. It is expected that client software with use some other 
-- type but convert to Double as it builds the syntax.
--
data Primitive = PNote   Double PrimProps Word8
               | PChord  Double PrimProps [Word8]
               | PRest   Double
   deriving (Eq,Ord,Show)


-- All notes in a chord have the same properties...
--
data PrimProps = PrimProps
      { velocity_on     :: Word8
      , velocity_off    :: Word8
      , note_volume     :: Word8
      }
  deriving (Eq,Ord,Show)


