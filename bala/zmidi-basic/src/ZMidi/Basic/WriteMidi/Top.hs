{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZMidi.Basic.WriteMidi.Top
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Version number
--
--------------------------------------------------------------------------------

module ZMidi.Basic.WriteMidi.Top

{-
  ( 
    Primitive(..)
  , PrimProps(..)

  ) where
-}
    where

import ZMidi.Basic.WriteMidi.HList

import ZMidi.Core                               -- package: zmidi-core

import Control.Applicative
import Data.List
import Data.Word




-- Primitive is either a note, chord or rest.
--
data Primitive = PNote   Word32 PrimProps Word8
               | PChord  Word32 PrimProps [Word8]
               | PRest   Word32
   deriving (Eq,Ord,Show)


-- All notes in a chord have the same properties...
--
data PrimProps = PrimProps
      { velocity_on     :: Word8
      , velocity_off    :: Word8
      , note_volume     :: Word8
      }
  deriving (Eq,Ord,Show)



--------------------------------------------------------------------------------

-- Note - the state tracks absolute time, conversion to delta time 
-- is performed as a traversal afterwards

data RenderState = RenderState
      { rs_volume               :: Word8
      , rs_ellapsed_time        :: Word32
      , rs_channel_number       :: Word8        -- 0..15
      }
  deriving (Eq,Show)

zeroRS :: RenderState
zeroRS = RenderState { rs_volume          = 127
                     , rs_ellapsed_time   = 0
                     , rs_channel_number  = 0
                     }


newtype OutMonad a = OutMonad { 
         getOutMonad :: RenderState -> (a,RenderState) }


instance Functor OutMonad where
  fmap f mf = OutMonad $ \s -> let (a,s1) = getOutMonad mf s in (f a,s1)

instance Applicative OutMonad where
  pure a    = OutMonad $ \s -> (a,s)
  mf <*> ma = OutMonad $ \s -> let (f,s1) = getOutMonad mf s
                                   (a,s2) = getOutMonad ma s1
                               in (f a,s2)

instance Monad OutMonad where
  return a  = OutMonad $ \s -> (a,s)
  m >>= k   = OutMonad $ \s -> let (a,s1) = getOutMonad m s
                               in (getOutMonad . k) a s1
                              

runOutMonad :: OutMonad a -> a
runOutMonad mf = fst $ getOutMonad mf zeroRS


getsRS :: (RenderState -> a) -> OutMonad a
getsRS fn = OutMonad $ \s -> (fn s,s)


setsRS :: (RenderState -> RenderState) -> OutMonad ()
setsRS fn = OutMonad $ \s -> ((),fn s)


incrChannelNumber :: OutMonad ()
incrChannelNumber = 
    setsRS (\s -> let i = rs_channel_number s in s { rs_channel_number = i+1})


incrEllapsedTime :: Word32 -> OutMonad ()
incrEllapsedTime n = 
    setsRS (\s -> let i = rs_ellapsed_time s in s { rs_ellapsed_time = i + n})

zeroEllapsedTime    :: OutMonad ()
zeroEllapsedTime    = setsRS (\s -> s { rs_ellapsed_time = 0 })

getEllapsedTime     :: OutMonad Word32
getEllapsedTime     = getsRS rs_ellapsed_time

getChannelNumber    :: OutMonad Word8
getChannelNumber    = getsRS rs_channel_number

type MultiChannelTrack = [[Primitive]]

outputMCST :: [MultiChannelTrack] -> MidiFile
outputMCST xs = MidiFile hdr $ map outputTrack xs
  where
    hdr = Header MF1 (fromIntegral $ length xs) (TPB 480)


outputTrack :: [[Primitive]] -> Track
outputTrack xss = Track $ runOutMonad $ channelBody xss

channelBody :: [[Primitive]] -> OutMonad [Message]
channelBody = 
    fmap (deltaTransform . concatMessages) . mapM channelData . limit16


-- The MIDI file format only allows 16 channels per track.
--
limit16 :: [a] -> [a]
limit16 = take 16

concatMessages :: [[Message]] -> [Message]
concatMessages []     = []
concatMessages [x]    = x
concatMessages (x:xs) = foldl' (mergeOrdered cmp) x xs
  where
    cmp (d1,_) (d2,_) = d1 `compare` d2


mergeOrdered :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
mergeOrdered cmp = step
  where
    step []     ys      = ys
    step xs     []      = xs
    step (x:xs) (y:ys)  = case cmp x y of
                            LT -> x : step xs (y:ys)
                            EQ -> x : y : step xs ys
                            GT -> y : step (x:xs) ys

deltaTransform :: [Message] -> [Message]
deltaTransform = snd . mapAccumL step 0
  where
    step abst (evt,body) = (evt, (evt-abst,body))



end_of_track :: Message
end_of_track = (0, MetaEvent $ EndOfTrack)


type HChannelData = H Message

channelData :: [Primitive] -> OutMonad [Message]
channelData xs = do 
    hs <- primitives xs 
    incrChannelNumber 
    zeroEllapsedTime
    return $ toListH hs

primitives :: [Primitive] -> OutMonad HChannelData
primitives []     = return []
primitives (x:xs) = step x xs
  where
    step a []     = primitive a
    step a (b:bs) = liftA2 appendH (primitive a) (step b bs)

primitive :: Primitive -> OutMonad HChannelData
primitive (PNote d props p)     = primNote d props p
primitive (PChord d props ps)   = primChord d props ps
primitive (PRest d)             = incrEllapsedTime d >> return emptyH

primNote :: Word32 -> PrimProps -> Word8 -> OutMonad HChannelData
primNote d props p = do 
   et <- getEllapsedTime
   ch <- getChannelNumber
   incrEllapsedTime d
   let non = mkNoteOn  et     p ch (velocity_on props)
   let nof = mkNoteOff (et+d) p ch (velocity_off props)
   return (twoH non nof)


primChord :: Word32 -> PrimProps -> [Word8] -> OutMonad HChannelData
primChord d props ps  = do 
   et <- getEllapsedTime
   ch <- getChannelNumber
   incrEllapsedTime d
   let nons = map (\p -> mkNoteOn  et     p ch (velocity_on props))  ps
   let nofs = map (\p -> mkNoteOff (et+d) p ch (velocity_off props)) ps
   return (fromListH nons `appendH` fromListH nofs)



mkNoteOn :: Word32 -> Word8 -> Word8 -> Word8 -> Message
mkNoteOn dt pch ch vel = (dt, VoiceEvent $ NoteOn ch pch vel)

mkNoteOff :: Word32 -> Word8 -> Word8 -> Word8 -> Message
mkNoteOff dt pch ch vel = (dt, VoiceEvent $ NoteOff ch pch vel)