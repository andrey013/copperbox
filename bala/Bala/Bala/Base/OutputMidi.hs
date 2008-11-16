
--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Base.OutputMidi
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Output Midi
--
--------------------------------------------------------------------------------



module Bala.Base.OutputMidi where

import Bala.Base.BaseExtra
import Bala.Base.Duration
import Bala.Base.Pitch
import Bala.Base.Structural hiding (note, chord)

import ZMidi
import ZMidi.Construction

import qualified Data.Foldable as F
import qualified Data.Map as Map
import Data.Sequence
import Data.Word

generateMidi :: Duration -> NoteList -> MidiFile
generateMidi bar_len ns = 
  execConstruction (midiLines $ noteListToLines bar_len ns) 384 120 

noteListToLines :: Duration -> NoteList -> [Seq (Seq Elt)]   
noteListToLines bar_len = 
    straightLines bar_len . buildOM . labelOverlays . labelBars
    
    

labelBars :: NoteList -> Seq (Int, Bar)
labelBars (NoteList se) = szipl' se [1..]


labelOverlays :: Seq (Int, Bar) -> Seq (Int, Int, Seq Elt)
labelOverlays = step empty . viewl where
  step acc EmptyL                       = acc 
  step acc ((n, (Bar se))      :< sse)  = step (acc |> (n,0,se)) (viewl sse)
  step acc ((n, (Overlay ovs)) :< sse)  = 
      let ovs' = fmap (\(i,a) -> (n,i,a)) $ szipl' ovs [0..]
      in step (acc >< ovs') (viewl sse)




type OverlayMap = Map.Map Int (Seq (Int, Int, Seq Elt))



buildOM :: Seq (Int, Int, Seq Elt) -> OverlayMap
buildOM = F.foldl fn Map.empty where
    fn m (n,i,se) = insertOM i (n,i,se) m 

    insertOM :: Int -> (Int, Int, Seq Elt) -> OverlayMap -> OverlayMap
    insertOM k v m = Map.insertWith (\a b -> b >< a) k (singleton v) m


straightLines :: Duration -> OverlayMap -> [Seq (Seq Elt)]
straightLines bar_len = map (packBars bar_len . viewl . fn) . Map.toList where
    fn (k,v) = fmap (\(i,_,se) -> (i,se)) v 


-- bars need to be packed ? ...
packBars :: Duration -> ViewL (Int,Seq Elt) -> Seq (Seq Elt)
packBars bar_len EmptyL           = empty
packBars bar_len ((n,se) :< sse)  
    | n == 1                      = pack (singleton se) 0 (viewl sse)
    | otherwise                   = pack (blankBars n |> se) n (viewl sse)
  where       
    blankBars n = sreplicate n (singleton $ rest bar_len) 
    
    pack acc n EmptyL           = acc
    pack acc n ((i,se) :< sse)
        -- contiguous -- no packing needed  
        | n+1 == i              = pack (acc |> se) i (viewl sse)
        | otherwise             = let fill = blankBars (i - n + 1) 
                                  in pack ((acc >< fill) |> se) i (viewl sse) 
    

    
midiLines :: [Seq (Seq Elt)] -> OutputMidi ()
midiLines = F.mapM_ outputLine

outputLine :: Seq (Seq Elt) -> OutputMidi ()
outputLine sse = F.mapM_ (F.mapM_ outputElt) sse >> nextChannel

        
outputElt :: Elt -> OutputMidi ()
outputElt (DEvt (Note p) d)   = note (midiPitch p) (ticks d)
outputElt (DEvt Rest d)       = spacer (ticks d)
outputElt (DEvt Spacer d)     = spacer (ticks d)
outputElt (Mark _)            = return ()
outputElt (Chord se d)        = chord (F.foldr fn [] se) (ticks d) where
    fn p xs = (midiPitch p) : xs
    
outputElt (AGrace se p d)     = note (midiPitch p) (ticks d) -- to do
outputElt (UGrace p d se)     = note (midiPitch p) (ticks d) -- to do


durationGraces :: (Seq (Pitch, Duration)) -> Duration
durationGraces = F.foldr (\e n -> snd e + n) 0



midiPitch :: Pitch -> Word8
midiPitch = fromIntegral . (+12) . semitones

ticks :: Duration -> Word32
ticks d | d == no_duration = 0
        | otherwise        = fn $ ratioElements $ convRational d
  where
    fn (n,1) = n * midi_wn
    fn (1,d) = midi_wn `div` d
    fn (n,d) = (n * midi_wn) `div` d  

midi_qn         :: Word32
midi_qn         = 384

midi_wn         :: Word32
midi_wn         = midi_qn * 4

            