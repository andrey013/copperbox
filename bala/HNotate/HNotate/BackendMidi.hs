--------------------------------------------------------------------------------
-- |
-- Module      :  HNotate.BackendMidi
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Emit Midi from Score representation.
--
--------------------------------------------------------------------------------


module HNotate.BackendMidi where

import HNotate.CommonUtils
import HNotate.Duration hiding (duration)
import HNotate.Env
import HNotate.MiniMidi
import HNotate.NoteListDatatypes
import HNotate.NotateMonad
import HNotate.OnsetQueue
import HNotate.Pitch
import HNotate.ProcessingTypes


import Control.Monad.Reader
import qualified Data.Foldable as F
import Data.Sequence
import Data.Word


midiOut :: FilePath -> NoteList -> NotateT IO ()
midiOut path evts = do 
    bar_len <- asks bar_length
    let allmsgs = translateMidiGlyphs bar_len (simplifyNoteList evts)
    liftIO $ writeMidiFile path (mkmidi allmsgs)
  where
    mkmidi msgs = let t1 = Track $ msgs |> eot_msg
                  in MidiFile (fromList [controlTrack 500000,t1])      
    
-- Simplified version of Tile & Glyph
data MidiGlyph = MNote Pitch Duration
               | MRest Duration
               | MChord [Pitch] Duration
               | MuGrace MidiGlyph [GraceNote]
               | MaGrace [GraceNote] MidiGlyph
  deriving (Eq,Show)




simplifyNoteList :: NoteList -> Seq (Int,Seq MidiGlyph)
simplifyNoteList = F.foldl simplifyBlock empty . getNoteList 


simplifyBlock :: Seq (Int,Seq MidiGlyph) -> Block -> Seq (Int,Seq MidiGlyph)
simplifyBlock acc (SingleBlock n b) = acc |> (n, simplifyBar b)
simplifyBlock acc (PolyBlock n bse) = acc >< fmap fn bse
  where fn b = (n, simplifyBar b)


-- Assumes well constructed sequence: 
-- unaccented graces always follow a note or chord
-- accented graces always prefix a note or chord
-- If this isn't the case then the graces will be dropped.

simplifyBar :: Bar -> Seq MidiGlyph
simplifyBar (Bar se) = simplify empty (viewl se) id
  where
    -- if the continuation k is a grace builder it will be lost for EmptyL
    simplify acc EmptyL       k = acc
    
    simplify acc (be :< bse)  k = case be of
        Singleton o -> case simplifyGlyph o of
            -- a note or rest - apply the grace continuation to it, and add
            Just e    -> simplify (acc |> k e) (viewl bse) id
            
            -- not representable in Midi - keep the grace continuation around
            Nothing   -> simplify acc (viewl bse) k
        
        Chord se d _ -> simplify (acc |> k (MChord (unseq se) d)) (viewl bse) id
        
        -- grace acts on the preceeding glyph which has already been enqueued
        GraceNotes se UGrace _ -> simplify (ugrace se (viewr acc)) (viewl bse) id
        
        -- grace acts on the next glyph, set up the continuation
        GraceNotes se AGrace _ -> simplify acc (viewl bse) (graceK se)
        
    graceK :: Seq GraceNote -> MidiGlyph -> MidiGlyph     
    graceK se g = MaGrace (unseq se) g
    
    ugrace :: Seq GraceNote -> ViewR MidiGlyph -> Seq MidiGlyph
    ugrace se EmptyR      = empty   -- ill formed, gracenotes get dropped
    ugrace se (bse :> be) = bse |> MuGrace be (unseq se)
              
    simplifyGlyph :: Glyph -> Maybe MidiGlyph
    simplifyGlyph (Note p d _)          = Just $ MNote p d
    simplifyGlyph (Rest _ d _)          = Just $ MRest d
    simplifyGlyph (RhythmicMark _ d _)  = Just $ MRest d
    simplifyGlyph (Mark _ _)            = Nothing


translateMidiGlyphs :: Duration -> Seq (Int,Seq MidiGlyph) -> Seq Message
translateMidiGlyphs bar_len = 
    deltaTransform . orderMessages . unorderedMessages bar_len

unorderedMessages :: Duration -> Seq (Int,Seq MidiGlyph) -> Seq Message
unorderedMessages bar_len = F.foldl fn empty . onsetTransform bar_len
  where 
    fn acc (dt,evts) = acc >< barEvts dt evts

    onsetTransform :: Duration -> Seq (Int,Seq MidiGlyph) 
                      -> Seq (Word32,Seq MidiGlyph)
    onsetTransform bar_len se = fmap fn se
      where fn (bar_num,evts) = (w32_bar_len * fromIntegral bar_num, evts)
            w32_bar_len       = duration bar_len


-- messages ordered, but have global time stamp   
orderMessages :: Seq Message -> Seq Message
orderMessages = foldlOnsetQueue fn empty . buildQueue mfst msnd
  where
    mfst (Message (a,_)) = a
    msnd (Message (_,b)) = b
    
    fn acc (gt, [])     = acc
    fn acc (gt, [x])    = acc |> Message (gt,x)
    fn acc (gt, xs)     = foldl (\a e -> a |> Message (gt,e)) acc xs
 
deltaTransform :: Seq Message -> Seq Message
deltaTransform = snd . F.foldl fn (0,empty)
  where
    fn (t,acc) (Message (gt,e)) = (gt, acc |> (Message (gt-t,e)))  

-- Use direct recursion rather than a recursion scheme 
-- because we need to add multiple elements to the accumulator.

barEvts :: Word32 -> Seq MidiGlyph -> Seq Message
barEvts onset se = step empty onset (viewl se)
  where
    step acc _  EmptyL    = acc
    
    step acc dt (e :< se) = case e of
    
        (MNote p d) -> let (dt',on,off) = noteOnNoteOff dt p d
                       in step (acc |> on |> off) dt' (viewl se)
        
        (MRest d)   -> step acc (dt + duration d) (viewl se)
        
        (MChord ps d) -> let (dt',cse) = chordEvents dt ps d 
                         in step (acc >< cse) dt' (viewl se)

        (MuGrace l gs) -> let (dt',gse) = unaccentedGraceEvents dt l gs 
                          in step (acc >< gse) dt' (viewl se)
        
        (MaGrace gs r) -> let (dt',gse) = accentedGraceEvents dt gs r 
                          in step (acc >< gse) dt' (viewl se)
                              
                      
chordEvents :: Word32 -> [Pitch] -> Duration -> (Word32, Seq Message)                
chordEvents = undefined

unaccentedGraceEvents :: Word32 -> MidiGlyph -> [GraceNote] 
        -> (Word32, Seq Message) 
unaccentedGraceEvents elapsed glyph xs = undefined

accentedGraceEvents :: Word32 -> [GraceNote]  -> MidiGlyph
        -> (Word32, Seq Message) 
accentedGraceEvents elapsed xs glyph = undefined
 


-- note on and note off
noteOnNoteOff :: Word32 -> Pitch -> Duration -> (Word32,Message,Message)
noteOnNoteOff elapsed p d = (dt,on,off)
  where
    dt  = elapsed + duration d
    on  = Message (elapsed, VoiceNoteOn  1 (pitch p) 127)
    off = Message (dt,      VoiceNoteOff 1 (pitch p) 64)


pitch :: Pitch -> Word8
pitch = fromIntegral . (+12) . semitones

duration :: Duration -> Word32
duration d | d == no_duration = 0
           | otherwise        = fn $ ratioElements $ convRational d
  where
    fn (n,1) = n * midi_whole
    fn (1,d) = midi_whole `div` d
    fn (n,d) = (n * midi_whole) `div` d  


  