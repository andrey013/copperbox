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
import HNotate.Duration
import HNotate.Env
import HNotate.MiniMidi
import HNotate.NoteListDatatypes hiding (chord)
import HNotate.OnsetQueue
import HNotate.Pitch
import HNotate.ProcessingTypes

import Control.Applicative hiding (empty)
import Control.Monad.Reader

import qualified Data.Foldable as F
import Data.List (sort)
import Data.Sequence
import qualified Data.Traversable as T
import Data.Word



midiOut :: FilePath -> [NoteList] -> NotateT IO ()
midiOut path ess = do
    mss <- mapM midiMessages ess
    rm  <- asks midi_rendering
    let ts = render rm mss
    midi_ast <- mkmidi ts
    liftIO $ writeMidiFile path midi_ast
  where
    mkmidi trks = tempoTrack >>= \tt ->
                  return $ MidiFile ( tt <| trks )
    
    render Midi_Parallel            = parallelTracks  
    render (Midi_Sequential delay)  = sequentialTracks (fromIntegral delay) 
                  

tempoTrack :: Monad m => NotateT m Track
tempoTrack = (\t -> controlTrack ((fromIntegral t * 500000) `div` 60))
    <$> asks tempo

parallelTracks :: [Seq Message] -> Seq Track
parallelTracks mss = foldr fn empty mss
  where fn msgs se = (Track $ msgs |> eot_msg) <| se
    
sequentialTracks :: Word32 -> [Seq Message] -> Seq Track
sequentialTracks delay mss = 
    singleton $ Track $ foldr fn (empty |> eot_msg) (fmap delayfirst mss)
  where 
    fn msgs se = msgs >< se
    
    delayfirst :: Seq Message -> Seq Message
    delayfirst se = case viewl se of
        Message (dt,evt) :< sa  -> (Message (dt+delay,evt)) <| sa
        EmptyL                  -> empty
  
  
         
midiMessages :: NoteList -> NotateT IO (Seq Message)
midiMessages evts = translateMidiAtoms (simplifyNoteList evts)
        
    
-- Simplified version of Grouping & Atom -- avoid nested graces
type Chan = Word8 


data MidiGrouping = MAtom Chan MidiAtom
                  | MuGrace Chan MidiAtom [GraceNote]
                  | MaGrace Chan [GraceNote] MidiAtom
  deriving (Eq,Show)

-- We have problems with ties. Joining notes split across overlays 
-- will actually be quite difficult with the current implementation...  
--                 | MTie 

data MidiAtom = MNote Pitch Duration
               | MRest Duration
               | MChord [Pitch] Duration              
  deriving (Eq,Show)



simplifyNoteList :: NoteList -> Seq (Int,Seq MidiGrouping)
simplifyNoteList = F.foldl simplifyBlock empty . getNoteList 


simplifyBlock :: Seq (Int,Seq MidiGrouping) -> Block -> Seq (Int,Seq MidiGrouping)
simplifyBlock acc (SingleBlock n b) = acc |> (n, simplifyBar 1 b)
simplifyBlock acc (PolyBlock n bse) = acc >< fmap fn (szipl bse [1..])
  where fn (b,ch) = (n, simplifyBar ch b)

szipl :: Seq a -> [b] -> Seq (a,b)
szipl se xs = step empty (viewl se) xs
  where
    step acc EmptyL     _       = acc
    step acc _          []      = acc
    step acc (a :< sa)  (b:bs)  = step (acc |> (a,b)) (viewl sa) bs    


-- Assumes well constructed sequence: 
-- unaccented graces always follow a note or chord
-- accented graces always prefix a note or chord
-- If this isn't the case then the graces will be dropped.

simplifyBar :: Word8 -> Bar -> Seq MidiGrouping
simplifyBar ch (Bar sg) = simplify empty (viewl sg) kGrouping
  where
    -- not quite identity as it wraps the Atom as a Grouping 
    kGrouping :: MidiAtom -> MidiGrouping 
    kGrouping = MAtom ch . id
    
    -- if the continuation k is a grace builder it will be lost for EmptyL
    simplify acc EmptyL       _ = acc
    
    simplify acc (be :< bse)  k = case be of
        Singleton o -> case simplifyAtom o of
            -- a note or rest - apply the grace continuation to it, and add
            Just e    -> simplify (acc |> k e) (viewl bse) kGrouping
            
            -- not representable in Midi - keep the grace continuation around
            Nothing   -> simplify acc (viewl bse) k
        
        Chord se d _ -> let chord = k (MChord (unseq se) d)
                        in simplify (acc |> chord) (viewl bse) kGrouping
        
        -- grace acts on the preceeding Atom which has already been enqueued
        GraceNotes se UGrace _ -> let se' = ugrace se (viewr acc)
                                  in simplify se' (viewl bse) kGrouping
        
        -- grace acts on the next Atom, set up the continuation
        GraceNotes se AGrace _ -> simplify acc (viewl bse) (graceK se)
        
        Nplet _ ud se _ -> let se' = fmap (\p -> kGrouping $ MNote p ud) se
                           in simplify (acc >< se') (viewl bse) k  
        
    graceK :: Seq GraceNote -> MidiAtom -> MidiGrouping     
    graceK se g = MaGrace ch (unseq se) g
    
    ugrace :: Seq GraceNote -> ViewR MidiGrouping -> Seq MidiGrouping
    ugrace _  EmptyR                = empty   -- ill formed, gracenotes get dropped
    ugrace se (bse :> (MAtom _ e))  = bse |> MuGrace ch e (unseq se)
    ugrace _  (bse :> be)           = bse |> be -- drop gracenotes - ideally we should coalesce them 
              
    simplifyAtom :: Atom -> Maybe MidiAtom
    simplifyAtom (Note p d _)          = Just $ MNote p d
    simplifyAtom (Rest d _)            = Just $ MRest d
    simplifyAtom (Spacer d _)          = Just $ MRest d
    simplifyAtom (RhythmicMark _ d _)  = Just $ MRest d
    simplifyAtom (Mark _ _)            = Nothing
    simplifyAtom BeamStart             = Nothing
    simplifyAtom BeamEnd               = Nothing
    simplifyAtom Tie                   = Nothing

translateMidiAtoms :: Monad m => Seq (Int,Seq MidiGrouping) -> NotateT m (Seq Message)
translateMidiAtoms = 
    unorderedMessages >=> return . deltaTransform . orderMessages 

{-
-- bars are not in an amenable 'shape' for a simple mergeTies.
-- They would need an stranspose first  
mergeTies :: Monad m => Seq (Int,Seq MidiGrouping) -> NotateT m (Seq (Int,Seq MidiGrouping))
mergeTies = error . show 
-}

unorderedMessages :: Monad m => 
    Seq (Int,Seq MidiGrouping) -> NotateT m (Seq Message)
unorderedMessages = (return . F.foldl fn empty) <=< onsetTransform
  where 
    fn acc (dt,evts) = acc >< barEvts dt evts

    onsetTransform :: Monad m => 
        Seq (Int,Seq MidiGrouping) -> NotateT m (Seq (Word32,Seq MidiGrouping))
    onsetTransform se = T.mapM scale se
      where scale (bar_num,evts) = scaleBarOnset bar_num >>= \dt -> 
                                   return (dt, evts)

scaleBarOnset :: Monad m => Int -> NotateT m Word32
scaleBarOnset bar_num = fn <$> asks bar_length  <*> asks anacrusis_displacement
  where
    fn b asis 
        | asis == duration_zero = (duration b) * (fromIntegral bar_num - 1)
        | otherwise             = (duration b) * (fromIntegral bar_num)

-- messages ordered, but have global time stamp   
orderMessages :: Seq Message -> Seq Message
orderMessages = foldlOnsetQueue fn empty . buildQueue mfst msnd
  where
    mfst (Message (a,_)) = a
    msnd (Message (_,b)) = b
    
    fn acc (_,  [])     = acc
    fn acc (gt, [x])    = acc |> Message (gt,x)
    fn acc (gt, xs)     = foldl (\a e -> a |> Message (gt,e)) acc (sort xs)
 
deltaTransform :: Seq Message -> Seq Message
deltaTransform = snd . F.foldl fn (0,empty)
  where
    fn (t,acc) (Message (gt,e)) = (gt, acc |> (Message (gt-t,e)))  


barEvts :: Word32 -> Seq MidiGrouping -> Seq Message
barEvts onset sg = step empty onset (viewl sg)
  where
    step acc _  EmptyL    = acc
    
    step acc dt (e :< se) = case e of
        MAtom ch gly    ->  let (dt', gse) = atomEvents ch dt gly
                            in step (acc >< gse) dt' (viewl se)

        MuGrace ch l gs ->  let (dt',gse) = unaccentedGraceEvents ch dt l gs 
                            in step (acc >< gse) dt' (viewl se)
        
        MaGrace ch gs r ->  let (dt',gse) = accentedGraceEvents ch dt gs r 
                            in step (acc >< gse) dt' (viewl se)
                              
 
-- gracenote
unaccentedGraceEvents :: Word8 -> Word32 -> MidiAtom -> [GraceNote] 
        -> (Word32, Seq Message) 
unaccentedGraceEvents ch elapsed gly xs = 
    let atom_len = rhythmicValue gly; grace_len =  graceLength xs in
    if (atom_len >= grace_len)     
    then let gly'       = modifyDuration gly (atom_len - grace_len)
             (dt,se)    = atomEvents ch elapsed gly'
             (dt',se')  = graceEvents ch dt xs
         in (dt', se >< se')
    else atomEvents ch elapsed gly -- graces too large drop them all
         


accentedGraceEvents :: Word8 -> Word32 -> [GraceNote]  -> MidiAtom
        -> (Word32, Seq Message) 
accentedGraceEvents ch elapsed xs gly = 
    let atom_len = rhythmicValue gly; grace_len =  graceLength xs in
    if (atom_len >= grace_len)     
    then let gly'       = modifyDuration gly (atom_len - grace_len)
             (dt, se)   = graceEvents ch elapsed xs
             (dt',se')  = atomEvents ch dt gly'             
         in (dt', se >< se')
    else atomEvents ch elapsed gly -- graces too large drop them all
    
    

graceLength :: [GraceNote] -> Duration
graceLength = foldr ((+) `onl` snd) duration_zero  


instance RhythmicValue MidiAtom where
  rhythmicValue (MNote _ d)       = d
  rhythmicValue (MRest d)         = d
  rhythmicValue (MChord _ d)      = d
  
  modifyDuration (MNote p _)    d = MNote p d
  modifyDuration (MRest _)      d = MRest d
  modifyDuration (MChord ps _)  d = MChord ps d
  
instance RhythmicValue MidiGrouping where
  rhythmicValue (MuGrace _ gly _)    = rhythmicValue gly
  rhythmicValue (MaGrace _ _ gly)    = rhythmicValue gly
  rhythmicValue (MAtom _ gly)        = rhythmicValue gly
  
  modifyDuration (MuGrace ch gly xs) d = MuGrace ch (modifyDuration gly d) xs
  modifyDuration (MaGrace ch xs gly) d = MaGrace ch xs (modifyDuration gly d)
  modifyDuration (MAtom ch gly)      d = MAtom ch (modifyDuration gly d)


atomEvents :: Word8 ->  Word32 -> MidiAtom -> (Word32, Seq Message) 
atomEvents ch elapsed (MNote p d)   = (dt, empty |> on |> off) 
  where (dt,on,off)  = noteOnNoteOff ch elapsed p d

atomEvents _  elapsed (MRest d)     = (elapsed + duration d, empty)

atomEvents ch elapsed (MChord ps d) = (dt, ons >< offs) where
    dt            = elapsed + duration d
    (ons,offs)    = foldr fn (empty,empty) ps
    fn p (sa,sb)  = let (_,on,off) = noteOnNoteOff ch elapsed p d 
                    in (on <| sa, off <| sb)
                          

graceEvents :: Word8 ->  Word32 -> [GraceNote] -> (Word32, Seq Message) 
graceEvents ch elapsed = foldl fn (elapsed,empty)
  where
    fn (dt,se) (p,d) = let (dt',on,off) = noteOnNoteOff ch dt p d 
                       in (dt', se |> on |> off)
    
      
-- note on and note off
noteOnNoteOff :: Word8 -> Word32 -> Pitch -> Duration -> (Word32,Message,Message)
noteOnNoteOff ch elapsed p d = (dt,on,off)
  where
    dt  = elapsed + duration d
    on  = Message (elapsed, VoiceNoteOn  ch (pitch p) 127)
    off = Message (dt,      VoiceNoteOff ch (pitch p) 64)


pitch :: Pitch -> Word8
pitch = fromIntegral . (+12) . semitones

duration :: Duration -> Word32
duration drn | drn == no_duration = 0
             | otherwise          = fn $ ratioElements $ convRational drn
  where
    fn (n,1) = n * midi_whole
    fn (1,d) = midi_whole `div` d
    fn (n,d) = (n * midi_whole) `div` d  


  