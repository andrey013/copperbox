

--------------------------------------------------------------------------------
-- |
-- Module      :  HNotate.BuildNoteList
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined
--
-- Render an Event 'Tree' to a score note list format using
-- recursion schemes.
--------------------------------------------------------------------------------



module HNotate.BuildNoteList where

import HNotate.CommonUtils
import HNotate.DebugWriter
import HNotate.Duration
import HNotate.Env
import HNotate.MusicRepDatatypes
import HNotate.NoteListDatatypes
import HNotate.OnsetQueue
import HNotate.PrettyInstances

import Control.Monad.State
import qualified Data.Foldable as F
import Data.List (unfoldr)
import Data.Maybe (fromMaybe)
import Data.Sequence hiding (reverse)
import Prelude hiding (null)
import Text.PrettyPrint.Leijen hiding (empty)
import qualified Text.PrettyPrint.Leijen as PP

        
        
-- VoiceOverlay - (i,d,se)
-- i - bar count, d - time from start of bar, se - events
type VoiceOverlayA = (Int, Duration, Seq Evt)

type VoiceOverlayB = (Int, Duration, Seq Glyph)

toNoteList :: EventList -> Env -> NoteList
toNoteList evts env = 
    let start = fromMaybe duration_zero (partial_measure env)
    in eventsToNoteList (measure_length env) start evts


eventsToNoteList :: Duration -> Duration -> EventList -> NoteList
eventsToNoteList bar_len partial_start = 
    collapseQueue . rawToQueue 
                  . map (partitionVoB bar_len) 
                  . untree bar_len partial_start    
                  . getEventList


eventsToNoteList_debug :: Duration -> Duration -> EventList 
                       -> DebugWriter NoteList
eventsToNoteList_debug bar_len partial_start = 
    o4 collapseQueue <=< o3 rawToQueue 
                     <=< o2 (map (partitionVoB bar_len)) 
                     <=< o1 (untree bar_len partial_start)    
                     <=< o0 getEventList
  where 
    o0 = (.) return   -- don't generate output
    o1 = genWriteStep "Flattened representation... "    ppListVoiceOverlayB
    o2 = genWriteStep "The flat rep partitioned..."     ppListSeqRawBar
    o3 = genWriteStep "The bars in the onset queue..."  ppOnsetQueue
    o4 = genWriteStep "Finally the note list..."        ppNoteList 
    

        
-- 'untree' is the difficult bit of building a note list
-- it flattens out the 'polyphony'
untree :: Duration -> Duration -> Seq Evt -> [VoiceOverlayB]    
untree bar_len partial_start evts = 
    worklist (reduceTreeStep bar_len) [initial_vo]
  where
    bar_number  = if (partial_start == duration_zero) then 1 else 0 
    initial_vo  = (bar_number, partial_start, evts)     


-- reduceTreeStep produces a 'flat view' of VoiceOverlayA which 
-- will have the same start point:
-- (bar number /bc/ and inner bar displacement /d/)
-- and also returns a (work)list of voice overlays where it 
-- encountered parallelism (aka. polyphony)      
reduceTreeStep :: Duration -> VoiceOverlayA -> (VoiceOverlayB, [VoiceOverlayA])
reduceTreeStep bar_len (bc,d,se) = vals $ F.foldl fn (bc,d,empty,[]) se
  where
    -- Take the initial bc & d to preserve the start point
    vals (_,_,se,xs) = ((bc,d,se),xs)  
    
    fn (bc,d,se,xs) (Poly ys) = 
        let ys' = map (\s -> (bc,0, spacerPrefix d s)) ys in (bc,d,se, xs++ys')
    
    fn (bc,d,se,xs) (Evt e)   = let d' = d + glyphDuration e in 
      case d' >= bar_len of
        True  -> let (b'',d'') = gylphSize bar_len d' in (bc+b'',d'',se |> e, xs)
        False -> (bc,d',se |> e, xs)

    -- Align a further VoiceOverlayA's at the star of a bar
    -- rather than an arbitrary point within the bar.
    -- This is done with a non-printable space and is essential 
    -- for LilyPond and Abc  output.    
    spacerPrefix d (EventList se)  | d > 0     = Evt (Spacer d) <| se
                                   | otherwise = se

      

type RawBar = (Int,Seq Glyph)

type PVobSt = (Int,Duration,Seq Glyph)
            
partitionVoB :: Duration -> VoiceOverlayB -> Seq RawBar
partitionVoB bar_len (bar_num, partial, se) = 
    finalize $ apo step flush (bar_num,partial,se)
  where
    step :: PVobSt -> Maybe (RawBar, PVobSt)
    step se = let (bc,d,sa,sb) = together (general_splitter se) in 
              if null sb then Nothing else Just ((bc,sa),(bc+1,d,sb))
    
    -- flush ignores 'start duration'
    flush :: PVobSt -> Seq RawBar
    flush (bn,_,se) = singleton (bn,se)         
    
    general_splitter (bn,d,se) = genSplit withinBar plusop (bn,d) se
    
    plusop :: (Int,Duration) -> Glyph -> (Int,Duration)
    plusop (bc,d) e = let dn = d + glyphDuration e in case dn >= bar_len of
        True  -> let (bc',d') = gylphSize bar_len dn in (bc+bc',d') 
        False -> (bc,dn)

    withinBar :: (Int,Duration) -> (Int,Duration) -> Bool
    withinBar (bc,_) (bc',_) = bc==bc'       
    
    together :: ((Int,Duration), Seq Glyph, Maybe Glyph, Seq Glyph)
                  -> (Int, Duration, Seq Glyph, Seq Glyph)
    together ((bc,d), sl, Nothing, sr) = (bc,d,sl,sr)
    together ((bc,d), sl, Just e,  sr) = let ed = glyphDuration e in 
        case d + ed == bar_len of
          True  -> (bc, 0, sl |> e, sr)
          False -> let leftd  = bar_len - d
                       rightd = ed - leftd
                       lg     = durationf (const leftd) e
                       rg     = durationf (const rightd) e
                   -- new duration is 0 as the 'right' glyph
                   -- is enqueued and will be consumed later
                   in (bc, 0, sl |> lg |> Tie, rg <| sr)

    -- drop the last element if it contains an empty body...                               
    finalize se = case viewr se of
      EmptyR         -> se        -- not much to do for empty 
      sa :> (bn,ssa) -> if null ssa then sa else sa |> (bn,ssa)
                                                                                      

-- glyphs might be bigger than a bar!
gylphSize :: Duration -> Duration -> (Int, Duration)
gylphSize bar_len drn = reccy 0 drn
  where reccy i drn   | drn > bar_len = (i,drn)
                      | otherwise     = (i+1, drn - bar_len)
                                                      

-- Change the raw bars to Bar and queue them on 
-- bar number. 
rawToQueue :: [Seq RawBar] -> OnsetQueue Bar
rawToQueue = buildQueue fst snd . rebuild
  where 
    rebuild :: [Seq RawBar] -> Seq (Int, Bar)
    rebuild = foldr (><) empty . map (build1 empty)
    
    build1 :: Seq (Int,Bar) -> Seq RawBar -> Seq (Int, Bar)
    build1 = F.foldr (\(i,se) sa -> if null se then sa
                                               else sa |> (i, Bar se))
 
-- Finally linearize the queue - aggregating simultaneous
-- bars into voice overlays (PolyBlock's) 
collapseQueue :: OnsetQueue Bar -> NoteList 
collapseQueue = NoteList . foldlOnsetQueue fn empty
  where    
    fn acc (i, [x]) = acc |> (SingleBlock i x)
    fn acc (i, xs)  = acc |> (PolyBlock i (fromList xs))



--------------------------------------------------------------------------------
-- debugging // pp output
    
    
ppListVoiceOverlayB :: [VoiceOverlayB] -> Doc
ppListVoiceOverlayB = vsep . map ppVoiceOverlayB

ppVoiceOverlayB (bc, d, se) = 
    text "bar" <+> int bc <> colon <+> pretty d <+> finger se 

ppListSeqRawBar :: [Seq RawBar] -> Doc
ppListSeqRawBar = vsep . map (genFinger ppRawBar)

ppSeqRawBar :: Seq RawBar -> Doc
ppSeqRawBar = genFinger ppRawBar

ppRawBar :: RawBar -> Doc
ppRawBar (i,se) = tupled [int i, finger se] 


ppOnsetQueue :: OnsetQueue Bar -> Doc
ppOnsetQueue = foldlOnsetQueue fn PP.empty
  where fn d (i,xs) = d <$> int i <+> text ":+" <+> list (map pretty xs)
  
ppNoteList :: NoteList -> Doc
ppNoteList = pretty

