{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}

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
import HNotate.Document
import HNotate.Duration
import HNotate.Env
import HNotate.Fits
import HNotate.MusicRepDatatypes
import HNotate.NotateMonad
import HNotate.NoteListDatatypes
import HNotate.OnsetQueue
import HNotate.PPInstances
import HNotate.ProcessingTypes

import Control.Applicative hiding (empty)
import Control.Monad.Reader
import qualified Data.Foldable as F
import Data.List (unfoldr)
import Data.Maybe (fromMaybe)
import Data.Sequence hiding (reverse)
import Prelude hiding (null, length)


-- The distance from the 'start' of a sequence of music.
-- Measured in bars and the duration within a bar.
-- If the 'zeroth' bar is an anacrusis, then the duration is 
-- 'how much to fill' and not 'how much filled'. 
data MetricalDisplacement = 
      StdDisp { bar_count :: Int,
                offset_ftl :: Duration -- offset 'from the left'
              }
    | AnaDisp { initial_anacrusis :: Duration }
  deriving (Eq,Show)

displaceRightwards :: MetricalDisplacement -> Duration -> MetricalDisplacement
displaceRightwards (AnaDisp a)    d | d > a     = StdDisp 1 (abs $ a - d)
                                    | otherwise = AnaDisp (a - d)  
displaceRightwards (StdDisp n l)  d = StdDisp n (l + d)

  
-- After addition the offset_from_left may actually be bigger than a bar!
-- This is an unfortunate problem of not having context for the addition
-- (e.g. getting the bar_length form a reader monad)  
-- Note (AnaDisp d) is effectively a negative number, so it stays the same   
mdispNormalize :: Duration -> MetricalDisplacement -> MetricalDisplacement
mdispNormalize bar_len (AnaDisp d) =   (AnaDisp d) 
mdispNormalize bar_len (StdDisp b d) 
    | d < bar_len   = StdDisp b d
    | otherwise     = StdDisp (b + fromIntegral c) v where
                        (c,v) = d `divModR` bar_len 
                     
                                 

  
data EvtVoiceOverlay = EVO { 
    evo_displacement  :: MetricalDisplacement,
    evo_events        :: Seq Evt
  }
  deriving (Show)   
  
data GlyphVoiceOverlay = GVO { 
    gvo_displacement  :: MetricalDisplacement,
    gvo_tiles         :: Seq Tile
  }
  deriving (Show) 




toNoteList :: Monad m => EventList -> NotateT m NoteList
toNoteList evts =
    asks unmetered >>= \unm -> 
    if unm then eventsToNoteListUnmetered evts
           else asks bar_length               >>= \ml -> 
                asks anacrusis_displacement   >>= \asis ->
                eventsToNoteList ml asis evts

eventsToNoteListUnmetered :: Monad m => EventList -> NotateT m NoteList
eventsToNoteListUnmetered = eventsToNoteList max_bar_len duration_zero
  where max_bar_len = makeDuration (maxBound::Int) 1
  
  
eventsToNoteList :: Monad m => Duration -> Duration -> EventList 
                        -> NotateT m NoteList
eventsToNoteList bar_len acis = 
    (o4 . collapseQueue) <=< (o3 . rawToQueue)
                         <=< (o2 . map (partitionGVO bar_len))            
                         <=< (o1 . untree bar_len acis) 
                         <=< (o0 . getEventList)
  where 
    o0 = return
    o1 = document 5 "Flattened representation... "    ppListGlyphVoiceOverlay
    o2 = document 5 "The flat rep partitioned..."     ppListSeqRawBar
    o3 = witness  5 "The bars in the onset queue..."  
    o4 = witness  5 "Finally the note list..."       
    

untree :: Duration -> Duration -> Seq Evt -> [GlyphVoiceOverlay]    
untree bar_len asis evts = worklist (reduceTreeStep bar_len) [initial_vo]
  where
    mdisp  = if asis == duration_zero then StdDisp 1 0 else AnaDisp asis
    initial_vo  = EVO mdisp evts 
    
    

reduceTreeStep :: Duration -> EvtVoiceOverlay 
                    -> (GlyphVoiceOverlay, [EvtVoiceOverlay])
reduceTreeStep bar_len (EVO ms se) = mkAnswer $ F.foldl fn (ms,empty,[]) se
  where
    -- Build the final aswer with the initial MS! 
    mkAnswer (_,se,polys)       = ((GVO ms se),polys)  
    
    fn (ms,se,polys) (Poly ys)  = let ys' = map (EVO ms . getEventList) ys
                                  in (ms,se,polys ++ ys')
    
    fn (ms,se,polys) (Evt e)    = (moveRightwards bar_len e ms, se |> e, polys)


moveRightwards :: Duration -> Tile -> MetricalDisplacement -> MetricalDisplacement
moveRightwards bar_len tile ms = 
    mdispNormalize bar_len $ ms `displaceRightwards` (rhythmicValue tile)
                 

type RawBar = (Int,Seq Tile)

number :: Int -> Seq a -> Seq (Int,a)
number start se = step (viewl se) [start..] where
  step EmptyL    _      = empty
  step (a :< sa) (x:xs) = (x,a) <| step (viewl sa) xs 


partitionGVO :: Duration -> GlyphVoiceOverlay -> Seq RawBar
partitionGVO bar_len (GVO (StdDisp bar_num disp) se) = 
    number bar_num $ asegmentHy (|> tieTile) 0 bar_len (preprocess disp se) 
  where
    preprocess d se | d == duration_zero  = se
                    | otherwise           = (spacerTile d) <| se  

partitionGVO bar_len (GVO (AnaDisp asis) se) = 
    number 0 $ asegmentHy (|> tieTile) asis bar_len se 
    
instance Fits Glyph Duration where
  measure (Note _ d _)            = d
  measure (Rest d _)              = d
  measure (Spacer d _)            = d
  measure (RhythmicMark _ d _)    = d
  measure (Mark _ _)              = duration_zero
  measure BeamStart               = duration_zero
  measure BeamEnd                 = duration_zero
  measure Tie                     = duration_zero
  
  
  resizeTo (Note p _ a)           d = Note p d a
  resizeTo (Rest _ a)             d = Rest d a 
  resizeTo (Spacer _ a)           d = Spacer d a
  resizeTo (RhythmicMark l _ m)   d = RhythmicMark l d m
  resizeTo (Mark l m)             d = Mark l m
  resizeTo BeamStart              d = BeamStart
  resizeTo BeamEnd                d = BeamEnd
  resizeTo Tie                    d = Tie
  
  
instance Fits Tile Duration where
  measure (Singleton e)           = measure e                                                
  measure (Chord se d a)          = d  
  measure (GraceNotes se m a)     = duration_zero
 
  resizeTo (Singleton e)        d = Singleton (resizeTo e d)                                                
  resizeTo (Chord se _ a)       d = Chord se d a  
  resizeTo (GraceNotes se m a)  d = GraceNotes se m a
   
     
                      
                      
-- Change the raw bars to Bar and queue them on 
-- bar number. 
rawToQueue :: [Seq RawBar] -> OnsetQueue Int Bar
rawToQueue = buildQueue fst snd . rebuild
  where 
    rebuild :: [Seq RawBar] -> Seq (Int, Bar)
    rebuild = foldr (><) empty . map (build1 empty)
    
    build1 :: Seq (Int,Bar) -> Seq RawBar -> Seq (Int, Bar)
    build1 = F.foldr (\(i,se) sa -> if null se then sa
                                               else sa |> (i, Bar se))
 
-- Finally linearize the queue - aggregating simultaneous
-- bars into voice overlays (PolyBlock's) 
collapseQueue :: OnsetQueue Int Bar -> NoteList 
collapseQueue = NoteList . foldlOnsetQueue fn empty
  where    
    fn acc (i, [x]) = acc |> (SingleBlock i x)
    fn acc (i, xs)  = acc |> (PolyBlock i (fromList xs))



--------------------------------------------------------------------------------
-- debugging // pp output
    
ppListGlyphVoiceOverlay :: [GlyphVoiceOverlay] -> ODoc
ppListGlyphVoiceOverlay = vsep . map ppGlyphVoiceOverlay

ppGlyphVoiceOverlay :: GlyphVoiceOverlay -> ODoc
ppGlyphVoiceOverlay (GVO (StdDisp n d) se) = 
  text "bar" <+> int n <> colon <+> ppDuration d <+> finger se 

ppGlyphVoiceOverlay (GVO (AnaDisp d) se) = 
  text "bar" <+> int 0 <> colon <+> ppDuration d <+> finger se 
  

ppListSeqRawBar :: [Seq RawBar] -> ODoc
ppListSeqRawBar = vsep . map (genFinger ppRawBar)

ppSeqRawBar :: Seq RawBar -> ODoc
ppSeqRawBar = genFinger ppRawBar

ppRawBar :: RawBar -> ODoc
ppRawBar (i,se) = tupled [int i, finger se] 



