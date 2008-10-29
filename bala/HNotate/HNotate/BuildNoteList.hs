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
import HNotate.MusicRepDatatypes
import HNotate.NotateMonad
import HNotate.NoteListDatatypes
import HNotate.OnsetQueue
import HNotate.PPInstances

import Control.Applicative hiding (empty)
import Control.Monad.Reader
import qualified Data.Foldable as F
import Data.List (unfoldr)
import Data.Maybe (fromMaybe)
import Data.Monoid
import Data.Sequence hiding (reverse)
import Prelude hiding (null)


-- The distance from the 'start' of a sequence of music.
-- Measured in bars and the duration within a bar 
data MetricalSize = MS { 
    bar_count :: Int,
    ib_duration :: Duration     -- 'in-bar' duration 
  }
  deriving (Eq,Show)

-- We only need 0 and + (a Monoid instance) and can get by 
-- without a Num instance
instance Monoid MetricalSize where
  mempty = MS 0 duration_zero
  MS b d `mappend` MS b' d' = MS (b+b') (d+d')     
  
-- After addition the 'in-bar' duration may actually be bigger than a bar!
-- This is an unfortunate problem of not having context for the addition
-- (e.g. getting the bar_length form a reader monad)     
msnormalize :: Duration -> MetricalSize -> MetricalSize
msnormalize bar_len (MS b d) 
    | d < bar_len   = MS b d
    | otherwise     = let (c,v) = d `divModR` bar_len 
                      in MS (b + fromIntegral c) v
                                 

  
data EvtVoiceOverlay = EVO { 
    evo_displacement  :: MetricalSize,
    evo_events        :: Seq Evt
  }
  deriving (Show)   
  
data GlyphVoiceOverlay = GVO { 
    gvo_displacement  :: MetricalSize,
    gvo_glyphs        :: Seq Glyph
  }
  deriving (Show) 




toNoteList :: Monad m => EventList -> NotateT m NoteList
toNoteList evts =
    asks unmetered >>= \unm -> 
    if unm then eventsToNoteListUnmetered evts
           else asks bar_length         >>= \ml -> 
                anacrusisDisplacement   >>= \acis ->
                eventsToNoteList ml acis evts

eventsToNoteListUnmetered :: Monad m => EventList -> NotateT m NoteList
eventsToNoteListUnmetered = eventsToNoteList max_bar_len duration_zero
  where max_bar_len = duration (maxBound::Int) 1
  
  
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
untree bar_len acis evts = 
    worklist (reduceTreeStep bar_len) [initial_vo]
  where
    bar_number  = if (acis == duration_zero) then 1 else 0 
    initial_vo  = EVO (MS bar_number acis) evts 
    
    

reduceTreeStep :: Duration -> EvtVoiceOverlay 
                    -> (GlyphVoiceOverlay, [EvtVoiceOverlay])
reduceTreeStep bar_len (EVO ms se) = mkAnswer $ F.foldl fn (ms,empty,[]) se
  where
    -- Build the final aswer with the initial MS! 
    mkAnswer (_,se,polys) = ((GVO ms se),polys)  
    
    fn (ms,se,polys) (Poly ys) = (ms,se,polys ++ map mkEVO ys)
        where
          -- Any poly voices we find we want them to start at a 'bar start'
          mkEVO (EventList se)  = if ib_duration ms == duration_zero
                                  then EVO ms se
                                  else EVO (leftToBarStart ms) (lspacer <| se)
          lspacer               = Evt $ Spacer (ib_duration ms)

    
    fn (ms,se,polys) (Evt e)   = (moveRightwards bar_len e ms, se |> e, polys)


moveRightwards :: Duration -> Glyph -> MetricalSize -> MetricalSize
moveRightwards bar_len gly ms = 
    msnormalize bar_len $ ms `mappend` (gylphSize bar_len gly)
                 
leftToBarStart :: MetricalSize -> MetricalSize 
leftToBarStart (MS b _) = MS b duration_zero


type RawBar = (Int,Seq Glyph)


partitionGVO :: Duration -> GlyphVoiceOverlay -> Seq RawBar
partitionGVO bar_len (GVO ms se) = 
    finalize $ F.foldl phi (empty,(ms,empty)) se
  where 
    finalize (srb, (ms,sg)) 
        | null sg     = srb
        | otherwise   = srb |> (bar_count ms, sg)
          
    phi (srb,(ms,sg)) e  = case fits e (bar_len - ib_duration ms) of
        Fit a     -> let ms' = moveRightwards bar_len e ms in
                     -- a 'fit' might be an exact fit and leave 
                     -- if so the move right will increased the bar_count 
                     if bar_count ms' /= bar_count ms 
                     then (srb |> (bar_count ms, (sg |> e)), (ms',empty))
                     else (srb, (ms', sg |> e))
                     
        Split l r -> (srb |> (bar_count ms, sg |> l |> Tie),
                        (moveRightwards bar_len r ms, singleton r) )


instance Fits Glyph Duration where
  measure (Note _ d)        = d
  measure (Rest d)          = d
  measure (Spacer d)        = d
  measure (Chord _ d)       = d
  measure e                 = duration_zero
  
  resizeTo (Note p _)   d   = Note p d
  resizeTo (Rest _)     d   = Rest d
  resizeTo (Spacer _)   d   = Spacer d
  resizeTo (Chord se _) d   = Chord se d
  resizeTo e            d   = e

                                                   

gylphSize :: Duration -> Glyph -> MetricalSize
gylphSize bar_len gly = 
    let d = glyphDuration gly; (bn,rest) = d `divModR` bar_len 
    in MS (fromIntegral bn) rest
    
     
                      
                      
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
    
ppListGlyphVoiceOverlay :: [GlyphVoiceOverlay] -> ODoc
ppListGlyphVoiceOverlay = vsep . map ppGlyphVoiceOverlay

ppGlyphVoiceOverlay :: GlyphVoiceOverlay -> ODoc
ppGlyphVoiceOverlay (GVO ms se) = 
  text "bar" <+> int (bar_count ms) <> colon 
             <+> ppDuration (ib_duration ms) <+> finger se 


ppListSeqRawBar :: [Seq RawBar] -> ODoc
ppListSeqRawBar = vsep . map (genFinger ppRawBar)

ppSeqRawBar :: Seq RawBar -> ODoc
ppSeqRawBar = genFinger ppRawBar

ppRawBar :: RawBar -> ODoc
ppRawBar (i,se) = tupled [int i, finger se] 



