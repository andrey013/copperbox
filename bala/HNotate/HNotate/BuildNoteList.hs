{-# OPTIONS -Wall #-}


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
-- Render an Event 'Tree' to a score note list.
--
--------------------------------------------------------------------------------



module HNotate.BuildNoteList where

import HNotate.BeamTrafo (beam)
import HNotate.Duration
import HNotate.Env
import HNotate.Fits
import HNotate.MusicRepDatatypes
import HNotate.NoteListDatatypes
import HNotate.ProcessingBase
import qualified HNotate.SequenceExtras as S

import Control.Applicative ( (<$>), (<*>) )
import Control.Monad.Reader
import Data.Sequence hiding (reverse)
import Prelude hiding (null, length)


buildNoteList :: Monad m => EventList -> NotateT m NoteList
buildNoteList evts = 
    fn <$> asks unmetered  <*> asks anacrusis_displacement <*> asks bar_length
                           <*> asks meter_pattern 
  where
    fn True _    _  mp  = buildUnmeteredNoteList mp evts
    fn _    asis bl mp  = buildMeteredNoteList asis bl mp evts


-- M(usical) Line = (start_time, Seq notes)
type MLine = (Duration,Seq Element)

buildMeteredNoteList :: 
    Duration -> Duration -> MeterPattern -> EventList -> NoteList
buildMeteredNoteList asis barlen mp = 
    NoteList  . fmap (alignOverlays barlen) 
              . S.transpose 
              . fromList 
              . map (segmentToBars asis barlen mp) 
              . map (spacerPrefix start) . defork start
  where
    start = if asis == duration_zero then duration_zero 
                                     else (barlen - asis)              

buildUnmeteredNoteList :: MeterPattern -> EventList -> NoteList
buildUnmeteredNoteList mp = 
    NoteList  . buildBlocks 
              . map (spacerPrefix duration_zero) 
              . defork duration_zero
  where
    -- build1 - for unmetered music we just have a single very long bar
    -- possibly overlayed
    buildBlocks :: [Seq Element] -> Seq Block
    buildBlocks []        = empty
    buildBlocks [x]       = singleton $ SingleBlock (makeBar x)
    buildBlocks xs        = singleton $ OverlayBlock (fromList $ map makeBar xs)
    
    makeBar :: Seq Element -> Bar 
    makeBar = Bar . beam (meterPatternDurations mp)

-- defork - turn the event list (which is really a tree) into genuinely
-- linear sequences, paired with their start time (onset time).          
-- Use two accumulators one for the 'current line' the other for forks
-- plus pass the onset time as state.
    
defork :: Duration -> EventList -> [MLine]
defork start (EventList sa) = step start (start,empty) [] (viewl sa) 
  where
    step :: Duration -> MLine -> [MLine] -> ViewL Event -> [MLine]
    step _ z zs EmptyL = z:zs
    
    step o z zs (SingleE e :< se) = 
        step (o + rhythmicValue e) (z `snoc` (o,e)) zs (viewl se)
        
    step o z zs (OverlayE xs :< se)     = let zs' = concat $ fmap (defork o) xs  
        in step o z (zs++zs') (viewl se)
    
    
    -- if the initial sequence is empty we want to time stamp it
    -- with the time of the first event
    snoc (i,se) (j,e) | null se   = (j, singleton e)  
                      | otherwise = (i,se |> e)

spacerPrefix :: Duration -> MLine -> Seq Element
spacerPrefix asis (d,se) | d == 0 || d == asis  = se
                         | otherwise            = spacerSgl d <| se

segmentToBars :: Duration -> Duration -> MeterPattern -> Seq Element -> Seq Bar
segmentToBars asis barlen mp = fmap makeBar . anasegment True asis barlen
  where
    makeBar = Bar . beam (meterPatternDurations mp)

alignOverlays :: Duration -> Seq Bar -> Block
alignOverlays barlen = build . viewl . S.filter (not . emptyBar) where
    emptyBar (Bar se) = null se
    
    -- an empty bar is malformed but we turn it into a spacer bar
    build EmptyL                = let sp = spacerSgl barlen
                                  in SingleBlock (Bar (singleton $ Singleton sp))
    build (a :< sa) | null sa   = SingleBlock a
                    | otherwise = OverlayBlock (a <| sa) 
  

