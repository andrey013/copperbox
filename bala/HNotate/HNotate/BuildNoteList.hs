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

import HNotate.Duration
import HNotate.Env
import HNotate.Fits
import HNotate.NoteListDatatypes
import HNotate.ProcessingBase
import HNotate.SequenceUtils

import Control.Monad.Reader
import Data.Sequence hiding (reverse)
import Prelude hiding (null, length)


buildNoteList :: Monad m => EventList -> NotateT m NoteList
buildNoteList evts = asks unmetered >>= fn
  where
    fn True   = return $ buildUnmeteredNoteList evts
    fn _      = do { asis   <- asks anacrusis_displacement 
                   ; barlen <- asks bar_length
                   ; return $ buildMeteredNoteList asis barlen evts }


-- M(usical) Line = (start_time, Seq notes)
type MLine = (Duration,Seq Grouping)

buildMeteredNoteList :: Duration -> Duration -> EventList -> NoteList
buildMeteredNoteList asis barlen = 
    NoteList  . fmap (alignOverlays barlen) 
              . stranspose 
              . fromList 
              . map (segmentToBars asis barlen) 
              . map (spacerPrefix start) . defork start
  where
    start = if asis == duration_zero then duration_zero 
                                     else (barlen - asis)              

buildUnmeteredNoteList :: EventList -> NoteList
buildUnmeteredNoteList = 
    NoteList  . buildBlocks 
              . map (spacerPrefix duration_zero) 
              . defork duration_zero
  where
    -- build1 - for unmetered music we just have a single very long bar
    -- possibly overlayed
    buildBlocks :: [Seq Grouping] -> Seq Block
    buildBlocks []        = empty
    buildBlocks [x]       = singleton $ SingleBlock (Bar x)
    buildBlocks xs        = singleton $ OverlayBlock (fromList $ map Bar xs)
    

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

spacerPrefix :: Duration -> MLine -> Seq Grouping
spacerPrefix asis (d,se) | d == 0 || d == asis  = se
                         | otherwise            = spacerSgl d <| se

segmentToBars :: Duration -> Duration -> Seq Grouping -> Seq Bar
segmentToBars asis barlen = fmap Bar . anasegment True asis barlen

alignOverlays :: Duration -> Seq Bar -> Block
alignOverlays barlen = build . viewl . sfilter (not . emptyBar) where
    emptyBar (Bar se) = null se
    
    -- an empty bar is malformed but we turn it into a spacer bar
    build EmptyL                = let sp = spacerSgl barlen
                                  in SingleBlock (Bar (singleton sp))
    build (a :< sa) | null sa   = SingleBlock a
                    | otherwise = OverlayBlock (a <| sa) 
  

