{-# LANGUAGE TypeSynonymInstances, MultiParamTypeClasses #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  HNotate.ToNoteList
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  TypeSynonymInstances, mptc.
--
-- Render a NoteList to Score format.
--
--------------------------------------------------------------------------------
{-
module ToScore (
    toScore,
    Notate_Sc_Env(..),
    default_score_env
  ) where
-}



module HNotate.ToNoteList where

import HNotate.Bifunctor
import HNotate.Duration
import HNotate.Env
import HNotate.EventInterface
import HNotate.EventList
import HNotate.NoteListDatatypes
import HNotate.OnsetQueue
import HNotate.Pitch


import Control.Applicative hiding (empty)
import Control.Monad.Reader
import qualified Data.Foldable as F

import qualified Data.Map as Map
import Data.Monoid
import Data.Sequence
import Prelude hiding (null, length)

type ConvertM a = Reader Env a 




--------------------------------------------------------------------------------
-- Flattening is done with a tipped accumulator 'Progress'.
-- We don't sort into single or poly blocks on the first traversal.

-- The Measure and its measure number.
type IndexedMeasure = (Int,ScoreMeasure)

-- The tip indexed measure and its duration
type Tip = (IndexedMeasure,Duration)

data Progress = Progress {
    chunk_so_far    :: Seq IndexedMeasure,
    tip_measure     :: Tip
 }



toNoteList :: (Event evt) => EventList evt -> Env -> ScoreNoteList
toNoteList evtlist env = ScNoteList $ blockLine $ 
    runReader (buildFlatRep 1 evtlist) env


makeGlyph :: (Event evt) => evt -> ScoreGlyph
makeGlyph evt = case eventvalues evt of
                  (Just p, d)    -> GlyNote p d
                  (Nothing, d)   -> GlyRest d
                  

-- flatten an Event Tree - the indexed measures won't properly be 
-- in order it the tree has `poly` elements  
buildFlatRep :: (Event evt) 
             => Int 
             -> EventList evt 
             -> ConvertM (Seq IndexedMeasure)
buildFlatRep mc tree = 
    let pzero = Progress mempty ((mc, ScMeasure mempty), mempty) in do
        progress <- flattenEvents (viewl $ getEventList tree) pzero
        return $ seal progress



flattenEvents :: (Event evt) 
              => ViewL (EvtPosition evt) 
              -> Progress 
              -> ConvertM Progress
flattenEvents vl acc = flatstep vl acc
  where
    flatstep EmptyL               acc = return acc
    
    flatstep (Evt e :< sq)        acc = 
        (acc `addToTip` makeGlyph e) >>= flatstep (viewl sq)

    flatstep (StartPar :< sq)     acc = flattenParallel (viewl sq) (acc,mempty)

    flatstep (StartPre :< sq)     acc = flattenPrefix (viewl sq) (acc,mempty)

    flatstep (Poly ts :< sq)      acc = flattenPoly ts sq acc

-- Flatten a parallel block to a chord
flattenParallel :: (Event evt)
                => ViewL (EvtPosition evt)
                -> (Progress, (Seq ScoreGlyph))
                -> ConvertM Progress
flattenParallel sq (se,stk) = flatpar sq (se,stk)
  where
    flatpar (Evt e :< sq)  (se,stk) =
        flatpar (viewl sq) (se,stk |> makeGlyph e)
        
    flatpar (EndPar :< sq) (se,stk) =
        se `addChordM` stk  >>= flattenEvents (viewl sq)

    flatpar _              (se,stk) =
        error "flattenParallel - unterminated Par"
            

-- Flatten a prefix block to grace notes
flattenPrefix :: (Event evt)
              => ViewL (EvtPosition evt)
              -> (Progress, (Seq ScoreGlyph))
              -> ConvertM Progress
flattenPrefix sq (se,stk) = flatpre sq (se,stk)
  where
    flatpre (EndPre :< sq) (se,stk) =
        flattenEvents (viewl sq) (se `addGrace` stk)

    flatpre (Evt e :< sq)  (se,stk) =
        flatpre (viewl sq) (se,stk |> makeGlyph e)

    flatpre _              (se,stk) =
        error "flattenPrefix - unterminated Pre"


                                 
-- Flatten a list of polyphonic 'trees' into the sequence.
-- Note this will result in measures being out of order - we need to
-- do a reconciliation step afterwards.
flattenPoly :: (Event evt)
            => [EventList evt]
            -> Seq (EvtPosition evt)
            -> Progress
            -> ConvertM Progress
flattenPoly ts next acc = let mc = measureCount acc in do 
    acc'    <- F.foldlM (fn mc) acc ts
    flattenEvents (viewl next) acc'
  where
    fn mc acc e = do 
        se <- buildFlatRep mc e
        return $ acc `joinProgress` se
    
   

instance OnsetEvent IndexedMeasure ScoreMeasure where
  onset (i,m) = (i,m)

  
blockLine :: (Seq IndexedMeasure) -> Seq ScoreBlock
blockLine se | null se   = mempty
             | otherwise = collapse $ buildQueue se
  where
    collapse :: OnsetQueue ScoreMeasure -> Seq ScoreBlock
    collapse oq = step mempty (viewH oq)
    
    step se ((i,sm) :>> oq)   = step (se |> mkBlock i sm) (viewH oq)
    step se EmptyQ            = se
    
    mkBlock i [x]   = ScSingleBlock i x
    mkBlock i xs    = ScPolyBlock i (fromList xs)

--------------------------------------------------------------------------------
-- Handling graces, chords and polyphonic lines

pitches = seqMaybes . fmap notePitch
  where 
    notePitch (GlyNote p  _)  = Just p
    notePitch _               = Nothing
    
graces = seqMaybes . fmap notePitchDur
  where 
    notePitchDur (GlyNote p  d) = Just (p,d)
    notePitchDur _              = Nothing  


      
seqMaybes :: Seq (Maybe a) -> Seq a
seqMaybes = rec mempty . viewl
  where
    rec se EmptyL           = se
    rec se (Just a :< sse)  = rec (se |> a) (viewl sse)
    rec se (Nothing :< sse) = rec se (viewl sse)
        

addChordM :: Progress -> Seq (Glyph Pitch Duration) -> ConvertM Progress
addChordM p se 
    | null se     = return p
    | otherwise   = p `addToTip` chord se 
  where 
    chord se = GlyChord (pitches se) (stackDuration se) 
    
    stackDuration :: Seq (Glyph Pitch Duration) -> Duration
    stackDuration se = case viewl se of
        (GlyNote p d) :< _  -> d
        EmptyL              -> error $ "stackDuration" -- quarternote
  
-- graces don't increase the duration so don't use `addToTip`    
addGrace :: Progress -> Seq (Glyph Pitch Duration) -> Progress
addGrace p@(Progress se ((mc, ScMeasure sm),d)) sse 
    | null se     = p
    | otherwise   = Progress se ((mc, ScMeasure (sm |> grace sse)),d) 
  where 
    grace se = GlyGraceNotes $ graces se 

--------------------------------------------------------------------------------

joinProgress :: Progress -> Seq IndexedMeasure -> Progress
joinProgress (Progress sx t) sy = Progress (sx >< sy) t


measureCount :: Progress -> Int 
measureCount (Progress _ ((i,_),_)) = i


advanceMeasure :: Tip -> Duration -> ConvertM Duration
advanceMeasure (_,d) n  = do 
    ml  <- asks measure_length
    if (d+n > ml) then return mempty else return (d+n)
      

-- Add an event (i.e. a glyph to the tip of the current measure.
-- If the measure is full (i.e. its current count == the measure count)
-- then add it to the accumulation of measures.
addToTip :: Progress -> ScoreGlyph -> ConvertM Progress
addToTip (Progress se tip@((i,t),d)) e = 
    fn <$> advanceMeasure tip (glyphDuration e)
  where
    fn d | d == 0    = Progress (se |> (i,t `addGlyph` e)) 
                                ((i+1, ScMeasure mempty),mempty)
         | otherwise = Progress se ((i, t `addGlyph` e),d)

    addGlyph :: ScoreMeasure -> ScoreGlyph -> ScoreMeasure
    addGlyph (ScMeasure se) e = ScMeasure (se |> e)
    
    

seal :: Progress -> Seq IndexedMeasure
seal (Progress se (tip@(i, ScMeasure me),_)) 
    | null me   = se 
    | otherwise = se |> tip
     



