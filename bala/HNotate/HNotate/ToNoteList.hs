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


import HNotate.Duration
import HNotate.Env
import HNotate.MusicRepDatatypes
import HNotate.NoteList
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





toNoteList :: EventList -> Env -> ScoreNoteList
toNoteList evtlist env = ScNoteList $ blockLine $ 
    runReader (buildFlatRep 1 evtlist) env


                  

-- flatten an Event Tree - the indexed measures won't properly be 
-- in order it the tree has `poly` elements  
buildFlatRep :: Int 
             -> EventList 
             -> ConvertM (Seq IndexedMeasure)
buildFlatRep mc tree = 
    let pzero = Progress mempty ((mc, ScMeasure mempty), mempty) in do
        progress <- flattenEvents (viewl $ getEventSeq tree) pzero
        return $ seal progress



flattenEvents :: ViewL Evt 
              -> Progress 
              -> ConvertM Progress
flattenEvents vl acc = flatstep vl acc
  where
    flatstep EmptyL               acc = return acc
    
    flatstep (Evt e :< sq)        acc = 
        (acc `addToTip` e) >>= flatstep (viewl sq)

    flatstep (Poly ts :< sq)      acc = flattenPoly ts sq acc


                                 
-- Flatten a list of polyphonic 'trees' into the sequence.
-- Note this will result in measures being out of order - we need to
-- do a reconciliation step afterwards.
flattenPoly :: [EventList]
            -> Seq Evt
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



joinProgress :: Progress -> Seq IndexedMeasure -> Progress
joinProgress (Progress sx t) sy = Progress (sx >< sy) t


measureCount :: Progress -> Int 
measureCount (Progress _ ((i,_),_)) = i


advanceMeasure :: Tip -> Duration -> ConvertM Duration
advanceMeasure (_,d) n  = do 
    ml  <- asks measure_length
    cza <- asks cadenza  
    if (d+n > ml && cza == False) then return mempty else return (d+n)
      

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
     




bracketMeasure :: MeterPattern -> Seq ScoreGlyph -> (Seq ScoreGlyph, Seq ScoreGlyph)
bracketMeasure mp se = let cs = countings mp in
    step cs se mempty 
  where
    step []     se acc = (acc,se)
    step (i:is) se acc = let (bkt,se') = bracket1 i (viewl se) 
                         in step is se' (acc >< bkt)

countings :: MeterPattern -> [Duration]
countings (xs,s) = map (\i -> fromIntegral i * s) xs
                         
-- bracket a the front of the sequence to the length of the given duration
-- return the bracketed front, plus the rest of the sequence
bracket1 :: Duration -> ViewL ScoreGlyph -> (Seq ScoreGlyph, Seq ScoreGlyph)
bracket1 d EmptyL     = (mempty,mempty)
bracket1 d (e :< se)  = step (d - glyphDuration e) (viewl se) (singleton e)
  where
    step d EmptyL    acc = (acc,mempty)
    step d (e :< se) acc 
        | d <= 0            = (acc, e  <| se) -- leave e on the pending seq
        | otherwise         = step (d - glyphDuration e) (viewl se) (acc |> e)                   


beam :: ViewL ScoreGlyph -> Seq ScoreGlyph
beam EmptyL     = mempty
beam (x :< se)  = step x (viewl se) (mempty,mempty)
  where 
    step a EmptyL    (bstk,acc)  = let (bstk', acc') = cat3 a bstk acc
                                   in popBeamers bstk' acc'
    step a (e :< se) (bstk,acc)  = step e (viewl se) (cat3 a bstk acc)
    
    
    cat3 a beamers cca
        | glyphDuration a < quarter = (beamers |> x, cca)
        | otherwise                 = error $ "cat3"
                                  
    popBeamers be se  = case viewl be of
        EmptyL      -> se
        (e :< rest) -> if (null rest) 
                       then se |> e
                       else se >< ((SgBeamStart <| be) |> SgBeamEnd) 
  
  