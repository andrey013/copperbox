--------------------------------------------------------------------------------
-- |
-- Module      :  ToSystem
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Render an EventTree as to LilyPond.
--
--------------------------------------------------------------------------------
{-
module ToScore (
    toScore,
    Notate_Sc_Env(..),
    default_score_env
  ) where
-}
module ToScore where

import Duration
import EventInterface
import EventTree
import NotateMonad
import ScoreRepresentation

import Control.Applicative hiding (empty)
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Foldable as F

-- import Data.Maybe
import Data.Monoid
import Data.Sequence
import Prelude hiding (null)



type ProcessM a = NotateM Notate_Sc_State Notate_Sc_Env a

type DSystem    = ScSystem Glyph Duration
type DStrata    = ScStrata Glyph Duration
type DChunk     = ScChunk Glyph Duration
type DMeasure   = ScMeasure Glyph Duration
type DGlyph     = ScGlyph Glyph Duration

type Glyph      = CommonGlyph Duration


data Notate_Sc_State = Notate_Sc_State {
    strata_count        :: Int,
    poly_number        :: Int,
    measure_count       :: Int,
    duration_count      :: Double
  }
  deriving (Show)

data Notate_Sc_Env = Notate_Sc_Env {
    sc_measure_length     :: Double,
    first_bar_onset       :: Double
  }
  deriving (Show)



intial_score_state = Notate_Sc_State {
    strata_count    = 1,
    poly_number    = 1,
    measure_count   = 1,
    duration_count  = 0.0
  }

default_score_env :: Notate_Sc_Env
default_score_env = Notate_Sc_Env {
    sc_measure_length   = 1.0,
    first_bar_onset     = 0.0
  }



--------------------------------------------------------------------------------
-- Flattening is done with a tipped accumulator 'FProgress'
data FProgress = FProgress {
    chunk_so_far :: DChunk,
    tip_measure  :: DMeasure
 }

initialProgress :: Int -> Int  -> FProgress
initialProgress mc voice = FProgress mempty (ScMeasure mc voice mempty)


-- Add an event (i.e. a glyph to the tip of the current measure.
-- If the measure is full (i.e. its current count == the measure count)
-- then add it to the accumulation of measures.
(|%>) :: FProgress
      -> DGlyph
      -> ProcessM FProgress
(FProgress se t) |%> a = fn <$> gets poly_number 
                                  <*> increaseMeasure (glyphDuration a)
  where
    fn v (Just i)  = let se' = se |> (t `addGlyph` a)
                         new_tip = ScMeasure i v mempty 
                     in FProgress se' new_tip
    fn v Nothing   = FProgress se (t `addGlyph` a)

    addGlyph :: DMeasure -> DGlyph -> DMeasure
    addGlyph (ScMeasure i v se) e = ScMeasure i v (se |> e)

seal :: FProgress -> DChunk
seal (FProgress se t@(ScMeasure _ _ tip)) 
    | null tip    = se 
    | otherwise   = se |> t
     

glyphDuration :: DGlyph -> Double
glyphDuration (ScGlyph _ d)  = toDouble d

                 
remainingDuration :: ProcessM Double
remainingDuration = (-) <$> asks sc_measure_length <*> gets duration_count


increaseMeasure :: Double -> ProcessM (Maybe Int)
increaseMeasure d = remainingDuration >>= fn d
  where
    fn new_dur dur_left
        | new_dur >= dur_left = Just    <$> nextMeasure
        | otherwise           = Nothing <$  increaseCurrent new_dur
        
    increaseCurrent d = do 
        dc <- gets duration_count
        modify (\s -> s {duration_count = dc + d})

    nextMeasure = do 
        mc <- gets measure_count
        modify (\s -> s{duration_count=0.0, measure_count=mc+1})
        return $ mc+1

--------------------------------------------------------------------------------
-- Handling graces, chords and polyphonic lines

pitches = seqMaybes . fmap notePitch
  where 
    notePitch (ScGlyph (CmnNote p) _) = Just p
    notePitch _                       = Nothing
    
graces = seqMaybes . fmap notePitchDur
  where 
    notePitchDur (ScGlyph (CmnNote p) d) = Just (p,d)
    notePitchDur _                       = Nothing  


      
seqMaybes :: Seq (Maybe a) -> Seq a
seqMaybes = rec mempty . viewl
  where
    rec se EmptyL           = se
    rec se (Just a :< sse)  = rec (se |> a) (viewl sse)
    rec se (Nothing :< sse) = rec se (viewl sse)
        
                      
addChordM :: FProgress -> Seq DGlyph -> ProcessM FProgress
addChordM p se 
    | null se     = return p
    | otherwise   = p |%> chord se 
  where 
    chord se = ScGlyph (CmnChord (pitches se)) (stackDuration se) 
    
    stackDuration :: Seq DGlyph -> Duration
    stackDuration se = case viewl se of
        ((ScGlyph _ d) :< _)   -> d
        EmptyL                -> error $ "stackDuration" -- quarternote
  
-- graces don't increase the duration so don't use (|%>)    
addGrace :: FProgress -> Seq DGlyph -> FProgress
addGrace p@(FProgress se (ScMeasure i v tip)) sg 
    | null se     = p
    | otherwise   = FProgress se (ScMeasure i v (tip |> grace sg)) 
  where 
    grace se = let gs = graces se 
               in ScGlyph (CmnGraceNotes gs) (sumDuration gs)
                        
sumDuration :: Seq (a,Duration) -> Duration               
sumDuration xs = quarter
    
addPolys :: FProgress -> [DChunk] -> FProgress
addPolys (FProgress se t) xs = FProgress (foldl (><) se xs) t


--------------------------------------------------------------------------------                
  
toScore :: (Event evt) => System evt -> Notate_Sc_Env -> DSystem
toScore sys env = evalNotate (processSystem sys) intial_score_state env


processSystem :: (Event evt) => System evt -> ProcessM DSystem
processSystem p@(System se) = ScSystem <$> F.foldlM fn mempty se
  where
    fn se x = do 
        e  <- flattenTree 1 x
        sc <- incrStrata
        return $ se |> (ScStrata sc e)
        
    incrStrata = do
        sc <- gets strata_count
        modify (\s -> s{strata_count=sc+1})
        return sc 


makeGlyph :: (Event evt) => evt -> ProcessM DGlyph
makeGlyph evt = case eventvalues evt of
                  (Just p, Just d)    -> return $ ScGlyph (CmnNote p) d
                  (Nothing, Just d)   -> return $ ScGlyph CmnRest d
                  (Nothing, Nothing)  -> error "withEvent - to do "
                  


flattenTree :: (Event evt) => Int -> EventTree evt -> ProcessM DChunk
flattenTree meas_count tree = do
    voice    <- gets poly_number
    modify   (\s -> s { measure_count = meas_count })
    progress <- flattenEvents (viewl $ getEventTree tree) 
                              (initialProgress meas_count voice)
    return $ seal progress



flattenEvents :: (Event evt) => ViewL (EvtPosition evt) -> FProgress -> ProcessM FProgress
flattenEvents vl acc = flatstep vl acc
  where
    flatstep EmptyL               acc = return acc


    flatstep (Evt e :< sq)        acc = makeGlyph e >>=  
                                        (acc |%>)   >>= 
                                        flatstep (viewl sq)

    flatstep (StartPar :< sq)     acc = flattenParallel (viewl sq) (acc,mempty)

    flatstep (StartPre :< sq)     acc = flattenPrefix (viewl sq) (acc,mempty)

    flatstep (Poly ts :< sq)      acc = flattenPoly ts sq acc

-- Flatten a parallel block to a chord
flattenParallel :: (Event evt)
                => ViewL (EvtPosition evt)
                -> (FProgress, (Seq DGlyph))
                -> ProcessM FProgress
flattenParallel sq (se,stk) = flatpar sq (se,stk)
  where
    flatpar (EndPar :< sq) (se,stk) =
        se `addChordM` stk  >>= flattenEvents (viewl sq)

    flatpar (Evt e :< sq)  (se,stk) = do
        gly <- makeGlyph e
        flatpar (viewl sq) (se,stk |> gly)

    flatpar _              (se,stk) =
        error "flattenParallel - unterminated Par"
            

-- Flatten a prefix block to grace notes
flattenPrefix :: (Event evt)
              => ViewL (EvtPosition evt)
              -> (FProgress, (Seq DGlyph))
              -> ProcessM FProgress
flattenPrefix sq (se,stk) = flatpre sq (se,stk)
  where
    flatpre (EndPre :< sq) (se,stk) =
        flattenEvents (viewl sq) (se `addGrace` stk)

    flatpre (Evt e :< sq)  (se,stk) = do
        gly <- makeGlyph e
        flatpre (viewl sq) (se,stk |> gly)

    flatpre _              (se,stk) =
        error "flattenPrefix - unterminated Pre"
                                 
-- Flatten a list of polyphonic 'trees' into the sequence.
-- Note this will result in measures being out of order - we need to
-- do a reconciliation step afterwards.
flattenPoly :: (Event evt)
            => [EventTree evt]
            -> Seq (EvtPosition evt)
            -> FProgress
            -> ProcessM FProgress
flattenPoly ts next acc = do 
    mc    <- gets measure_count
    xs    <- mapM (\t -> incrPoly >> flattenTree mc t) ts
    modify (\s -> s { measure_count = mc })
    flattenEvents (viewl next) (acc `addPolys` xs)
  where
    incrPoly = do
        v <- gets poly_number
        modify (\s -> s{poly_number= v+1})    
    
   



