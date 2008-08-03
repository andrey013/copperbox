{-# LANGUAGE MultiParamTypeClasses #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Perform.Score.ToScore
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

module Bala.Perform.Score.ToScore (
  toScore,
  Perform_Sc_Env(..), 
  default_score_env
  ) where

import Bala.Perform.Base.Datatypes
import Bala.Perform.Base.EventTree
import Bala.Perform.Base.PerformMonad
import Bala.Perform.Base.Class
import Bala.Perform.Score.Datatypes

import Control.Applicative hiding (empty)
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Foldable as F
import qualified Data.IntMap as IM
import Data.Monoid
import Data.Sequence hiding (reverse)
import Prelude hiding (null)


type ProcessM a = PerformM Perform_Sc_State Perform_Sc_Env a


  
  
data Perform_Sc_State = Perform_Sc_State { 
    tagcounter          :: Int,
    refcounter          :: Int,
    measure_count       :: Int,
    duration_count      :: Double
  }  
  deriving (Show)

data Perform_Sc_Env = Perform_Sc_Env { 
    sc_measure_length     :: Double,
    first_bar_onset       :: Double
  }  
  deriving (Show)
  
-- 'tipped' accumulator - a Seq of bars produced plus the 'tip' the current bar
-- (plus a dictionary of parts for polypohonic elements) 
data TA = TA {
    ta_poly_elts            :: ScPolyRefs, 
    ta_accumulated_measures :: Seq (ScMeasure),
    ta_tip_measure          :: ScMeasure 
  }

emptyTA = TA mempty mempty (ScMeasure 0 mempty mempty)

atMeasure (TA r sm (ScMeasure _ si tip)) i = TA r sm (ScMeasure i si tip)

spaceTip :: TA -> Double -> TA
spaceTip ta@(TA r sm (ScMeasure i si _)) dur 
    | dur == 0  = ta
    | otherwise = let spacer = ScSpacer (fromDouble dur) 
                  in TA r sm (ScMeasure i si (mempty |> spacer))

addRefToTip :: Int -> TA -> TA
addRefToTip x (TA r sm (ScMeasure i si sg)) = 
    TA r sm (ScMeasure i (si |> x) sg)


intial_score_state = Perform_Sc_State { 
    tagcounter      = 1,
    refcounter      = 1,
    measure_count   = 1,
    duration_count  = 0.0
  } 

default_score_env :: Perform_Sc_Env
default_score_env = Perform_Sc_Env { 
    sc_measure_length   = 1.0,   
    first_bar_onset     = 0.0 
  }


withGets :: (Perform_Sc_State -> a) -> (a -> ProcessM b) -> ProcessM b 
withGets lbl f = do
  i <- gets lbl
  f i  

{- 
withNewTag :: (ScTag -> ProcessM a) -> ProcessM a    
withNewTag f = withGets tagcounter      $ \i -> 
    modify (\s -> s {tagcounter = i+1}) >> 
    f (ScTag i) 
-}      
    
withNewRefId :: (Int -> ProcessM a) -> ProcessM a    
withNewRefId f = withGets refcounter    $ \i -> 
    modify (\s -> s {refcounter = i+1}) >>
    f i


remainingDuration :: ProcessM Double
remainingDuration = (-) <$> asks sc_measure_length <*> gets duration_count

        
increaseMeasure :: Double -> ProcessM (Maybe Int) 
increaseMeasure d = remainingDuration >>= fn d 
  where
    fn d duration_left
        | d >= duration_left  = withGets measure_count $ \mc -> do
                modify (\s -> s {duration_count = 0.0, measure_count = mc+1})
                return $ Just (mc + 1) 
                
                
        | otherwise           = withGets duration_count $ \dc ->
                modify (\s -> s {duration_count = dc + d}) >> return Nothing
              


-- Add an event (i.e. a glyph to the tip of the current measure.
-- If the measure is full (i.e. its current count == the measure count)
-- then add it to the accumulation of measures.
(|%>) :: TA 
      -> ScGlyph 
      -> ProcessM TA
tippedacc |%> a = fn <$> increaseMeasure (glyphDuration a)
  where
    fn (Just mc)  = appendMeasure tippedacc a mc 
    fn Nothing    = tippedacc `suffixTip` a
     

-- Concatenate the measure onto the sequence of measures 
-- and reset the tip of the TA
appendMeasure :: TA -> ScGlyph -> Int -> TA 
appendMeasure (TA refs sm (ScMeasure i si se)) a mc  = 
    let m   = ScMeasure i si (se |> a) 
        m0  = ScMeasure (i+1) mempty mempty 
    in TA refs (sm |> m) m0     
    
appendRefs :: TA -> ScPolyRefs ->  TA   
appendRefs (TA refs sm se) r2 = 
    TA (refs `mappend` r2) sm se

-- Add a glyph to the the tip of the TA
suffixTip :: TA -> ScGlyph -> TA
suffixTip (TA refs sm (ScMeasure i xs se)) e = 
    TA refs sm $ ScMeasure i xs (se |> e)


 


glyphDuration :: ScGlyph -> Double
glyphDuration (ScNote  _ d)             = toDouble d
glyphDuration (ScRest  d)               = toDouble d
glyphDuration (ScSpacer  d)             = toDouble d
glyphDuration (ScChord _ d)             = toDouble d
glyphDuration (ScGraceNotes xs)         = 0.0         
        


{-

-- Hmm, I can't remember why I thought you might want to use this rather than
-- finalRest
    
-- extend the final note in the TA    
finalExt :: ScoreDuration dur 
         => TA 
         -> ProcessM (ScPartRefs, Seq (ScPoly))
finalExt (TA refs sb (ScMeasure i se)) = case viewr se of
    EmptyR     -> return (refs,sb)
    (se' :> e) -> (fn se') <$> remext e
  where
    fn s e = (refs, sb |> (ScPolyM $ ScMeasure i (s |> e)))

-- extend the event with the 'remainder duration' of the bar
remext :: ScoreDuration dur => ScGlyph -> ProcessM (ScGlyph)
remext e = fn <$> remainingDuration
  where
    fn duration_left = changeDuration (glyphDuration e + duration_left) e
     
    -- (grace notes shouldn't be at the end)
    changeDuration :: ScoreDuration dur 
                   => Double -> ScGlyph -> ScGlyph
    changeDuration d (ScNote p _)           = ScNote p (fromDouble d)
    changeDuration d (ScRest _)             = ScRest (fromDouble d)
    changeDuration d (ScGroup ScChord xs)   = 
        let xs' = map (changeDuration d) xs in ScGroup ScChord xs'
    changeDuration d g                      = g

-}        
    
-- add a spacer to finalize the TA
finalSpacer :: TA 
            -> ProcessM (ScPolyRefs, Seq (ScMeasure))
finalSpacer (TA refs sb m@(ScMeasure i xs se)) = case viewr se of
    
    EmptyR -> if null xs then return (refs,sb) else return (refs,sb |> m) 
    _      -> fn <$> remrest
  where
    fn r = (refs, sb |> (ScMeasure i xs (se |> r))) 

    -- make a rest with the 'remainder duration' of the bar
    remrest :: ProcessM ScGlyph
    remrest = (ScSpacer . fromDouble) <$> remainingDuration

  



    
-- mkRef :: ProcessM ScGlyph
-- mkRef = ScRef <$> newTag 



withEvent :: (Perform evt) 
          => evt 
          -> (ScGlyph -> ProcessM a) 
          -> ProcessM a
withEvent evt f = makesEvent evt >>= f
  where
    makesEvent evt = case eventvalues evt of
                        (Just p, Just d)    -> return $ ScNote p d
                        (Nothing, Just d)   -> return $ ScRest d
                        (Nothing, Nothing)  -> error "withEvent - to do "

-- All notes should have the same duration - is a check in order?

suffixChord :: TA 
            -> [ScGlyph] 
            -> ProcessM (TA)
suffixChord se []  = return $ se
suffixChord se stk = error "suffixChord" -- se |%> (scChord $ reverse stk) 

suffixGrace :: TA 
            -> [ScGlyph] 
            -> ProcessM (TA)
suffixGrace se []  = return $ se
suffixGrace se stk = error "suffixGrace" -- se |%> (scGrace $ reverse stk) 


    
flattenStep :: (Perform evt) 
            => ViewL (EvtPosition evt)
            -> TA  
            -> ProcessM TA
flattenStep sq acc = flatstep sq acc
  where
    flatstep EmptyL               acc = return acc

    flatstep (Evt e :< sq)        acc = withEvent e $ \ez -> 
                                        acc |%> ez >>= flatstep (viewl sq)

    flatstep (StartPar :< sq)     acc = flattenParallel (viewl sq) (acc,[]) 
    
    flatstep (StartPre :< sq)     acc = flattenPrefix (viewl sq) (acc,[])
            
    flatstep (Poly ts :< sq)      acc = flattenPoly ts sq acc


-- Flatten a parallel block to a chord
flattenParallel :: (Perform evt) 
                => ViewL (EvtPosition evt)
                -> (TA, [ScGlyph])
                -> ProcessM TA                       
flattenParallel sq (se,stk) = flatpar sq (se,stk)
  where
    flatpar (EndPar :< sq) (se,stk) = 
        se `suffixChord` stk >>= flattenStep (viewl sq)

    flatpar (Evt e :< sq)  (se,stk) = withEvent e $ \ez ->
        flatpar (viewl sq) (se, ez:stk)

    flatpar _              (se,stk) = 
        error "flattenParallel - unterminated Par"


-- Flatten a prefix block to grace notes    
flattenPrefix :: (Perform evt) 
              => ViewL (EvtPosition evt)
              -> (TA, [ScGlyph])
              -> ProcessM TA 
flattenPrefix sq (se,stk) = flatpre sq (se,stk)
  where  
    flatpre (EndPre :< sq) (se,stk) = do
        se `suffixGrace` stk >>= flattenStep (viewl sq)

    flatpre (Evt e :< sq)  (se,stk) = withEvent e $ \ez ->
        flatpre (viewl sq) (se, (ez:stk))
                            
    flatpre _              (se,stk) = 
        error "flattenPrefix - unterminated Pre"  

-- Flatten a list of polyphonic 'trees' to 'footnotes'
flattenPoly :: (Perform evt) 
            => [EventTree evt]
            -> Seq (EvtPosition evt)
            -> TA 
            -> ProcessM TA
flattenPoly ts next acc = withGets measure_count $    \mc   -> 
    mapM (flattenPass mc (-1)) ts                >>=  \xs   ->
    mergeTAs acc xs                              >>=  \acc' ->
    modify (\s -> s { measure_count = mc })      >> 
    flattenStep (viewl next) acc'



mergeTAs :: TA -> [ScPart] -> ProcessM (TA)
mergeTAs = foldM merge1
  
merge1 :: TA -> ScPart -> ProcessM (TA)
merge1 tacc p = withNewRefId $ \i -> 
    let line  = sc_part_primary_line p
        rm    = IM.insert i line (getPolyRefs $ sc_part_poly_refs p) 
        tacc' = appendRefs tacc (ScPolyRefs rm)
        tacc'' = addRefToTip i tacc' 
    in return $ tacc''


{-
-- This doesn't work properly for nested TA's 
-- Ought to do a top down traversal
mergeTAs :: TA -> [ScPart] -> ProcessM (TA)
mergeTAs tacc ts = uncurry (appendRefs tacc) <$> foldM fn (mempty,[]) ts
  where 
    fn (refs,xs) part = appendP (refs,xs) <$> reshape part
    
    appendP (r1,xs) (r2,x) = (r1 `mappend` r2, xs++[x]) 
    
    reshape :: ScPart -> ProcessM (ScPartRefs, Int)
    reshape (ScPart _ refs sp) = withNewRefId $ \i -> 
        let mp' = IM.insert i (extractMeasures sp) (getRefs refs)
        in return (ScPartRefs mp',i)

   
extractMeasures :: Seq (ScPoly) -> Seq (ScMeasure)
extractMeasures = F.foldl fn mempty
  where
    fn sm (ScPolyRef xs) = error "Urk - unexpected nesting"
    fn sm (ScPolyM m)    = sm |> m
-}


flattenPass :: (Perform evt) 
          => Int -> Int -> EventTree evt -> ProcessM ScPart          
flattenPass mc pnum t = do
    modify (\s -> s { measure_count = mc })
    dc          <- gets duration_count    
    ta'         <- flattenStep (viewl $ unET t) (mkTA dc)
    (refs,se)   <- finalSpacer ta'
    return $ ScPart pnum refs se 
  where
    mkTA dc     = emptyTA `atMeasure` mc `spaceTip` dc 
                      
        
processPerformance :: (Perform evt) 
                   => Performance evt 
                   -> ProcessM ScScore
processPerformance p@(Perf xs) = ScScore <$> foldM fn mempty (zip xs [1..]) 
  where
    fn sq (x,i) = (sq |>) <$> flattenPass 1 i x 

toScore1 :: (Perform evt) 
         => EventTree evt 
         -> Perform_Sc_Env 
         -> ScScore
toScore1 tree env = toScore (Perf [tree]) env

toScore :: (Perform evt)  
        => Performance evt 
        -> Perform_Sc_Env 
        -> ScScore
toScore perf env = evalPerform (processPerformance perf) intial_score_state env
