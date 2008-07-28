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

module Bala.Perform.Score.ToScore where

import Bala.Base
import Bala.Format.Score
import Bala.Perform.Base.EventTree
import Bala.Perform.Base.PerformMonad
import Bala.Perform.Base.Class

import Control.Applicative hiding (empty)
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Foldable as F
import qualified Data.Map as Map
import Data.Monoid
import Data.Sequence hiding (reverse)


type ProcessM a = PerformM Perform_Sc_State Perform_Sc_Env a


  
  
data Perform_Sc_State = Perform_Sc_State { 
    tagcounter          :: Integer,
    refcounter          :: Integer,
    measure_count       :: Integer,
    duration_count      :: Double
  }  
  deriving (Show)

data Perform_Sc_Env = Perform_Sc_Env { 
    measure_length        :: Double,
    first_bar_onset       :: Double
  }  
  deriving (Show)
  
-- 'tipped' accumulator - a Seq of bars produced plus the 'tip' the current bar
-- (plus a dictionary of parts for polypohonic elements) 
data TA pch dur = 
  TA (ScPartRefs pch dur) (Seq (ScPoly pch dur)) (ScMeasure pch dur) 

emptyTA = TA mempty mempty (ScMeasure 0 mempty)

atMeasure (TA r sm (ScMeasure _ tip)) i = TA r sm (ScMeasure i tip)

spaceTip :: (ScoreDuration dur) => TA pch dur -> Double -> TA pch dur
spaceTip ta@(TA r sm (ScMeasure i _)) dur 
    | dur == 0  = ta
    | otherwise = let spacer = ScSpacer (fromDouble dur) 
                  in TA r sm (ScMeasure i (mempty |> spacer))

intial_score_state = Perform_Sc_State { 
    tagcounter      = 1,
    refcounter      = 1,
    measure_count   = 1,
    duration_count  = 0.0
  } 

default_score_env :: Perform_Sc_Env
default_score_env = Perform_Sc_Env { 
    measure_length    = 1.0,   
    first_bar_onset   = 0.0 
  }


withGets :: (Perform_Sc_State -> a) -> (a -> ProcessM b) -> ProcessM b 
withGets lbl f = do
  i <- gets lbl
  f i  

 
withNewTag :: (ScTag -> ProcessM a) -> ProcessM a    
withNewTag f = withGets tagcounter      $ \i -> 
    modify (\s -> s {tagcounter = i+1}) >> 
    f (ScTag i) 
      
    
withNewRefId :: (Integer -> ProcessM a) -> ProcessM a    
withNewRefId f = withGets refcounter    $ \i -> 
    modify (\s -> s {refcounter = i+1}) >>
    f i


remainingDuration :: ProcessM Double
remainingDuration = (-) <$> asks measure_length <*> gets duration_count

        
increaseMeasure :: Double -> ProcessM (Maybe Integer) 
increaseMeasure d = fn d =<< remainingDuration
  where
    fn d duration_left
        | d >= duration_left  = withGets measure_count $ \mc -> do
                modify (\s -> s {duration_count = 0.0, measure_count = mc+1})
                return $ Just (mc + 1) 
                
                
        | otherwise           = let d' = duration_left + d in 
                modify (\s -> s {duration_count = d'}) >> return Nothing
              

fnMC :: Integer -> ProcessM (Maybe Integer)      
fnMC mc = Just (mc+1) <$ modify (\s -> s {duration_count = 0.0, measure_count=mc+1})   


-- Add an event (i.e. a glyph to the tip of the current measure.
-- If the measure is full (i.e. its current count == the measure count)
-- then add it to the accumulation of measures.
(|%>) :: ScoreDuration dur 
      => TA pch dur 
      -> ScGlyph pch dur 
      -> ProcessM (TA pch dur)
tippedacc |%> a = fn <$> increaseMeasure (glyphDuration a)
  where
    fn (Just mc)  = appendMeasure tippedacc a mc 
    fn Nothing    = tippedacc `suffixTip` a
     

-- Concatenate the measure onto the sequence of measures 
-- and reset the tip of the TA
appendMeasure :: TA pch dur -> ScGlyph pch dur -> Integer -> TA pch dur 
appendMeasure (TA refs sm (ScMeasure i se)) a mc  = 
    let m   = ScPolyM $ ScMeasure i (se |> a) 
        m0  = ScMeasure i mempty 
    in TA refs (sm |> m) m0     
    
appendRefs :: TA pch dur -> ScPartRefs pch dur -> Integer -> TA pch dur   
appendRefs (TA refs sm se) r2 i = 
    TA (refs `mappend` r2) (sm |> ScPolyRef i) se

-- Add a glyph to the the tip of the TA
suffixTip :: TA pch dur -> ScGlyph pch dur -> TA pch dur
suffixTip (TA refs sm (ScMeasure i se)) e = TA refs sm $ ScMeasure i (se |> e)


 


glyphDuration :: ScoreDuration dur => ScGlyph pch dur -> Double
glyphDuration (ScNote  _ d)             = toDouble d
glyphDuration (ScRest  d)               = toDouble d
glyphDuration (ScGroup ScChord (x:xs))  = glyphDuration x
glyphDuration (ScGroup ScGraceNotes _)  = 0.0
glyphDuration (ScGroup ScBeam xs)       = let f e d = d + glyphDuration e
                                          in foldr f 0.0 xs           
glyphDuration _                         = 0.0        








    


    
-- extend the final note in the TA    
finalExt :: ScoreDuration dur 
         => TA pch dur 
         -> ProcessM (ScPartRefs pch dur, Seq (ScPoly pch dur))
finalExt (TA refs sb (ScMeasure i se)) = case viewr se of
    EmptyR     -> return (refs,sb)
    (se' :> e) -> (fn se') <$> remext e
  where
    fn s e = (refs, sb |> (ScPolyM $ ScMeasure i (s |> e)))

-- extend the event with the 'remainder duration' of the bar
remext :: ScoreDuration dur => ScGlyph pch dur -> ProcessM (ScGlyph pch dur)
remext e = fn <$> remainingDuration
  where
    fn duration_left = changeDuration (glyphDuration e + duration_left) e
     
    -- Don't change the duration of beamed notes or grace notes
    -- LilyPond or Abc can decide what to do
    -- (grace notes shouldn't be at the end anyway)
    changeDuration :: ScoreDuration dur 
                   => Double -> ScGlyph pch dur -> ScGlyph pch dur
    changeDuration d (ScNote p _)           = ScNote p (fromDouble d)
    changeDuration d (ScRest _)             = ScRest (fromDouble d)
    changeDuration d (ScGroup ScChord xs)   = 
        let xs' = map (changeDuration d) xs in ScGroup ScChord xs'
    changeDuration d g                      = g
        
    
-- add a rest to finalize the TA
finalRest :: ScoreDuration dur 
          => TA pch dur 
          -> ProcessM (ScPartRefs pch dur, Seq (ScPoly pch dur))
finalRest (TA refs sb (ScMeasure i se)) = case viewr se of
    EmptyR -> return (refs,sb)
    _      -> fn <$> remrest
  where
    fn r = (refs, sb |> (ScPolyM $ ScMeasure i (se |> r))) 

    -- make a rest with the 'remainder duration' of the bar
    remrest :: ScoreDuration dur => ProcessM (ScGlyph pch dur)
    remrest = (ScRest . fromDouble) <$> remainingDuration




scGroup :: ScGroupType -> [ScGlyph pch dur] -> ScGlyph pch dur
scGroup typ xs = ScGroup typ xs

scChord = scGroup ScChord             
             
scGrace = scGroup  ScGraceNotes     


scNote :: pch -> dur -> ScGlyph pch dur
scNote p d = ScNote (ScPitch p) d


scRest :: dur -> ScGlyph pch dur
scRest d = ScRest d
    
-- mkRef :: ProcessM ScGlyph
-- mkRef = ScRef <$> newTag 



withEvent :: (Perform evt pch dur) 
          => evt 
          -> (ScGlyph pch dur -> ProcessM a) 
          -> ProcessM a
withEvent evt f = makesEvent evt >>= f
  where
    makesEvent evt = case eventvalues evt of
                        (Just p, Just d)    -> return $ scNote p d
                        (Nothing, Just d)   -> return $ scRest d
                        (Nothing, Nothing)  -> error "withEvent - to do "

-- All notes should have the same duration - is a check in order?

suffixChord :: ScoreDuration dur 
            => TA pch dur 
            -> [ScGlyph pch dur] 
            -> ProcessM (TA pch dur)
suffixChord se []  = return $ se
suffixChord se stk = se |%> (scChord $ reverse stk) 

suffixGrace :: ScoreDuration dur 
            => TA pch dur 
            -> [ScGlyph pch dur] 
            -> ProcessM (TA pch dur)
suffixGrace se []  = return $ se
suffixGrace se stk = se |%> (scGrace $ reverse stk) 


    
flattenStep :: (Perform evt pch dur, ScoreDuration dur) 
            => ViewL (EvtPosition evt)
            -> TA pch dur  
            -> ProcessM (TA pch dur)
flattenStep sq acc = flatstep sq acc
  where
    flatstep EmptyL               acc = return acc

    flatstep (Evt e :< sq)        acc = withEvent e $ \ez -> 
                                        acc |%> ez >>= flatstep (viewl sq)

    flatstep (StartPar :< sq)     acc = flattenParallel (viewl sq) (acc,[]) 
    
    flatstep (StartPre :< sq)     acc = flattenPrefix (viewl sq) (acc,[])
            
    flatstep (Poly ts :< sq)      acc = flattenPoly ts sq acc


-- Flatten a parallel block to a chord
flattenParallel :: (Perform evt pch dur, ScoreDuration dur) 
                => ViewL (EvtPosition evt)
                -> (TA pch dur, [ScGlyph pch dur])
                -> ProcessM (TA pch dur)                        
flattenParallel sq (se,stk) = flatpar sq (se,stk)
  where
    flatpar (EndPar :< sq) (se,stk) = 
        se `suffixChord` stk >>= flattenStep (viewl sq)

    flatpar (Evt e :< sq)  (se,stk) = withEvent e $ \ez ->
        flatpar (viewl sq) (se, ez:stk)

    flatpar _              (se,stk) = 
        error "flattenParallel - unterminated Par"


-- Flatten a prefix block to grace notes    
flattenPrefix :: (Perform evt pch dur, ScoreDuration dur) 
              => ViewL (EvtPosition evt)
              -> (TA pch dur, [ScGlyph pch dur])
              -> ProcessM (TA pch dur)  
flattenPrefix sq (se,stk) = flatpre sq (se,stk)
  where  
    flatpre (EndPre :< sq) (se,stk) = do
        se `suffixGrace` stk >>= flattenStep (viewl sq)

    flatpre (Evt e :< sq)  (se,stk) = withEvent e $ \ez ->
        flatpre (viewl sq) (se, (ez:stk))
                            
    flatpre _              (se,stk) = 
        error "flattenPrefix - unterminated Pre"  

-- Flatten a list of polyphonic 'trees' to 'footnotes'
flattenPoly :: (Perform evt pch dur, ScoreDuration dur) 
            => [EventTree evt]
            -> Seq (EvtPosition evt)
            -> TA pch dur 
            -> ProcessM (TA pch dur)
flattenPoly ts next acc = withGets measure_count $    \mc   -> 
    mapM (flattenPass mc (-1)) ts                >>=  \xs   ->
    mergeTAs acc xs                              >>=  \acc' ->
    modify (\s -> s { measure_count = mc })      >> 
    flattenStep (viewl next) acc'


mergeTAs :: TA pch dur -> [ScPart pch dur] -> ProcessM (TA pch dur)
mergeTAs tacc ts = foldM fn tacc ts
  where 
    fn tacc part = uncurry (appendRefs tacc) <$> reshape part
    
    reshape :: ScPart pch dur -> ProcessM (ScPartRefs pch dur, Integer)
    reshape (ScPart _ refs sp) = withNewRefId $ \i -> 
        let mp' = Map.insert i (F.toList sp) (getRefs refs)
        in return (ScPartRefs mp',i)
    

flattenPass :: (Perform evt pch dur, ScoreDuration dur) 
          => Integer -> Integer -> EventTree evt -> ProcessM (ScPart pch dur)          
flattenPass mc pnum t = do
    modify (\s -> s { measure_count = mc })
    dc          <- gets duration_count    
    ta'         <- flattenStep (viewl $ unET t) (mkTA dc)
    (refs,se)   <- finalExt ta'
    return $ ScPart pnum refs se 
  where
    mkTA dc     = emptyTA `atMeasure` mc `spaceTip` dc 
                      
        
processPerformance :: (Perform evt pch dur, ScoreDuration dur) 
                   => Performance evt 
                   -> ProcessM (ScScore pch dur)
processPerformance p@(Perf xs) = ScScore <$> foldM fn mempty (zip xs [1..]) 
  where
    fn sq (x,i) = (sq |>) <$> flattenPass 1 i x 

toScore1 :: (Perform evt pch dur, ScoreDuration dur) 
         => EventTree evt 
         -> Perform_Sc_Env 
         -> ScScore pch dur
toScore1 tree env = toScore (Perf [tree]) env

toScore :: (Perform evt pch dur, ScoreDuration dur)  
        => Performance evt 
        -> Perform_Sc_Env 
        -> ScScore pch dur
toScore perf env = evalPerform (processPerformance perf) intial_score_state env
