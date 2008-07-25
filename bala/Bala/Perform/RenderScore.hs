--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Perform.RenderLilyPond
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

module Bala.Perform.RenderScore where

import Bala.Base
import Bala.Format.Score
import Bala.Perform.EventTree
import Bala.Perform.PerformBase

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
  
-- 'tipped' sequence - a Seq of bars produced plus the 'tip' the current bar
-- (plus a dictionary of parts for polypohonic elements) 
data TSeq pch dur = 
  TSeq (ScPartRefs pch dur) (Seq (ScPoly pch dur)) (ScMeasure pch dur) 



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

  
newTag :: ProcessM ScTag
newTag = do
    i <- gets tagcounter
    modify (\s -> s {tagcounter = i+1})
    return $ ScTag i

newRefId :: ProcessM Integer
newRefId = do
    i <- gets refcounter
    modify (\s -> s {refcounter = i+1})
    return i
    

        
increaseMeasure :: Double -> ProcessM (Maybe Integer) 
increaseMeasure d = do
    dc    <- gets duration_count
    sz    <- asks measure_length
    case (d + dc >= sz) of 
        -- what if this is a tied note over a bar?
        True -> do 
            mc  <- gets measure_count
            modify (\s -> s {duration_count = 0.0, measure_count=mc+1})
            return $ Just (mc+1)
        False -> modify (\s -> s {duration_count = d + dc}) >> return Nothing


glyphDuration :: ScDuration dur => ScGlyph pch dur -> Double
glyphDuration (ScNote  _ d)             = toDouble d
glyphDuration (ScRest  d)               = toDouble d
glyphDuration (ScGroup ScChord (x:xs))  = glyphDuration x
glyphDuration (ScGroup ScGraceNotes _)  = 0.0
glyphDuration (ScGroup ScBeam xs)       = 
    let f e d = d + glyphDuration e
    in foldr f 0.0 xs           
glyphDuration _                         = 0.0        



(|%>) :: ScDuration dur => TSeq pch dur -> ScGlyph pch dur -> ProcessM (TSeq pch dur)
(|%>) (TSeq refs sb se) a = let d = glyphDuration a in do
    optnext <- increaseMeasure d
    case optnext of
        Just i -> let sb' = sb |> (ScPolyM (se `suffix` a))
                      m0  = mkMeasure i
                  in return $ TSeq refs sb' m0
        Nothing -> return $ TSeq refs sb (se `suffix` a)
  where
    mkMeasure i = ScMeasure i emptyseq        
    suffix (ScMeasure i se) e = ScMeasure i (se |> e)

-- extend the event with the 'remainder duration' of the bar
remext :: ScDuration dur => ScGlyph pch dur -> ProcessM (ScGlyph pch dur)
remext e = let ed = glyphDuration e in do
    dc    <- gets duration_count
    sz    <- asks measure_length
    return $ changeDuration (ed + (sz - dc)) e
  where
    -- Don't change the duration of beamed notes or grace notes
    -- LilyPond or Abc can decide what to do
    -- (grace notes shouldn't be at the end anyway)
    changeDuration :: ScDuration dur => Double -> ScGlyph pch dur -> ScGlyph pch dur
    changeDuration d (ScNote p _)               = ScNote p (fromDouble d)
    changeDuration d (ScRest _)                 = ScRest (fromDouble d)
    changeDuration d (ScGroup ScChord xs)       = 
        let xs' = map (changeDuration d) xs in ScGroup ScChord xs'
    changeDuration d g                          = g

-- make a rest with the 'remainder duration' of the bar
remrest :: ScDuration dur => ProcessM (ScGlyph pch dur)
remrest = do
    dc    <- gets duration_count
    sz    <- asks measure_length
    return $ ScRest (fromDouble $ sz - dc) 
    


    
-- extend the final note in the TSeq    
finalExt :: ScDuration dur 
         => TSeq pch dur 
         -> ProcessM (ScPartRefs pch dur, Seq (ScPoly pch dur))
finalExt (TSeq refs sb (ScMeasure i se)) = case viewr se of
  EmptyR -> return (refs,sb)
  (se' :> e) -> remext e >>= \e' -> 
                return $ (refs, sb |> (ScPolyM $ ScMeasure i (se' |> e'))) 

-- add a rest to finalize the TSeq
finalRest :: ScDuration dur 
          => TSeq pch dur 
          -> ProcessM (ScPartRefs pch dur, Seq (ScPoly pch dur))
finalRest (TSeq refs sb (ScMeasure i se)) = case viewr se of
  EmptyR -> return (refs,sb)
  _  -> remrest >>= \r' -> 
        return $ (refs, sb |> (ScPolyM $ ScMeasure i (se |> r'))) 


emptyseq :: Seq a
emptyseq = empty



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

makesEvent :: (Perform evt pch dur) => evt -> ProcessM (ScGlyph pch dur)
makesEvent evt = case eventvalues evt of
    (Just p, Just d)    -> return $ scNote p d
    (Nothing, Just d)   -> return $ scRest d
    (Nothing, Nothing)  -> undefined -- mkRef


-- All notes should have the same duration - is a check in order?

suffixChord :: ScDuration dur 
            => TSeq pch dur 
            -> [ScGlyph pch dur] 
            -> ProcessM (TSeq pch dur)
suffixChord se []  = return $ se
suffixChord se stk = se |%> (scChord $ reverse stk) 

suffixGrace :: ScDuration dur 
            => TSeq pch dur 
            -> [ScGlyph pch dur] 
            -> ProcessM (TSeq pch dur)
suffixGrace se []  = return $ se
suffixGrace se stk = se |%> (scGrace $ reverse stk) 


    
flattenStep :: (Perform evt pch dur, ScDuration dur) 
            => TSeq pch dur 
            -> ViewL (EvtPosition evt) 
            -> ProcessM (TSeq pch dur)
flattenStep acc sq = flatstep acc sq 
  where
    flatstep acc EmptyL                = return acc

    flatstep acc (Evt e :< sq)         = do
        e'        <- makesEvent e
        acc'      <- acc |%> e'
        flatstep acc' (viewl sq)

    flatstep acc (StartPar :< sq)      =
        flattenParallel (acc,[]) (viewl sq)
    
    flatstep acc (StartPre :< sq)      =
        flattenPrefix (acc,[]) (viewl sq)
            
    flatstep acc (Poly ts :< sq)       = 
        flattenPoly acc ts sq

flattenParallel (se,stk) sq = flatpar (se,stk) sq
  where
    flatpar (se,stk) (Evt e :< sq)  = do
        e         <- makesEvent e
        flatpar (se, e:stk) (viewl sq)
        
        
    flatpar (se,stk) (EndPar :< sq) = do
        se'       <- suffixChord se stk 
        flattenStep se' (viewl sq)
      
    flatpar (se,stk) _              = 
        error "flattenParallel - unterminated Par"
    

flattenPrefix (se,stk) sq = flatpre (se,stk) sq
  where  
    flatpre (se,stk) (Evt e :< sq)  = do
        ez        <- makesEvent e
        flatpre (se, (ez:stk)) (viewl sq)
      
    flatpre (se,stk) (EndPre :< sq) = do
        se'       <- suffixGrace se stk 
        flattenStep se' (viewl sq)
                            
    flatpre (se,stk) _              = 
        error "flattenPrefix - unterminated Pre"  

flattenPoly :: (Perform evt pch dur, ScDuration dur) 
            => TSeq pch dur 
            -> [EventTree evt]
            -> Seq (EvtPosition evt)
            -> ProcessM (TSeq pch dur)
flattenPoly acc ts next = do
      mc    <- gets measure_count
      xs    <- mapM (flattenPass mc (-1)) ts
      acc'  <- mergeTSeqs acc xs
      modify (\s -> s { measure_count = mc })
      flattenStep acc' (viewl next)
  where
    newTSeq mc = TSeq mempty emptyseq (ScMeasure mc emptyseq)      

mergeTSeqs :: TSeq pch dur -> [ScPart pch dur] -> ProcessM (TSeq pch dur)
mergeTSeqs acc ts = foldM fn acc ts
  where 
    fn (TSeq refs se m) part = do
        (e,rf)  <- reshape part
        let refs' = refs
        return $ TSeq (refs `mappend` rf) (se |> e) m


reshape :: ScPart pch dur -> ProcessM (ScPoly pch dur, ScPartRefs pch dur)
reshape (ScPart _ refs sp) = do
    i <- newRefId
    let mp' = Map.insert i (F.toList sp) (getRefs refs)
    return (ScPolyRef i, ScPartRefs mp')


flattenPass :: (Perform evt pch dur, ScDuration dur) 
          => Integer -> Integer -> EventTree evt -> ProcessM (ScPart pch dur)          
flattenPass mc pnum t = do
    modify (\s -> s { measure_count = mc })
    dc          <- gets duration_count
    tseq'       <- flattenStep (mkTSeq dc) (viewl $ unET t)
    (refs,se)   <- finalExt tseq'
    return $ ScPart pnum refs se 
  where
    mkTSeq dc
        | dc == 0   = TSeq mempty emptyseq (ScMeasure mc emptyseq)
        | otherwise = let spacer = ScSpacer (fromDouble dc) 
                          se     = emptyseq |> spacer 
                      in TSeq mempty emptyseq (ScMeasure mc se)    
                      
        
processPerformance :: (Perform evt pch dur, ScDuration dur) 
                   => Performance evt 
                   -> ProcessM (ScScore pch dur)
processPerformance p@(Perf xs) = ScScore <$> foldM fn emptyseq (zip xs [1..]) 
  where
    fn sq (x,i) = (sq |>) <$> flattenPass 1 i x 

renderScore1 :: (Perform evt pch dur, ScDuration dur) 
             => EventTree evt 
             -> Perform_Sc_Env 
             -> ScScore pch dur
renderScore1 tree env = 
    renderScore (Perf [tree]) env

renderScore :: (Perform evt pch dur, ScDuration dur)  
            => Performance evt 
            -> Perform_Sc_Env 
            -> ScScore pch dur
renderScore perf env = 
    evalPerform (processPerformance perf) intial_score_state env
