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
data TSeq = TSeq  ScPartRefs  (Seq ScPoly) ScMeasure 



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


glyphDuration :: ScGlyph -> Double
glyphDuration (ScNote  _ _ d)            = d
glyphDuration (ScRest  d)                = d
glyphDuration (ScGroup ScChord (x:xs))  = glyphDuration x
glyphDuration (ScGroup ScGraceNotes _)  = 0.0
glyphDuration (ScGroup ScBeam xs)       = 
    let f e d = d + glyphDuration e
    in foldr f 0.0 xs           
glyphDuration _                           = 0.0        



(|%>) :: TSeq -> ScGlyph -> ProcessM TSeq
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
remext e = let ed = glyphDuration e in do
    dc    <- gets duration_count
    sz    <- asks measure_length
    return $ changeDuration (ed + (sz - dc)) e
  where
    -- Don't change the duration of beamed notes or grace notes
    -- LilyPond or Abc can decide what to do
    -- (grace notes shouldn't be at the end anyway)
    changeDuration :: Double -> ScGlyph -> ScGlyph
    changeDuration d (ScNote p o _)             = ScNote p o d
    changeDuration d (ScRest _)                 = ScRest d
    changeDuration d (ScGroup ScChord xs)       = 
        let xs' = map (changeDuration d) xs in ScGroup ScChord xs'
    changeDuration d g                            = g

-- make a rest with the 'remainder duration' of the bar
remrest :: ProcessM ScGlyph
remrest = do
    dc    <- gets duration_count
    sz    <- asks measure_length
    return $ ScRest (sz - dc) 
    


    
-- extend the final note in the TSeq    
finalExt :: TSeq -> ProcessM (ScPartRefs, Seq ScPoly)
finalExt (TSeq refs sb (ScMeasure i se)) = case viewr se of
  EmptyR -> return (refs,sb)
  (se' :> e) -> remext e >>= \e' -> 
                return $ (refs, sb |> (ScPolyM $ ScMeasure i (se' |> e'))) 

-- add a rest to finalize the TSeq
finalRest :: TSeq -> ProcessM (ScPartRefs, Seq ScPoly)
finalRest (TSeq refs sb (ScMeasure i se)) = case viewr se of
  EmptyR -> return (refs,sb)
  _  -> remrest >>= \r' -> 
        return $ (refs, sb |> (ScPolyM $ ScMeasure i (se |> r'))) 


emptyseq :: Seq a
emptyseq = empty



scGroup :: ScGroupType -> [ScGlyph] -> ScGlyph
scGroup typ xs = ScGroup typ xs

scChord = scGroup ScChord             
             
scGrace = scGroup  ScGraceNotes     


scNote :: Pitch -> Duration -> ScGlyph
scNote p d = ScNote (pitchName p) (octaveMeasure p) (durationSize d)


scRest :: Duration -> ScGlyph
scRest d = ScRest (durationSize d)
    
-- mkRef :: ProcessM ScGlyph
-- mkRef = ScRef <$> newTag 

makesEvent :: (Perform evt) => evt -> ProcessM ScGlyph
makesEvent evt = case (opitch evt, oduration evt) of
    (Just p, Just d)    -> return $ scNote p d
    (Nothing, Just d)   -> return $ scRest d
    (Nothing, Nothing)  -> undefined -- mkRef


-- All notes should have the same duration - is a check in order?

suffixChord :: TSeq -> [ScGlyph] -> ProcessM TSeq
suffixChord se []  = return $ se
suffixChord se stk = se |%> (scChord $ reverse stk) 

suffixGrace :: TSeq -> [ScGlyph] -> ProcessM TSeq
suffixGrace se []  = return $ se
suffixGrace se stk = se |%> (scGrace $ reverse stk) 


    
flattenStep :: (Perform t) => TSeq -> ViewL (EvtPosition t) -> ProcessM TSeq
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

flattenPoly :: (Perform t) =>
               TSeq -> [EventTree t] -> Seq (EvtPosition t) -> ProcessM TSeq
flattenPoly acc ts next = do
      mc    <- gets measure_count
      xs    <- mapM (flattenPass mc (-1)) ts
      acc'  <- mergeTSeqs acc xs
      modify (\s -> s { measure_count = mc })
      flattenStep acc' (viewl next)
  where
    newTSeq mc = TSeq mempty emptyseq (ScMeasure mc emptyseq)      

mergeTSeqs :: TSeq -> [ScPart] -> ProcessM TSeq
mergeTSeqs acc ts = foldM fn acc ts
  where 
    fn (TSeq refs se m) part = do
        (e,rf)  <- reshape part
        let refs' = refs
        return $ TSeq (refs `mappend` rf) (se |> e) m


reshape :: ScPart -> ProcessM (ScPoly,ScPartRefs)
reshape (ScPart _ refs sp) = do
    i <- newRefId
    let mp' = Map.insert i (F.toList sp) (getRefs refs)
    return (ScPolyRef i, ScPartRefs mp')


flattenPass :: (Perform evt) 
          => Integer -> Integer -> EventTree evt -> ProcessM ScPart          
flattenPass mc pnum t = do
    modify (\s -> s { measure_count = mc })
    dc          <- gets duration_count
    tseq'       <- flattenStep (mkTSeq dc) (viewl $ unET t)
    (refs,se)   <- finalExt tseq'
    return $ ScPart pnum refs se 
  where
    mkTSeq dc
        | dc == 0   = TSeq mempty emptyseq (ScMeasure mc emptyseq)
        | otherwise = let spacer = ScSpacer dc 
                          se     = emptyseq |> spacer 
                      in TSeq mempty emptyseq (ScMeasure mc se)    
                      
        
processPerformance :: Perform evt => Performance evt -> ProcessM ScScore
processPerformance p@(Perf xs) = ScScore <$> foldM fn emptyseq (zip xs [1..]) 
  where
    fn sq (x,i) = (sq |>) <$> flattenPass 1 i x 

renderScore1 :: (Perform evt) 
            => EventTree evt -> Perform_Sc_Env -> ScScore
renderScore1 tree env = 
    renderScore (Perf [tree]) env

renderScore :: (Perform evt) 
           => Performance evt -> Perform_Sc_Env -> ScScore
renderScore perf env = 
    evalPerform (processPerformance perf) intial_score_state env
