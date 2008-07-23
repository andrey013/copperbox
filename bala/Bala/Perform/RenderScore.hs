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
import Data.Sequence hiding (reverse)


type ProcessM a = PerformM Perform_Sc_State Perform_Sc_Env a



data Perform_Sc_State = Perform_Sc_State { 
    tagcounter      :: Integer,
    part_count      :: Integer,
    bar_count       :: Integer,
    duration_count  :: Double
  }  
  deriving (Show)

data Perform_Sc_Env = Perform_Sc_Env { 
    bar_size        :: Double,
    first_bar_onset :: Double
  }  
  deriving (Show)
  
-- 'tipped' sequence - a Seq of bars produced plus the 'tip' the current bar 
newtype TSeq = TSeq { getTSeq :: (Seq ScBar, ScBar)} 



intial_score_state = Perform_Sc_State { 
    tagcounter      = 1,
    part_count      = 0,
    bar_count       = 1,
    duration_count  = 0.0
  } 

default_score_env :: Perform_Sc_Env
default_score_env = Perform_Sc_Env { 
    bar_size        = 1.0,   
    first_bar_onset = 0.0 
  }

withPart :: ProcessM Integer
withPart = do 
    pc      <- gets part_count 
    modify (\s -> s {bar_count = 1, part_count=pc+1})
    return (pc+1)
  
newTag :: ProcessM ScTag
newTag = do
    i <- gets tagcounter
    modify (\s -> s {tagcounter = i+1})
    return $ ScTag i

initialTSeq :: ProcessM TSeq
initialTSeq = do
    bc    <- gets bar_count  -- should be 1
    return $ TSeq (emptyseq, ScBar bc emptyseq)
        
progressBar :: Double -> ProcessM (Maybe Integer) 
progressBar d = do
    dc    <- gets duration_count
    sz    <- asks bar_size
    case (d + dc >= sz) of 
        -- what if this is a tied note over a bar?
        True -> do 
            bc  <- gets bar_count
            modify (\s -> s {duration_count = 0.0, bar_count=bc+1})
            return $ Just (bc+1)
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
(|%>) (TSeq (sb,se)) a = let d = glyphDuration a in do
    optnext <- progressBar d
    case optnext of
        Just i -> return $ TSeq (sb |> (se `suffix` a), mkBar i)
        Nothing -> return $ TSeq (sb,se `suffix` a)
  where
    mkBar i = ScBar i emptyseq        
    suffix (ScBar i se) e = ScBar i (se |> e)

-- extend the event with the 'remainder duration' of the bar
remext e = let ed = glyphDuration e in do
    dc    <- gets duration_count
    sz    <- asks bar_size
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
    sz    <- asks bar_size
    return $ ScRest (sz - dc) 
    
unSeqBar (ScBar _ sg) = sg

    
-- extend the final note in the TSeq    
finalExt :: TSeq -> ProcessM (Seq ScBar)
finalExt (TSeq (sb,ScBar i se)) = case viewr se of
  EmptyR -> return sb
  (se' :> e) -> remext e >>= \e' -> return $ sb |> (ScBar i (se' |> e')) 

-- add a rest to finalize the TSeq
finalRest :: TSeq -> ProcessM (Seq ScBar)
finalRest (TSeq (sb,ScBar i se)) = case viewr se of
  EmptyR -> return sb
  _  -> remrest >>= \r' -> return $ sb |> (ScBar i (se |> r')) 


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
    
mkRef :: ProcessM ScGlyph
mkRef = ScRef <$> newTag 

makesEvent :: (Perform evt) => evt -> ProcessM ScGlyph
makesEvent evt = case (opitch evt, oduration evt) of
    (Just p, Just d)    -> return $ scNote p d
    (Nothing, Just d)   -> return $ scRest d
    (Nothing, Nothing)  -> mkRef


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
            
    flatstep acc _                     = error "oflat - to do"
    
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
        


flattenPass :: (Perform evt) 
          => EventTree evt -> ProcessM ScPart          
flattenPass t = do
    tseq0     <- initialTSeq 
    pc        <- withPart
    tseq'     <- flattenStep tseq0 (viewl $ unET t)
    se        <- finalExt tseq'
    return $ ScPart pc se 
    
processPerformance :: Perform evt => Performance evt -> ProcessM ScScore
processPerformance p@(Perf xs) = ScScore <$> foldM fn emptyseq xs 
  where
    fn sq x = (sq |>) <$> flattenPass x 

renderScore1 :: (Perform evt) 
            => EventTree evt -> Perform_Sc_Env -> ScScore
renderScore1 tree env = 
    renderScore (Perf [tree]) env

renderScore :: (Perform evt) 
           => Performance evt -> Perform_Sc_Env -> ScScore
renderScore perf env = 
    evalPerform (processPerformance perf) intial_score_state env
