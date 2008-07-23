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

import Control.Applicative
import Control.Monad.State
import Data.Sequence hiding (reverse)


type ProcessM a = PerformM Perform_Sc_State Perform_Sc_Env a


data Perform_Sc_State = Perform_Sc_State { 
    tagcount :: Integer
  }  
  deriving (Show)

data Perform_Sc_Env = Perform_Sc_Env { 
    unknown :: ()
  }  
  deriving (Show)
  

intial_score_state = Perform_Sc_State { 
    tagcount  = 1
  } 

default_score_env :: Perform_Sc_Env
default_score_env = Perform_Sc_Env { unknown = () }
  
newTag :: ProcessM ScTag
newTag = do
    i <- gets tagcount
    modify (\s -> s {tagcount = i+1})
    return $ ScTag i


scPitch :: Pitch -> Duration -> ProcessM ScGlyph
scPitch p d = mkNote p d <$> newTag
  where
    mkNote p d tag = ScNote tag (pitchName p) (octaveMeasure p) (durationSize d)


scRest :: Duration -> ProcessM ScGlyph
scRest d = mkRest d <$> newTag
  where
    mkRest d tag = ScRest tag (durationSize d)
    
    


makesEvent evt = case (opitch evt, oduration evt) of
    (Just p, Just d)    -> scPitch p d
    (Nothing, Just d)   -> scRest d
    (Nothing, Nothing)  -> error "makesEvent - to do"

oflat se  EmptyL               = return (reverse se) 

oflat se (Evt e :< sq)         = do
    e        <- makesEvent e
    oflat (e:se) (viewl sq)
    
    
oflat se _                      = error "oflat - to do"

run'oflat :: (Perform evt) 
          => EventTree evt -> ProcessM ScScore             
run'oflat t = do 
    xs <- oflat [] (viewl $ unET t)
    return $ ScScore xs 
    
processPerformance :: Perform evt => Performance evt -> ProcessM ScScore
processPerformance p@(Perf (x:xs)) = do 
  run'oflat x 

renderScore1 :: (Perform evt) 
            => EventTree evt -> Perform_Sc_Env -> ScScore
renderScore1 tree env = 
    renderScore (Perf [tree]) env

renderScore :: (Perform evt) 
           => Performance evt -> Perform_Sc_Env -> ScScore
renderScore perf env = 
    evalPerform (processPerformance perf) intial_score_state env
