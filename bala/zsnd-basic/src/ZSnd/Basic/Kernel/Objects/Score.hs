{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZSnd.Basic.Kernel.Objects.Score
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
--
--------------------------------------------------------------------------------

module ZSnd.Basic.Kernel.Objects.Score
  ( 

    traceNotelist
  , traceNotelistU

  , traceAdvNotelist
  , traceAdvNotelistU

  , traceOnsetNotelist
  , traceOnsetNotelistU

    
  ) where


import ZSnd.Basic.Kernel.Base.BaseDefs
import ZSnd.Basic.Kernel.Base.Context
import ZSnd.Basic.Kernel.Base.WrappedPrimitive
import ZSnd.Basic.Kernel.Objects.AdvNotelist
import ZSnd.Basic.Kernel.Objects.Basis
import ZSnd.Basic.Kernel.Objects.LocEvent
import ZSnd.Basic.Kernel.Objects.OnsetNotelist
import ZSnd.Basic.Kernel.Objects.TraceNotelist

import ZSnd.Core                                -- package: zsnd-core


--
-- Note - because we don\'t load font metrics from file, sharing 
-- context is not so valuable...
--


traceNotelist :: Context uctx -> Notelist uctx u a -> Maybe Score
traceNotelist ctx mf = liftToScoreMb $ execNotelist ctx mf


traceNotelistU :: Context uctx -> Notelist uctx u a -> Score
traceNotelistU ctx mf = maybe fk id $ traceNotelist ctx mf
  where
    fk = error "traceNotelistU - emptyScore." 




traceAdvNotelist :: InterpretUnit u
                 => Context ctx -> AdvNotelist ctx u a -> Maybe Score
traceAdvNotelist ctx mf = 
    let (PrimW ca _) = runLocEvent 0 ctx $ execAdvNotelist ctx mf
    in liftToScoreMb $ singleH ca

traceAdvNotelistU :: InterpretUnit u
                  => Context ctx -> AdvNotelist ctx u a -> Score
traceAdvNotelistU ctx mf = maybe fk id $ traceAdvNotelist ctx mf
  where
    fk = error "traceAdvNotelistU - emptyScore." 



traceOnsetNotelist :: InterpretUnit u
                   => Context ctx -> OnsetNotelist ctx u a -> Maybe Score
traceOnsetNotelist ctx mf = 
    let (PrimW ca _) = runLocEvent 0 ctx $ execOnsetNotelist ctx mf
    in liftToScoreMb $ singleH ca

traceOnsetNotelistU :: InterpretUnit u
                    => Context ctx -> OnsetNotelist ctx u a -> Score
traceOnsetNotelistU ctx mf = maybe fk id $ traceOnsetNotelist ctx mf
  where
    fk = error "traceOnsetNotelistU - emptyScore." 




-- | Promotion of @HPrim@ to @Picture@.
--
-- 
liftToScoreMb :: HPrim u -> Maybe Score
liftToScoreMb hf = let prims = hprimToList hf in 
                   if null prims then Nothing else Just (frame prims)

{-

score1 :: [NoteStmt] -> Score
score1 xs = Score { score_duration = da
                  , score_sequence = fa }
  where
    (da,fa) = foldl' fn (0, mf0) $ sortBy orderNote xs
  
    fn (d0,f) (NoteStmt t d mf) = (max d0 (t+d), (\t0 -> f t0 >> mf (t0 + t) d))

    mf0 _ = return ()

--------------------------------------------------------------------------------
-- concat

emptyScore :: Score 
emptyScore = Score { score_duration   = 0
                   , score_sequence   = (\_ -> return ())
                   }


instance Monoid Score where
  mempty  = emptyScore
  mappend = overlayScore


overlayScore :: Score -> Score -> Score 
overlayScore (Score d0 f0) (Score d1 f1) = 
    Score { score_duration = max d0 d1
          , score_sequence = (\v -> f0 v >> f1 v)
          }

instance Overlay Score where
  overlay = overlayScore


instance Append Score where
  append = appendScore

appendScore :: Score -> Score -> Score 
appendScore (Score d0 f0) (Score d1 f1) = 
    Score { score_duration = d0 + d1
          , score_sequence = (\v -> f0 v >> f1 (v + d0))
          }



instance Space Score where
  space = spaceScore

spaceScore :: Double -> Score -> Score -> Score 
spaceScore w (Score d0 f0) (Score d1 f1) = 
    Score { score_duration = d0 + w + d1
          , score_sequence = (\v -> f0 v >> f1 (v + w + d0))
          }



-}