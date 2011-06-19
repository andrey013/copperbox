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
    Score
  , runScore
  , traceNotelist
  , traceAdvNotelist

    
  ) where


import ZSnd.Basic.Kernel.Base.BaseDefs
import ZSnd.Basic.Kernel.Base.Context
import ZSnd.Basic.Kernel.Base.WrappedPrimitive
import ZSnd.Basic.Kernel.Objects.AdvNotelist
import ZSnd.Basic.Kernel.Objects.Basis
import ZSnd.Basic.Kernel.Objects.Concat
import ZSnd.Basic.Kernel.Objects.LocEvent
import ZSnd.Basic.Kernel.Objects.TraceNotelist

import ZSnd.Core                                -- package: zsnd-core


import Data.List 
import Data.Monoid




data Score = Score 
      { score_duration   :: Double
      , score_sequence   :: OnsetDbl -> ScoBuilder ()
      }


type instance DUnit Score = Double

-- Note runScore builds a one element list.

runScore :: Score -> Section
runScore sco = runScoBuilder (score_sequence sco $ 0)

traceNotelist :: ctx -> Notelist ctx u a -> Score
traceNotelist ctx mf = liftToScore $ execNotelist ctx mf

traceAdvNotelist :: (CtxTempo ctx, InterpretUnit u) 
                 => ctx -> AdvNotelist ctx u a -> Score
traceAdvNotelist ctx mf = 
    let (PrimW ca _) = runLocEvent 0 ctx $ execAdvNotelist ctx mf
    in liftToScore $ singleH ca



-- Note unlike Wumpus, empty note lists pose no problem for ZSnd.



-- | Promotion of @HPrim@ to @Picture@.
--
-- 
liftToScore :: HPrim u -> Score
liftToScore = score1 . hprimToList



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



