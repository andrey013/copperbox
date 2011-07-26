{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Majalan.Core.CsoundScore
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Build Csound Scores.
--
--------------------------------------------------------------------------------

module Majalan.Core.CsoundScore
  (

    Score
  , Note
  , runScore

  -- * Re-exports
  , GenStmt(..)
  , InstStmtProps(..)
  , AbsPrimStmt                 -- opaque
  , PrimStmt                    -- opaque
  
  -- * Build scores
  , frame 

  , absNote

  , withGens

  , scoOver
  , scoMoveBy
  , scoBeside

  , extendTimeFrame

  , printScore
 
  ) where

import Majalan.Core.ScoreInternal
import Majalan.Core.Utils.FormatCombinators ( format )
import qualified Majalan.Core.Utils.JoinList as JL

import Data.List ( sortBy )


newtype Score env = Score { getScore :: env -> PrimScore }

newtype Note env  = Note  { getNote :: env -> AbsPrimStmt } 


runScore :: env -> Score env -> PrimScore
runScore env = getScore `flip` env

-- | Lift a list of primitives to a score.
--
-- \*\* WARNING \*\* - this function throws a runtime error when 
-- supplied the empty list.
--
frame :: [Note env] -> Score env
frame [] = error "Majalan.Core.CsoundScore.frame - empty list"
frame xs = Score $ \env -> step0 $ sortBy cmp $ map (getNote `flip` env) xs
  where 
    cmp a b = compare (absPrimStart a) (absPrimStart b)

    step0 []                          = error "frame - unreachable" 

    step0 (AbsInstStmt t0 props :es)  = 
        step1 (t0, t0 + inst_dur props) t0 (JL.one $ InstStmt 0 props) es

    step1 (t0,t1) _ ac []                          = 
        PrimLeaf (Timespan t0 t1, standardFrame, []) ac

    step1 (t0,t1) ot ac (AbsInstStmt abst props :es) = 
        let dt  = abst - ot
            t1' = max t1 (abst + inst_dur props)
        in step1 (t0,t1') abst (ac `JL.snoc` InstStmt dt props) es



-- Does Score share an Env mapping instr nums, or is a frame rendered
-- with an Env forming a Score?

-- | 'absEvent' : @ numF * onset_time * props -> AbsPrimStmt @
-- 
-- Create a note statement (@i-statment@).
--
absNote :: (env -> Int) -> Double -> Double -> [CsValue] -> Note env 
absNote numF otim dur vals = Note $ \env -> 
    AbsInstStmt otim (InstStmtProps { inst_num = numF env
                                    , inst_dur = dur
                                    , inst_params = vals })




withGens :: [GenStmt] -> Score env -> Score env
withGens gs sco = Score $ \env -> case getScore sco env of
    PrimScore loc body -> PrimScore (ext loc) body
    PrimLeaf  loc body -> PrimLeaf (ext loc) body
  where
    ext (tim,fr,ys) = (tim,fr,gs ++ ys)



infixr 6 `scoOver`, `scoBeside`

-- | 'scoOver' : @ score * score -> SCore @
--
-- Combine the scores by playing them simultaneously.
-- The onsets of both scores are unchanged.
--
scoOver :: Score env -> Score env -> Score env
a `scoOver` b = Score $ \env -> 
    let s1 = getScore a env
        s2 = getScore b env
        tspan = scoreTimespan s1 `timespanUnion` scoreTimespan s2
    in PrimScore (tspan,standardFrame,[]) (JL.join (JL.one s1) (JL.one s2))



-- | 'scoMoveBy' : @ score * time -> SCore @
-- 
--  Move a score by the supplied time. 
-- 
-- The result time should be @>= 0@, thought this is not enforced.
--
scoMoveBy :: Score env -> Double -> Score env
scoMoveBy sco dx = Score $ \env -> case getScore sco env of
    PrimScore loc body -> PrimScore (moveLocale dx loc) body
    PrimLeaf  loc body -> PrimLeaf (moveLocale dx loc) body



-- | 'scoBeside' : @ score * score -> Score @
--
-- Combine the scores by playing them sequentially.
-- The second score is played after then first.
--
scoBeside :: Score env -> Score env -> Score env
a `scoBeside` b = Score $ \env -> 
    let x1 = timespan_end $ scoreTimespan $ getScore a env
        x2 = timespan_start $ scoreTimespan $ getScore b env
        dx = x1 - x2 
    in getScore (a `scoOver` (b `scoMoveBy` dx)) env
        
    


-- | 'extendTimeFrame' : @ initial_delay * epilogue_delay * score -> Score @
--
-- Combine the scores by playing them sequentially.
-- The second score is played after then first.
--
extendTimeFrame :: Double -> Double -> Score env -> Score env
extendTimeFrame d0 d1 sco = Score $ \env -> case getScore sco env of 
   PrimScore (tspan,fr,gs) body -> PrimScore (extT tspan, extF fr,gs) body
   PrimLeaf  (tspan,fr,gs) body -> PrimLeaf  (extT tspan, extF fr,gs) body
  
  where
    extT (Timespan a0 a1) = Timespan a0 (a1 + d0 + d1)
    extF (Frame o sx)     = Frame (o+d0) sx



-- | Print the syntax tree of a Score to the console.
--
printScore :: env -> Score env -> IO ()
printScore env sco = 
    putStrLn (show $ format $ getScore sco env) >> putStrLn []

