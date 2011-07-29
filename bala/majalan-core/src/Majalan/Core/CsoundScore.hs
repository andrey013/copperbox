{-# LANGUAGE TypeFamilies               #-}
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

    RScore
  , Note
  , runScore

  -- * Re-exports
  , GenStmt(..)
  , InstStmtProps(..)
  , AbsPrimStmt                 -- opaque
  , PrimStmt                    -- opaque
  , CsValue(..)
  
  -- * Build scores
  , frame 

  , absNote

  , prefixGens

  , scoOver
  , scoMoveBy
  , scoBeside

  , extendTimeFrame

  , printScore
 
  ) where

import Majalan.Core.Basis
import Majalan.Core.ScoreInternal
import Majalan.Core.Timespan
import Majalan.Core.Utils.FormatCombinators ( format )
import qualified Majalan.Core.Utils.JoinList as JL

import Data.List ( sortBy )


newtype RScore env = RScore { getRScore :: env -> Score }

newtype Note env  = Note  { getNote :: env -> AbsPrimStmt } 

type instance DUnit (RScore env) = Double

type instance DUnit (Note env) = Double

--
-- The same env for instr number lookup must be used for scores 
-- built by combining (sub-) scores. Thus a function from 
-- @env -> Score@ properly models numbering.
--

--
-- Note - Score and Note are more restricted than a Reader monad,
-- they only produce answers of the /natural/ type.
--
-- Is there any virtue to them being more general, e.g. Reader 
-- plus a Writer collecting elements?
--


runScore :: env -> RScore env -> Score
runScore env = getRScore `flip` env

-- | Lift a list of primitives to a score.
--
-- \*\* WARNING \*\* - this function throws a runtime error when 
-- supplied the empty list.
--
frame :: [Note env] -> RScore env
frame [] = error "Majalan.Core.CsoundScore.frame - empty list"
frame xs = RScore $ \env -> step0 $ sortBy cmp $ map (getNote `flip` env) xs
  where 
    cmp a b = compare (absPrimStart a) (absPrimStart b)

    step0 []                          = error "frame - unreachable" 

    step0 (AbsInstStmt t0 props :es)  = 
        step1 (t0, t0 + inst_dur props) t0 (JL.one $ InstStmt 0 props) es

    step1 (t0,t1) _ ac []                          = 
        Leaf (Timespan t0 t1, standardFrame, []) ac

    step1 (t0,t1) ot ac (AbsInstStmt abst props :es) = 
        let dt  = abst - ot
            t1' = max t1 (abst + inst_dur props)
        in step1 (t0,t1') abst (ac `JL.snoc` InstStmt dt props) es



-- Score has an Env mapping instr nums.


-- | 'absEvent' : @ numF * onset_time * props -> AbsPrimStmt @
-- 
-- Create a note statement (@i-statment@).
--
absNote :: (env -> Int) -> Double -> Double -> [CsValue] -> Note env 
absNote numF otim dur vals = Note $ \env -> 
    AbsInstStmt otim (InstStmtProps { inst_num = numF env
                                    , inst_dur = dur
                                    , inst_params = vals })

--
-- absNote is a problem. We need notes to have the same type
-- regardless of instrument, otherwise scores will be horrible. 
--
-- Passing a function to get instr_num from the env seems to be
-- the only adequate way of enabling this.
--


-- | Prefix a Score with f-statements.
--
prefixGens :: [GenStmt] -> RScore env -> RScore env
prefixGens gs sco = RScore $ \env -> foldr consGenStmt (getRScore sco env) gs



infixr 6 `scoOver`, `scoBeside`

-- | 'scoOver' : @ score * score -> SCore @
--
-- Combine the scores by playing them simultaneously.
-- The onsets of both scores are unchanged.
--
scoOver :: RScore env -> RScore env -> RScore env
a `scoOver` b = RScore $ \env -> 
    let s1 = getRScore a env
        s2 = getRScore b env
    in joinScore s1 s2



-- | 'scoMoveBy' : @ score * time -> RScore @
-- 
--  Move a score by the supplied time. 
-- 
-- The result time should be @>= 0@, thought this is not enforced.
--
scoMoveBy :: RScore env -> Double -> RScore env
scoMoveBy sco dx = RScore $ \env -> moveScore dx $ getRScore sco env


-- | 'scoBeside' : @ score * score -> RScore @
--
-- Combine the scores by playing them sequentially.
-- The second score is played after then first.
--
scoBeside :: RScore env -> RScore env -> RScore env
a `scoBeside` b = RScore $ \env -> 
    let s1 = getRScore a env
        s2 = getRScore b env
        x1 = timespan_end   $ timeframe s1
        x2 = timespan_start $ timeframe s2
        dx = x1 - x2 
    in s1 `joinScore` (moveScore dx s2)
        
    


-- | 'extendTimeFrame' : @ initial_delay * epilogue_delay * score -> Score @
--
-- Add an initial delay and an epilogue delay to the score extending
-- its timespan.
--
extendTimeFrame :: Double -> Double -> RScore env -> RScore env
extendTimeFrame d0 d1 sco = RScore $ \env -> 
    extendScoreTime d0 d1 $ getRScore sco env


-- | Print the syntax tree of a Score to the console.
--
printScore :: env -> RScore env -> IO ()
printScore env sco = 
    putStrLn (show $ format $ getRScore sco env) >> putStrLn []

