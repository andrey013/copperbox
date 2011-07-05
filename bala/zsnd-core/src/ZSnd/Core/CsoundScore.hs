{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZSnd.Core.CsoundScore
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

module ZSnd.Core.CsoundScore
  (

  -- * Re-exports
    Score(..)
  , GenStmtProps(..)
  , InstStmtProps(..)
  , AbsPrimStmt                 -- opaque
  , PrimStmt                    -- opaque


  -- * Build scores
  , frame 

  , absTableGen
  , absEvent

  , scoOver
  , scoMoveBy
  , scoBeside

  , extendTimeFrame

  , printScore
 
  ) where

import ZSnd.Core.ScoreInternal
import ZSnd.Core.Utils.FormatCombinators ( format )
import qualified ZSnd.Core.Utils.JoinList as JL

import Data.List ( sortBy )

-- | Lift a list of primitives to a score.
--
-- \*\* WARNING \*\* - this function throws a runtime error when 
-- supplied the empty list.
--
frame :: [AbsPrimStmt] -> Score
frame [] = error "Zsnd.Core.CsoundScore.frame - empty list"
frame xs = step0 $ sortBy cmp xs 
  where 
    cmp a b = compare (absPrimStart a) (absPrimStart b)

    step0 []                          = error "frame - unreachable" 
    step0 (AbsTableStmt ot props :es) = 
        step1 (ot,0) ot (JL.one $ TableStmt 0 props) es

    step0 (AbsInstStmt  ot props :es) = 
        step1 (ot,inst_dur props) ot (JL.one $ InstStmt 0 props) es


    step1 (t0,d) _ ac []                          = 
        Leaf (Timespan t0 (t0+d), standardFrame) ac

    step1 (t0,d) t1 ac (AbsTableStmt ot props :es) = 
        step1 (t0,d) ot (JL.snoc ac $ TableStmt (ot - t1) props) es

    step1 (t0,d) t1 ac (AbsInstStmt ot props :es) = 
        let dnew = d + (ot - t1) + inst_dur props
        in step1 (t0,dnew) ot (JL.snoc ac $ InstStmt (ot - t1) props) es
   

-- | 'absTableGen' : @ onset_time * props -> AbsPrimStmt @
-- 
-- Create a function statement (@f-statment@).
--
absTableGen :: Double -> GenStmtProps -> AbsPrimStmt
absTableGen = AbsTableStmt


-- | 'absEvent' : @ onset_time * props -> AbsPrimStmt @
-- 
-- Create a note statement (@i-statment@).
--
absEvent :: Double -> InstStmtProps -> AbsPrimStmt
absEvent = AbsInstStmt





infixr 6 `scoOver`, `scoBeside`

-- | 'scoOver' : @ score * score -> SCore @
--
-- Combine the scores by playing them simultaneously.
-- The onsets of both scores are unchanged.
--
scoOver :: Score -> Score -> Score
a `scoOver` b = Score (tspan,standardFrame) (JL.join (JL.one b) (JL.one a))   
  where
    tspan = scoreTimespan a `timespanUnion` scoreTimespan b



-- | 'scoMoveBy' : @ score * time -> SCore @
-- 
--  Move a score by the supplied time. 
-- 
-- The result time should be @>= 0@, thought this is not enforced.
--
scoMoveBy :: Score -> Double -> Score
scoMoveBy (Score loc body) dx = Score (moveLocale dx loc) body
scoMoveBy (Leaf  loc body) dx = Leaf (moveLocale dx loc) body



-- | 'scoBeside' : @ score * score -> Score @
--
-- Combine the scores by playing them sequentially.
-- The second score is played after then first.
--
scoBeside :: Score -> Score -> Score
a `scoBeside` b = a `scoOver` (b `scoMoveBy` dx) 
  where 
    x1 = timespan_end $ scoreTimespan a
    x2 = timespan_start $ scoreTimespan b 
    dx = x1 - x2 



-- | 'extendTimeFrame' : @ initial_delay * epilogue_delay * score -> Score @
--
-- Combine the scores by playing them sequentially.
-- The second score is played after then first.
--
extendTimeFrame :: Double -> Double -> Score -> Score
extendTimeFrame d0 d1 sco = step sco 
  where
    step (Score (tspan,fr) body) = Score (extT tspan, extF fr) body
    step (Leaf  (tspan,fr) body) = Leaf  (extT tspan, extF fr) body
  
    extT (Timespan a0 a1) = Timespan a0 (a1 + d0 + d1)
    
    extF (Frame o sx)     = Frame (o+d0) sx


-- | Print the syntax tree of a Score to the console.
--
printScore :: Score -> IO ()
printScore pic = putStrLn (show $ format pic) >> putStrLn []