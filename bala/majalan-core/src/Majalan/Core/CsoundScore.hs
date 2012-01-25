{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Majalan.Core.CsoundScore
-- Copyright   :  (c) Stephen Tetley 2012
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

    InstrMap
  , buildInstrMap


  , FailMsg
  , ScoreM

  , RScore
  , Note
  , runScoreM
  , runScoreU

  -- * Re-exports
  , Score
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

import Control.Applicative
import qualified Data.ByteString.Char8 as B
import Data.List ( sortBy )
import qualified Data.Map as Map



--------------------------------------------------------------------------------
-- Instr map - per score assignment of instrument numbers

newtype InstrMap = InstrMap { getInstrMap :: Map.Map B.ByteString Int }

instance Show InstrMap where
  show = show . map swap . Map.toList . getInstrMap
    where
      swap (a,b) = (b,a)

-- | Note - throws a runtime error if the same instruemtn is 
-- assigned multiple numbers.
--
buildInstrMap :: [(Int,B.ByteString)] -> InstrMap
buildInstrMap xs = InstrMap $ foldr fn Map.empty xs
  where
    fn (i,bs) ac = case Map.lookup bs ac of 
                     Nothing -> Map.insert bs i ac
                     Just j  -> error $ err_msg i j (show bs)
    
    err_msg i j ss = "buildInstrMap - multiple assignments to " ++ ss 
                       ++ " " ++ show i ++ " and " ++ show j




--------------------------------------------------------------------------------

type FailMsg = String

newtype ScoreM a = ScoreM { getSM :: InstrMap -> Either FailMsg a }

instance Functor ScoreM where
  fmap f mf = ScoreM $ \r -> fmap f $ getSM mf r

instance Applicative ScoreM where
  pure a    = ScoreM $ \_ -> Right a
  mf <*> ma = ScoreM $ \r -> getSM mf r <*> getSM ma r

instance Monad ScoreM where
  return a  = ScoreM $ \_ -> Right a
  m >>= k   = ScoreM $ \r -> getSM m r >>= \a -> getSM (k a) r


runScoreM :: InstrMap -> ScoreM a -> Either FailMsg a
runScoreM instrs mf = getSM mf instrs



-- | Unsafe version of 'runScoreM'.
--
runScoreU :: InstrMap -> ScoreM a -> a
runScoreU instrs mf = either fk id $ getSM mf instrs
  where
    fk err = error $ "runScoreU - " ++ err



type RScore = ScoreM Score

type Note   = ScoreM AbsPrimStmt

type instance DUnit (ScoreM a) = DUnit a


--
-- The same env for instr number lookup must be used for scores 
-- built by combining (sub-) scores. Thus a function from 
-- @env -> Score@ properly models numbering.
--



-- | Lift a list of primitives to a score.
--
-- \*\* WARNING \*\* - this function throws a runtime error when 
-- supplied the empty list.
--
frame :: [Note] -> RScore
frame []     = error "Majalan.Core.CsoundScore.frame - empty list"
frame (x:xs) = fmap frame2 $ step x xs
  where
    step mf []     = liftA  (\a -> [a]) mf
    step mf [y]    = liftA2 (\a b -> [a,b]) mf y
    step mf (y:ys) = liftA2 (:) mf (step y ys)


frame2 :: [AbsPrimStmt] -> Score
frame2 = step0 . sortBy cmp 
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
absNote :: B.ByteString -> Double -> Double -> [CsValue] -> Note
absNote key otim dur vals = ScoreM $ \r ->
    case Map.lookup key (getInstrMap r) of
      Nothing -> Left err_msg 
      Just ix -> Right $ AbsInstStmt otim (InstStmtProps { inst_num = ix
                                                         , inst_dur = dur
                                                         , inst_params = vals })
  where
    err_msg = "absNote - missing key " ++ show key

--
-- absNote is a problem. We need notes to have the same type
-- regardless of instrument, otherwise scores will be horrible. 
--
-- Passing a function to get instr_num from the env seems to be
-- the only adequate way of enabling this.
--


-- | Prefix a Score with f-statements.
--
prefixGens :: [GenStmt] -> RScore -> RScore
prefixGens gs sco = (\ac -> foldr consGenStmt ac gs) <$> sco



infixr 6 `scoOver`, `scoBeside`

-- | 'scoOver' : @ score * score -> SCore @
--
-- Combine the scores by playing them simultaneously.
-- The onsets of both scores are unchanged.
--
scoOver :: RScore -> RScore -> RScore
scoOver = liftA2 joinScore
 


-- | 'scoMoveBy' : @ score * time -> RScore @
-- 
--  Move a score by the supplied time. 
-- 
-- The result time should be @>= 0@, thought this is not enforced.
--
scoMoveBy :: RScore -> Double -> RScore
scoMoveBy sco dx = fmap (moveScore dx) sco


-- | 'scoBeside' : @ score * score -> RScore @
--
-- Combine the scores by playing them sequentially.
-- The second score is played after then first.
--
scoBeside :: RScore -> RScore -> RScore
scoBeside = liftA2 fn 
  where
    fn s1 s2 = let x1 = timespan_end   $ timeframe s1
                   x2 = timespan_start $ timeframe s2
                   dx = x1 - x2 
               in s1 `joinScore` (moveScore dx s2)
        
    


-- | 'extendTimeFrame' : @ initial_delay * epilogue_delay * score -> Score @
--
-- Add an initial delay and an epilogue delay to the score extending
-- its timespan.
--
extendTimeFrame :: Double -> Double -> RScore -> RScore
extendTimeFrame d0 d1 = fmap (extendScoreTime d0 d1)


-- | Print the syntax tree of a Score to the console.
--
printScore :: InstrMap -> RScore -> IO ()
printScore instrs sco =
    either fk sk $ runScoreM instrs sco 
  where
    fk err = putStrLn err
    sk ans = putStrLn (show $ format ans) >> putStrLn []

