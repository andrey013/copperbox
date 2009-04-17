{-# OPTIONS -Wall #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Mullein.Core
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Common functions operating on core types
--
--------------------------------------------------------------------------------

module Mullein.Core where

import Mullein.CoreTypes
import Mullein.Duration
import Mullein.ScoreSyntax ( Element(..) )
import Mullein.Utils

import Control.Monad.State
import Data.Ratio


--------------------------------------------------------------------------------
-- Note lists

type NoteCtx a = State NoteListCtx a

-- NoteListCtx represents /shorthand state/ so we can omit
-- some details when building the notelist (e.g. duration) 
data NoteListCtx = NoteListCtx 
      { unit_note_length :: Duration }
  deriving (Eq,Show)


notelist :: [NoteCtx Element] -> [Element]
notelist fs = evalState (sequence fs) ctx0 where
    ctx0 = NoteListCtx { unit_note_length = 1%4 }

(&) :: NoteCtx Element -> NoteCtx () -> NoteCtx Element
(&) f upd  = upd >> f 

-- Building overlays

type BarNum = Int
type OverlayList = (NoteList, [(BarNum,NoteList)])

primary :: NoteList -> OverlayList
primary xs = (xs,[])

addOverlay :: BarNum -> NoteList -> OverlayList -> OverlayList
addOverlay n xs (p,xss) = (p,(n,xs):xss)




--------------------------------------------------------------------------------
-- Musical representation




--------------------------------------------------------------------------------
-- Meter

meterFraction :: Meter -> Duration
meterFraction (TimeSig n d) = n%d
meterFraction CommonTime    = 4%4 
meterFraction CutTime       = 2%2 
           

metricalSpec :: Int -> Int -> MetricalSpec
metricalSpec n d 
      | compoundMeter  n d  = (time_sig, replicate 3 $ (rational n d) / 3)
      | simpleMeter n d     = (time_sig, replicate n (rational 1 d))
      | otherwise           = error $ err_msg
  where
    time_sig = TimeSig (fromIntegral n) (fromIntegral d)
 
    err_msg = "simpleMetricalSpec - can't generate a meter pattern for a "
           ++ "meter that is neither simple or compound."



-- Note compoundMeter and simpleMeter overlap

compoundMeter :: Integral a => a -> a -> Bool
compoundMeter n d = log2whole d && (n `mod` 3 == 0)
         
simpleMeter :: Integral a => a -> a -> Bool
simpleMeter _ d = log2whole d

log2whole :: Integral a => a -> Bool
log2whole = (==0) . snd . pf . logBase 2 . fromIntegral where
    pf :: Double -> (Int, Double)
    pf = properFraction
        
--------------------------------------------------------------------------------
-- pitch labels        

-- in module LabelSet
