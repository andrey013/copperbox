{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZMidi.Basic.Kernel.Base.BaseDefs
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Wrapped Primitives supporting concatenation.
--
--------------------------------------------------------------------------------

module ZMidi.Basic.Kernel.Base.BaseDefs
  ( 

  -- * Alias for mappend
    (><)

  -- * Types and aliases
  , OnsetTime
  , BPM
  , GMInst
  , GMDrum

  , InterpretUnit(..)
  , uconvert

  , UState
  , UserStateM(..)

  ) where



import Control.Applicative
import Data.Monoid
import Data.Word


infixr 5 ><

-- | @concat@
--
-- > infixr 5 ><
--
(><) :: Monoid a => a -> a -> a
(><) = mappend



-- | Internally represent Onset times (and durations) as Double.
-- 
-- Only in a final conversion step are ticks (MIDI\'s 
-- representation) used.
-- 
type OnsetTime = Double


-- | Beats per minute - just an alias to Double.
--
type BPM = Double


-- | Enumeration of the General MIDI instruments.
--
type GMInst = Word8



-- | Enumeration of the General MIDI drum types.
--
type GMDrum = Word8



class Num u => InterpretUnit u where
  normalize  :: BPM -> u -> Double
  dinterp    :: BPM -> Double -> u


instance InterpretUnit Double where
  normalize _ = id
  dinterp _   = id



-- | Convert a scalar value from one unit to another.
--
uconvert :: (InterpretUnit u, InterpretUnit u1) => BPM -> u -> u1
uconvert bpm = dinterp bpm . normalize bpm


-- | Some type of User State.
-- 
-- This very useful for reducing the kind of the UserState class 
-- to @(* -> *)@.
--  
type family UState m :: *


class (Applicative m, Monad m) => UserStateM (m :: * -> *) where
  getState    :: st ~ UState m  => m st
  setState    :: st ~ UState m  => st -> m ()
  updateState :: st ~ UState m  => (st -> st) -> m ()

--------------------------------------------------------------------------------




