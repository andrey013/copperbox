{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZSnd.Basic.Kernel.Base.BaseDefs
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
--
--------------------------------------------------------------------------------

module ZSnd.Basic.Kernel.Base.BaseDefs
  ( 

    DUnit
  , OnsetDbl
  , Tempo
  , UNil(..)
  
  , InterpretUnit(..)
  , normalizeF
  , dinterpF
  , uconvert1
  , uconvertF
  
  ) where


import ZSnd.Core                                -- package: zsnd-core

import Data.Monoid



type family DUnit u :: *

type OnsetDbl = Double

newtype Tempo = Tempo { getTempo :: Double }
  deriving (Eq,Ord,Num,Floating,Fractional,Real,RealFrac,RealFloat)

instance Show Tempo where
  showsPrec p d = showsPrec p (getTempo d)



-- | The empty data type - i.e. @()@ - wrapped with a phantom unit 
-- parameter.
--
data UNil   u = UNil          deriving (Eq,Ord,Read,Show)

type instance DUnit (UNil u) = u

type instance DUnit Score = Double


instance Functor UNil where
  fmap _ UNil = UNil


instance Monoid (UNil u) where
  mempty        = UNil
  _ `mappend` _ = UNil




--
-- Design note 
--
-- Having UNil as a unit-parametric answer type is important to 
-- Wumpus. In Wumpus, typical answer types are BoundingBox or Vec2 
-- both are parametric on unit  with unit at the /Functor position/
-- in the type. As the other typical answer type is null @()@, 
-- Wumpus defines UNil, a unit-parametric phantom type so answers 
-- can always be functors with unit.
--
-- ZSnd doesn\'t seem to naturally favour funtorial answer types - 
-- the most important answers are expected to be null @()@ and 
-- duration (really a \"scalar\" not a functor). But scalar 
-- answers are problematic for unit changing functions - to allow
-- unit changing, we would need a signature like this:
--
-- > Event ctx u a -> Event ctx u1 b
--
-- But this type is completely unrestricted - the answer could be 
-- changed to a completely arbitrary type during unit change when 
-- unit change is supposed to only change the unit of duration 
-- (e.g. changing symbolic durations to Double representing 
-- seconds).
--
-- To add some structure, it seems preferable to follow Wumpus and
-- have all answers functors (so we need UNil and Duration). 
-- Although this adds quite substantial overhead, it does mean 
-- unit change is structure preserving.
--
--



class Num u => InterpretUnit u where
  normalize :: Tempo -> u -> Double
  dinterp   :: Tempo -> Double -> u

instance InterpretUnit Double where
  normalize _ = id
  dinterp _   = id


-- | 'normalize' an object that gives access to its unit at the 
-- functor position.
--
normalizeF :: (Functor t, InterpretUnit u) => Tempo -> t u -> t Double
normalizeF bpm = fmap (normalize bpm)

-- | 'dinterp' an object that gives access to its unit at the 
-- functor position.
--
dinterpF :: (Functor t, InterpretUnit u) => Tempo -> t Double -> t u
dinterpF bpm = fmap (dinterp bpm)

-- | Convert a scalar value from one unit to another.
--
uconvert1 :: (InterpretUnit u, InterpretUnit u1) => Tempo -> u -> u1
uconvert1 bpm = dinterp bpm . normalize bpm

-- | Unit convert an object that gives access to its unit at the
-- Functor position.
--
-- In practive this will be \*all\* Event answers.
--
uconvertF :: (Functor t, InterpretUnit u, InterpretUnit u1) 
          => Tempo -> t u -> t u1
uconvertF bpm = fmap (uconvert1 bpm)



{-

-- | Onset time should always be positive.
--
-- @note_length@ gives us the length of the event without us 
-- having to run it. This is essential for summing the length of
-- a list of notes before we renderer them - for score composition 
-- it would be computationally expensive to render the list then 
-- move all the elements. 
--  
-- 
data NoteStmt = NoteStmt 
      { onset_time    :: OnsetDbl
      , note_length   :: Double
      , event_gen     :: OnsetDbl -> AbsPrimStmt
      }
      
      -- , onset_time  :: Double

orderNote :: NoteStmt -> NoteStmt -> Ordering
orderNote (NoteStmt t1 _ _) (NoteStmt t2 _ _) = compare t1 t2


-}