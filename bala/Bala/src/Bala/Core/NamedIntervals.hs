{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Core.NamedIntervals
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Pitch represention
--
--------------------------------------------------------------------------------

module Bala.Core.NamedIntervals
  ( 

  -- ** Named intervals
    unison
  , major2
  , major3
  , major6
  , major7
  , major9
  , major10
  , minor2
  , minor3
  , minor6
  , minor7
  , minor9
  , minor10
  , perfect1
  , perfect4
  , perfect5
  , perfect8
  , perfect11
  , perfect12
  , augmented2
  , augmented3
  , augmented4
  , augmented5
  , augmented6
  , augmented7
  , augmented8
  , augmented9
  , augmented11
  , augmented12
  , augmented13
  , diminished2
  , diminished3
  , diminished4
  , diminished5
  , diminished6
  , diminished7
  , diminished8
  , diminished9
  , diminished11
  , diminished12
  , diminished13
  , octave1
  , octave2

  ) where

import Bala.Core.Interval

import Data.Monoid

unison              :: Interval
unison              = makeInterval 1 0

major2              :: Interval
major3              :: Interval
major6              :: Interval
major7              :: Interval
major9              :: Interval
major10             :: Interval
major2              = makeInterval 2 2
major3              = makeInterval 3 4
major6              = makeInterval 6 9
major7              = makeInterval 7 11
major9              = perfect8 `mappend` major2
major10             = perfect8 `mappend` major3

minor2              :: Interval
minor3              :: Interval
minor6              :: Interval
minor7              :: Interval
minor9             :: Interval
minor10             :: Interval
minor2              = makeInterval 2 1
minor3              = makeInterval 3 3
minor6              = makeInterval 6 8
minor7              = makeInterval 7 10
minor9              = perfect8 `mappend` minor2
minor10             = perfect8 `mappend` minor3

perfect1            :: Interval 
perfect4            :: Interval 
perfect5            :: Interval 
perfect8            :: Interval 
perfect11           :: Interval
perfect12           :: Interval
perfect1            = makeInterval 1 0
perfect4            = makeInterval 4 5
perfect5            = makeInterval 5 7
perfect8            = makeInterval 8 12
perfect11           = perfect8 `mappend` perfect4
perfect12           = perfect8 `mappend` perfect5


augmented2          :: Interval
augmented3          :: Interval
augmented4          :: Interval
augmented5          :: Interval
augmented6          :: Interval
augmented7          :: Interval
augmented8          :: Interval
augmented9          :: Interval
augmented11         :: Interval
augmented12         :: Interval
augmented13         :: Interval
augmented2          = makeInterval 2 3
augmented3          = makeInterval 3 5
augmented4          = makeInterval 4 6
augmented5          = makeInterval 5 8
augmented6          = makeInterval 6 10
augmented7          = makeInterval 7 12
augmented8          = makeInterval 8 13
augmented9          = perfect8 `mappend` augmented2
augmented11         = perfect8 `mappend` augmented4 
augmented12         = perfect8 `mappend` augmented5
augmented13         = perfect8 `mappend` augmented6 



diminished2         :: Interval
diminished3         :: Interval
diminished4         :: Interval
diminished5         :: Interval
diminished6         :: Interval
diminished7         :: Interval
diminished8         :: Interval
diminished9         :: Interval
diminished11        :: Interval
diminished12        :: Interval
diminished13        :: Interval
diminished2         = makeInterval 2 0
diminished3         = makeInterval 3 2
diminished4         = makeInterval 4 4
diminished5         = makeInterval 5 6
diminished6         = makeInterval 6 7
diminished7         = makeInterval 7 9
diminished8         = makeInterval 8 11
diminished9         = perfect8 `mappend` diminished2
diminished11        = perfect8 `mappend` diminished4 
diminished12        = perfect8 `mappend` diminished5
diminished13        = perfect8 `mappend` diminished6 

-- naming changes for these
octave1             :: Interval
octave2             :: Interval
octave1             = makeInterval 8 12
octave2             = makeInterval 15 24

