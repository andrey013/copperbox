{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Core.TonalNamed
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Named elements
--
--------------------------------------------------------------------------------

module Bala.Core.TonalNamed where

import Bala.Core.Tonal

import Data.AdditiveGroup

c5                  :: Pitch
c5                  = Pitch C 60

d5                  :: Pitch
d5                  = Pitch D 62

e5                  :: Pitch
e5                  = Pitch E 64

f5                  :: Pitch
f5                  = Pitch F 65

g5                  :: Pitch
g5                  = Pitch G 67

a5                  :: Pitch
a5                  = Pitch A 69

b5                  :: Pitch
b5                  = Pitch B 71





unison              :: Interval
unison              = Interval 1 0

octave              :: Interval
octave              = Interval 8 12

-- major
major2              :: Interval
major2              = Interval 2 2

major3              :: Interval
major3              = Interval 3 4

major6              :: Interval
major6              = Interval 6 9

major7              :: Interval
major7              = Interval 7 11

major9              :: Interval
major9              = octave ^+^ major2

major10             :: Interval
major10             = octave ^+^ major3


-- minor 
minor2              :: Interval
minor2              = Interval 2 1

minor3              :: Interval
minor3              = Interval 3 3

minor6              :: Interval
minor6              = Interval 6 8

minor7              :: Interval
minor7              = Interval 7 10

minor9              :: Interval
minor9              = octave ^+^ minor2

minor10             :: Interval
minor10             = octave ^+^ minor3

-- perfect
perfect1            :: Interval 
perfect1            = Interval 1 0

perfect4            :: Interval 
perfect4            = Interval 4 5

perfect5            :: Interval 
perfect5            = Interval 5 7

perfect8            :: Interval 
perfect8            = Interval 8 12

perfect11           :: Interval
perfect11           = octave ^+^ perfect4

perfect12           :: Interval
perfect12           = octave ^+^ perfect5

-- augmented
augmented2          :: Interval
augmented2          = Interval 2 3

augmented3          :: Interval
augmented3          = Interval 3 5

augmented4          :: Interval
augmented4          = Interval 4 6

augmented5          :: Interval
augmented5          = Interval 5 8

augmented6          :: Interval
augmented6          = Interval 6 10

augmented7          :: Interval
augmented7          = Interval 7 12

augmented8          :: Interval
augmented8          = Interval 8 13

augmented9          :: Interval
augmented9          = octave ^+^ augmented2

augmented11         :: Interval
augmented11         = octave ^+^ augmented4 

augmented12         :: Interval
augmented12         = octave ^+^ augmented5

augmented13         :: Interval
augmented13         = octave ^+^ augmented6 


-- diminished
diminished2         :: Interval
diminished2         = Interval 2 0

diminished3         :: Interval
diminished3         = Interval 3 2

diminished4         :: Interval
diminished4         = Interval 4 4

diminished5         :: Interval
diminished5         = Interval 5 6

diminished6         :: Interval
diminished6         = Interval 6 7

diminished7         :: Interval
diminished7         = Interval 7 9

diminished8         :: Interval
diminished8         = Interval 8 11

diminished9         :: Interval
diminished9         = octave ^+^ diminished2

diminished11        :: Interval
diminished11        = octave ^+^ diminished4 

diminished12        :: Interval
diminished12        = octave ^+^ diminished5

diminished13        :: Interval
diminished13        = octave ^+^ diminished6 


