{-# LANGUAGE TypeSynonymInstances       #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Duration
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Duration modelled as Rational
--
--------------------------------------------------------------------------------

module Bala.Duration 
  ( 
  -- * Data type
    Duration

  -- * Extract duration
  , RationalDuration(..)

  -- * Operations
  , dotn
  
  , split2 
  , split3 
  
  -- * Named instances
  , longa

  -- ** (American)
  , double_whole
  , whole
  , half
  , quarter
  , eighth
  , sixteenth
  , thirty_second
  , sixty_fourth
  , one_hundred_twenty_eighth
    
  -- ** (English)
  , breve
  , semibreve
  , minim
  , crochet
  , quaver
  , semiquaver
  , demisemiquaver
  , hemidemisemiquaver
  , semihemidemisemiquaver
    
  -- ** (Shorthand)
  , wn
  , hn
  , qn
  , en
  , sn
  , ttn
  , sfn  

  ) where

import Data.Ratio

type Duration = Rational

--------------------------------------------------------------------------------



class RationalDuration a where
  rationalDuration :: a -> Rational

instance RationalDuration Duration where
  rationalDuration = id

--------------------------------------------------------------------------------

-- | Dot a duration.
dotn :: Int -> Duration -> Duration
dotn i d | i < 1      = d
         | otherwise  = d + step i (d /2)
  where
    step 0 _ = 0
    step n r = r + step (n-1) (r / 2)


-- | Partition a duration into two parts.
split2 :: (Integer,Integer) -> Duration -> (Duration,Duration)
split2 (a,b) r = (r*(a%z), r*(b%z)) where z = a+b

-- | Partition a duration into three parts.
split3 :: (Integer,Integer,Integer) -> Duration -> (Duration,Duration,Duration)
split3 (a,b,c) r = (r*(a%z), r*(b%z), r*(c%z)) where z = a+b+c

      
--------------------------------------------------------------------------------
-- Named elements


longa                       :: Duration
longa                       = 4%1

-- American naming.
double_whole                :: Duration
double_whole                = 2%1

whole                       :: Duration
whole                       = 1%1

half                        :: Duration
half                        = 1%2

quarter                     :: Duration
quarter                     = 1%4

eighth                      :: Duration
eighth                      = 1%8

sixteenth                   :: Duration
sixteenth                   = 1%16

thirty_second               :: Duration
thirty_second               = 1%32

sixty_fourth                :: Duration
sixty_fourth                = 1%64

one_hundred_twenty_eighth   :: Duration
one_hundred_twenty_eighth   = 1%128

-- English naming.
breve                       :: Duration
breve                       = double_whole

semibreve                   :: Duration
semibreve                   = whole

minim                       :: Duration
minim                       = half

crochet                     :: Duration
crochet                     = quarter

quaver                      :: Duration 
quaver                      = eighth

semiquaver                  :: Duration 
semiquaver                  = sixteenth

demisemiquaver              :: Duration
demisemiquaver              = thirty_second

hemidemisemiquaver          :: Duration 
hemidemisemiquaver          = sixty_fourth

semihemidemisemiquaver      :: Duration
semihemidemisemiquaver      = one_hundred_twenty_eighth

-- Shorthands
wn    :: Duration
wn    = whole

hn    :: Duration
hn    = half

qn    :: Duration
qn    = quarter

en    :: Duration
en    = eighth

sn    :: Duration
sn    = sixteenth

ttn   :: Duration
ttn   = thirty_second
     
sfn   :: Duration
sfn   = sixty_fourth


