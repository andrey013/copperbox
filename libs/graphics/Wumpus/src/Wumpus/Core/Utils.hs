{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Core.Utils
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Utility functions
--
--------------------------------------------------------------------------------


module Wumpus.Core.Utils
  ( 


  -- * Three values  
    max3
  , min3
  , med3

  -- * truncate / print a double
  , dtrunc
  , roundup

  , mkTimeStamp

  , sequenceA
  , (<:>) 

  ) where

import Control.Applicative
import Data.List ( intersperse )
import System.Time 




-- | max of 3
max3 :: Ord a => a -> a -> a -> a
max3 a b c = max (max a b) c

-- | min of 3
min3 :: Ord a => a -> a -> a -> a
min3 a b c = min (min a b) c


-- | median of 3
med3 :: Ord a => a -> a -> a -> a
med3 a b c = if c <= x then x else if c > y then y else c
  where 
    (x,y)                 = order a b
    order p q | p <= q    = (p,q)
              | otherwise = (q,p)



dtrunc :: Double -> String
dtrunc d | abs d < 0.0001  = "0.0"
         | d < 0.0           = '-' :  show (abs tx)
         | otherwise         = show tx
  where
    tx :: Double
    tx = (realToFrac (roundi (d*1000000.0))) / 1000000.0
 

roundup :: Double -> String
roundup = show . ceilingi


-- Avoid those annoying 'Defaulting ...' warnings...
roundi :: RealFrac a => a -> Integer
roundi = round

ceilingi :: RealFrac a => a -> Integer
ceilingi = ceiling



mkTimeStamp :: IO String
mkTimeStamp = getClockTime >>= toCalendarTime >>= return . format
  where
    format t = mkTime t ++ " " ++ mkDate t
    mkTime = concat . intersperse ":" . sequenceA tfuns
    mkDate = concat . intersperse " " . sequenceA dfuns
    tfuns  = [ pad2 . ctHour, pad2 . ctMin, pad2 . ctSec ]
    dfuns  = [ show . ctDay, show . ctMonth, show . ctYear ]
    pad2 i | i < 10    = '0' : show i
           | otherwise = show i  

-- | Applicative version of (monadic) 'sequence'.
-- Because we use MonadLib we don't want to bring in 
-- Control.Monad.Instances ()
sequenceA :: Applicative f => [f a] -> f [a]
sequenceA = foldr (<:>) (pure []) 


-- | Applicative 'cons'.
infixr 6 <:>
(<:>) :: Applicative f => f a -> f [a] -> f [a]
(<:>) a b = (:) <$> a <*> b