{-# LANGUAGE FlexibleInstances          #-}
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
-- Utility functions and a OneList (non-empty list) data type.
--
--------------------------------------------------------------------------------


module Wumpus.Core.Utils
  ( 

  -- * Component-wise min and max
    CMinMax(..)
  , within

  -- * Three values  
  , max3
  , min3
  , med3


  -- * Truncate / print a double
  , PSUnit(..)
  , truncateDouble
  , roundup

  , clamp
  , ramp
  , ramp255

  -- * PostScript timetmap
  , mkTimeStamp

  -- * Pretty printers for strings  
  , parens
  , hsep
  , commasep
  , tupled

  -- * Extras  
  , sequenceA
  , (<:>) 


  

  -- * One type - non-empty list type
  , OneList(..)
  , mkList2
  , onesmapM_
  , toListWith
  , toListWithM
  , fromListErr

  ) where



import Control.Applicative
import Control.Monad ( ap )
import Data.List ( intersperse )
import Data.Ratio
import System.Time 




--------------------------------------------------------------------------------

-- | /Component-wise/ min and max. 
-- Standard 'min' and 'max' via Ord are defined lexographically
-- on pairs, e.g.:
-- 
-- > min (1,2) (2,1) = (1,2)
-- 
-- For certain geometrical objects (Points!) we want the 
-- (constructed-) componentwise min and max, e.g:
--
-- > cmin (1,2) (2,1) = (1,1) 
-- > cmax (1,2) (2,1) = (2,2)
-- 

class CMinMax a where
  cmin :: a -> a -> a
  cmax :: a -> a -> a



instance (Ord a, Ord b) => CMinMax (a,b) where
  cmin (x,y) (x',y') = (min x x', min y y')
  cmax (x,y) (x',y') = (max x x', max y y')


-- | Test whether a is within opper and lower.
within :: Eq a => CMinMax a => a -> a -> a -> Bool
within a lower upper = (cmin a lower) == lower && (cmax a upper) == upper


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


--------------------------------------------------------------------------------
-- PS Unit

class Num a => PSUnit a where
  toDouble :: a -> Double
  dtrunc   :: a -> String
  
  dtrunc = truncateDouble . toDouble

instance PSUnit Double where
  toDouble = id
  dtrunc   = truncateDouble

instance PSUnit Float where
  toDouble = realToFrac

instance PSUnit (Ratio Integer) where
  toDouble = realToFrac

instance PSUnit (Ratio Int) where
  toDouble = realToFrac


-- | Truncate the printed decimal representation of a Double.
-- The is prefered to 'showFFloat' from Numeric as it produces
-- shorter representations where appropriate.
-- 
-- 0.000000000 becomes 0.0 rather than however many digs are 
-- specified.
--  
truncateDouble :: Double -> String
truncateDouble d | abs d < 0.0001  = "0.0"
                 | d < 0.0         = '-' :  show (abs tx)
                 | otherwise       = show tx
  where
    tx :: Double
    tx = (realToFrac (roundi (d*1000000.0))) / 1000000.0

roundi :: RealFrac a => a -> Integer
roundi = round

-- | Take 'ceilingi' and show.
roundup :: Double -> String
roundup = show . ceilingi

-- Avoid those annoying 'Defaulting ...' warnings...
ceilingi :: RealFrac a => a -> Integer
ceilingi = ceiling


clamp :: Ord a => a -> a -> a -> a 
clamp a b x = max a (min b x)

ramp :: Double -> Double
ramp = clamp 0 1

-- | Scale a Double between 0.0 and 1.0 to be an Int between 0 
-- and 255.
ramp255 :: Double -> Int
ramp255 = clamp 0 255  . ceiling . (*255)



-- | Generate a time stamp for the output files. Note PostScript
-- does no interpretation of the time stamp, it is solely for 
-- information and so the representation is arbitrary.
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



-- | Enclose string in parens.
parens :: String -> String 
parens s = "(" ++ s  ++ ")"

-- | Separate with a space.
hsep :: [String] -> String
hsep = concat . intersperse " "

commasep :: [String] -> String
commasep = concat . intersperse ","

-- | @ (..., ...)@
tupled :: [String] -> String
tupled = parens . concat . intersperse ", " 



-- | Applicative version of (monadic) 'sequence'.
-- Because we use MonadLib we don't want to bring in 
-- Control.Monad.Instances ()
sequenceA :: Applicative f => [f a] -> f [a]
sequenceA = foldr (<:>) (pure []) 


-- | Applicative 'cons'.
infixr 6 <:>
(<:>) :: Applicative f => f a -> f [a] -> f [a]
(<:>) a b = (:) <$> a <*> b



--------------------------------------------------------------------------------

infixr 5 :+

data OneList a = One a | a :+ OneList a
  deriving (Eq)

instance Show a => Show (OneList a) where
  show = ('{':) . ($ []) . step where
     step (One a)   = shows a . showChar '}'
     step (a :+ xs) = shows a . showChar ',' . step xs


mkList2 :: a -> a -> OneList a
mkList2 a b = a :+ One b


onesmapM_ :: Monad m => (a -> m b) -> OneList a -> m ()
onesmapM_ f (One a)   = f a >> return ()
onesmapM_ f (a :+ xs) = f a >> onesmapM_ f xs

toListWith :: (a -> b) -> OneList a -> [b]
toListWith f (One a)   = [f a]
toListWith f (a :+ xs) = f a : toListWith f xs

toListWithM :: Monad m => (a -> m b) -> OneList a -> m [b]
toListWithM f (One a)   = return return `ap` f a
toListWithM f (a :+ xs) = return (:) `ap` f a `ap` toListWithM f xs


fromListErr :: String -> [a] -> OneList a
fromListErr msg []     = error msg
fromListErr _   [a]    = One a
fromListErr msg (a:xs) = a :+ fromListErr msg xs
