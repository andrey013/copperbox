{-# LANGUAGE FlexibleInstances          #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Core.Utils
-- Copyright   :  (c) Stephen Tetley 2009-2010
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


  -- * Three values  
    max3
  , min3
  , med3


  -- * Truncate / print a double
  , PSUnit(..)
  , truncateDouble
  , roundup


  , rescale
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

  -- * Hughes list
  , H
  , emptyH
  , toListH
  , snocH  


  -- * specs etc. from Data.Aviary
  , appro
  , oo
  , ooo
  , oooo
  , rap


  ) where



import Control.Applicative
import Data.List ( intersperse )
import Data.Ratio
import Data.Time



--------------------------------------------------------------------------------


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


-- rescale a (originally in the range amin to amax) within the 
-- the range bmin to bmax.
--
rescale :: Fractional a => (a,a) -> (a,a) -> a -> a
rescale (amin,amax) (bmin,bmax) a = 
    bmin + apos * (brange / arange)  
  where
    arange = amax - amin
    brange = bmax - bmin
    apos   = a - amin 

clamp :: Ord a => a -> a -> a -> a 
clamp amin amax x = max amin (min amax x)

ramp :: Double -> Double
ramp = clamp 0 1

-- | Scale a Double between 0.0 and 1.0 to be an Int between 0 
-- and 255.
ramp255 :: Double -> Int
ramp255 = clamp 0 255  . ceiling . (*255)


--------------------------------------------------------------------------------

-- | Generate a time stamp for the output files. Note PostScript
-- does no interpretation of the time stamp, it is solely for 
-- information and so the representation is arbitrary.

mkTimeStamp :: IO String
mkTimeStamp = getZonedTime >>= return . format . zonedTimeToLocalTime
  where
    format t  = mkTime t ++ " " ++ mkDate t
    mkTime = concat . intersperse ":" . sequenceA tfuns . localTimeOfDay
    mkDate = showGregorian . localDay
    tfuns  = [ pad2 . todHour, pad2 . todMin, pad2 . floori . todSec ]
    pad2 i | i < 10    = '0' : show i
           | otherwise = show i  

floori :: RealFrac a => a -> Int
floori = floor


--------------------------------------------------------------------------------

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
-- Hughes list

type H a = [a] -> [a]

emptyH :: H a
emptyH = id

toListH :: H a -> [a]
toListH = ($ [])

snocH :: H a -> a -> H a
snocH hl a = hl . (a:)

--------------------------------------------------------------------------------

-- | A variant of the @D2@ or dovekie combinator - the argument
-- order has been changed to be more satisfying for Haskellers:
--
-- > (appro comb f g) x y
--
-- > (f x) `comb` (g y)
-- 
-- @on@ from Data.Function is similar but less general, where 
-- the two intermediate results are formed by applying the same 
-- function to the supplied arguments:
--
-- > on = (appro comb f f)
--
appro :: (c -> d -> e) -> (a -> c) -> (b -> d) -> a -> b -> e
appro comb f g x y = comb (f x) (g y) 


--------------------------------------------------------------------------------
-- Specs - blackbird, bunting, ...

-- Alleviate your composing-sectioning mania with specs!
--
-- E.g.:
-- (abs .) . (*) ==> abs `oo` (*)
--
-- The family name /specs/ (glasses, specs, lunettes) is a 
-- visual pun when infix directives @`oo`@ are included. The 
-- @o@\'s of individual combinators are a fraternal nod to 
-- Clean and ML who use @o@ as function composition. Naturally
-- we don\'t defined @o@ here and waste a good variable on a 
-- redundant combinator.

-- | Compose an arity 1 function with an arity 2 function.
-- B1 - blackbird
oo :: (c -> d) -> (a -> b -> c) -> a -> b -> d
oo f g = (f .) . g

-- | Compose an arity 1 function with an arity 3 function.
-- B2 - bunting
ooo :: (d -> e) -> (a -> b -> c -> d) -> a -> b -> c -> e
ooo f g = ((f .) .) . g

-- | Compose an arity 1 function with an arity 4 function.
oooo :: (e -> f) -> (a -> b -> c -> d -> e) -> a -> b -> c -> d -> f
oooo f g = (((f .) .) .) . g  



-- ($) reversed - aka T - aka (#)
--
infixl 1 `rap`
rap :: a -> (a -> b) -> b
rap a f = f a