{-# LANGUAGE FlexibleInstances          #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Fresh.Utils
-- Copyright   :  (c) Stephen Tetley 2009-2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Fresh utils.
--
--------------------------------------------------------------------------------


module Wumpus.Fresh.Utils
  ( 

  -- | Opt - maybe strict in Many
    Opt(..)
  , some

  -- | Conditional application
  , applyIf

  , rescale

  -- * Truncate / print a double
  , PSUnit(..)
  , dtruncFmt

  , truncateDouble
  , roundup
  


  -- * PostScript time stamp
  , psTimeStamp

  -- * Hughes list
  , H
  , emptyH
  , wrapH
  , consH
  , snocH  
  , appendH
  , toListH

  ) where


import qualified Wumpus.Fresh.FormatCombinators as Fmt


import Data.Ratio
import Data.Time

data Opt a = None | Some !a 
  deriving (Eq,Show)

some :: a -> Opt a -> a
some dflt None     = dflt
some _    (Some a) = a 

applyIf :: Bool -> (a -> a) -> a -> a
applyIf cond fn a = if cond then fn a else a


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

dtruncFmt :: PSUnit a => a -> Fmt.Doc
dtruncFmt = Fmt.text . dtrunc


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




--------------------------------------------------------------------------------


-- | To be used with getZonedTime

psTimeStamp :: ZonedTime -> ShowS
psTimeStamp zt = localTimeS . showChar ' ' . localDayS
  where
    local_tim   = zonedTimeToLocalTime zt
    localTimeS  = timeOfDay  $ localTimeOfDay $ local_tim
    localDayS   = showString $ showGregorian  $ localDay local_tim

timeOfDay :: TimeOfDay -> ShowS
timeOfDay t = 
    fn todHour . showChar ':' . fn todMin . showChar ':' . fn (floori . todSec)
  where
    fn f = pad2 (f t) 


pad2 :: Int -> ShowS
pad2 i | i < 10    = ('0':) . shows i
       | otherwise = shows i  


floori :: RealFrac a => a -> Int
floori = floor



--------------------------------------------------------------------------------
-- Hughes list

type H a = [a] -> [a]

emptyH :: H a
emptyH = id


wrapH :: a -> H a
wrapH a = consH a id 

consH :: a -> H a -> H a
consH a f = (a:) . f

snocH :: H a -> a -> H a
snocH hl a = hl . (a:)

appendH :: H a -> H a -> H a
appendH f g = f . g

toListH :: H a -> [a]
toListH = ($ [])


