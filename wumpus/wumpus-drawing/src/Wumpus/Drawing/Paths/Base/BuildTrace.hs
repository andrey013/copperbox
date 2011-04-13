{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Drawing.Paths.Base.BuildTrace
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Common tracing data type for the monadic builders
-- 
--------------------------------------------------------------------------------

module Wumpus.Drawing.Paths.Base.BuildTrace
  ( 

    BuildLog
  , SubPathEnd(..)
  
  , extractTrace
  , addInsert
  , addPen
     
  ) where



import Wumpus.Basic.Kernel                      -- package: wumpus-basic
import Wumpus.Basic.Utils.HList


import Data.Monoid

-- Note - connectors and paths are quite different things.
--
-- Connectors always know start and end points, a path is built 
-- from a start point.
--


data BuildLog a  = Log { insert_trace    :: H a
                       , pen_trace       :: H a
                       }
                 | NoLog

data SubPathEnd = SPE_Closed | SPE_Open
  deriving (Eq,Show)

--------------------------------------------------------------------------------
-- instances

instance Monoid (BuildLog a) where
  mempty                        = NoLog
  NoLog     `mappend` b         = b
  a         `mappend` NoLog     = a
  Log li lp `mappend` Log ri rp = Log (li `appendH` ri) (lp `appendH` rp)




-- | Extract the pen and insert drawings from a 'BuildLog'.
--
-- Any sub-paths traced by the pen are drawn at the back in the 
-- Z-order.
--
extractTrace :: OPlus a => a -> BuildLog a -> a
extractTrace zero NoLog           = zero
extractTrace zero (Log ins pen)   = altconcat zero xs 
  where
    xs = toListH $ pen `appendH` ins


-- | Add an /insert/.
--
-- This is a decoration drawn at the /current point/ during path
-- building.
-- 
addInsert :: a -> BuildLog a -> BuildLog a
addInsert a NoLog = Log { insert_trace = wrapH a, pen_trace = emptyH }
addInsert a s@(Log { insert_trace=i }) = s { insert_trace = i `snocH` a }


-- | Add a /pen/ drawing. 
--
-- This is a stroked path sub-path formed prior to a @moveto@ 
-- instruction. Moveto is synonymous with /pen up/ - at a pen up
-- and sub-path is drawn as a trace.
-- 
addPen :: a -> BuildLog a -> BuildLog a
addPen a NoLog = Log { insert_trace = wrapH a, pen_trace = emptyH }
addPen a s@(Log { pen_trace=i }) = s { pen_trace = i `snocH` a }




