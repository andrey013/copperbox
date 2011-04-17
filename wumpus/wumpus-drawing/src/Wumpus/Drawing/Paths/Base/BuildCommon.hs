{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Drawing.Paths.Base.BuildCommon
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Common data types for the monadic builders.
-- 
--------------------------------------------------------------------------------

module Wumpus.Drawing.Paths.Base.BuildCommon
  ( 

    BuildLog
  , PathEnd(..)
  , Vamp(..)
  
  , extractTrace
  , addInsert
  , addPen

  , insert1
  , pen1
     
  ) where

import Wumpus.Drawing.Paths.Base.RelPath ( RelPath )

import Wumpus.Basic.Kernel                      -- package: wumpus-basic
import Wumpus.Basic.Utils.HList

import Wumpus.Core                              -- package: wumpus-core

import Data.Monoid

-- | Monadic builders allow both inserts (patterned after TikZ)
-- which are decorations along the path and sub paths that are 
-- stroked and possibly closed.
--
data BuildLog a  = Log { insert_trace    :: H a
                       , pen_trace       :: H a
                       }
                 | NoLog

data PathEnd = PATH_CLOSED | PATH_OPEN
  deriving (Eq,Show)


data Vamp u = Vamp 
      { vamp_move_span  :: Vec2 u
      , vamp_move_start :: Vec2 u
      , vamp_dc_update  :: DrawingContextF
      , vamp_deco_path  :: RelPath u
      , vamp_path_end   :: PathEnd
      }

type instance DUnit (Vamp u) = u

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
extractTrace :: OPlus a => a -> BuildLog a -> (a,a)
extractTrace zero NoLog           = (zero,zero)
extractTrace zero (Log ins pen)   = (pent,inst)
  where
    pent = altconcat zero $ toListH $ pen 
    inst = altconcat zero $ toListH $ ins


-- | Add an /insert/.
--
-- This is a decoration drawn at the /current point/ during path
-- building.
-- 
addInsert :: a -> BuildLog a -> BuildLog a
addInsert a NoLog = Log { insert_trace = wrapH a, pen_trace = emptyH }
addInsert a s@(Log { insert_trace=i }) = s { insert_trace = i `snocH` a }


-- | Build a trace from an /insert/.
--
-- This is a decoration drawn at the /current point/ during path
-- building.
-- 
insert1 :: a -> BuildLog a
insert1 a = Log { insert_trace = wrapH a, pen_trace = emptyH }

-- | Add a /pen/ drawing. 
--
-- This is a stroked path sub-path formed prior to a @moveto@ 
-- instruction. Moveto is synonymous with /pen up/ - at a pen up
-- and sub-path is drawn as a trace.
-- 
addPen :: a -> BuildLog a -> BuildLog a
addPen a NoLog = Log { insert_trace = wrapH a, pen_trace = emptyH }
addPen a s@(Log { pen_trace=i }) = s { pen_trace = i `snocH` a }


-- | Build a trace from an /insert/.
--
-- This is a decoration drawn at the /current point/ during path
-- building.
-- 
pen1 :: a -> BuildLog a
pen1 a = Log { insert_trace = emptyH, pen_trace = wrapH a }



