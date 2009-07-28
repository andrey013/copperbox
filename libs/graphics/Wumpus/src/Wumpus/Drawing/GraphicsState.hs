{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Drawing.GraphicsState
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Data types form Graphics State
--
--------------------------------------------------------------------------------


module Wumpus.Drawing.GraphicsState 
  (
  -- * Graphics state types
    PsEnv(..)

  , LineCap(..)
  , JoinStyle(..)
  , DashPattern(..)
  , Pen(..)
  , newPen

  , Font(..)
   
  )where

import Wumpus.Core.Colour

data PsEnv = PsEnv { 
       cPen         :: Pen,
       cFont        :: Font,
       cColour      :: DRGB
    }
  deriving (Eq,Show)


data LineCap = CapButt | CapRound | CapSquare
  deriving (Enum,Eq,Show)

data JoinStyle = JoinMiter | JoinRound | JoinBevel
  deriving (Enum,Eq,Show)

data DashPattern = Solid | Dash Int [Int]
  deriving (Eq,Show)

data Pen = Pen { 
      lineWidth     :: Double,
      miterLimit    :: Double,
      lineCap       :: LineCap,
      lineJoin      :: JoinStyle,
      dashPattern   :: DashPattern 
    }
  deriving (Eq,Show)



newPen :: Pen
newPen = Pen { lineWidth    = 1.0,          
               miterLimit   = 10.0,
               lineCap      = CapButt, 
               lineJoin     = JoinMiter,    
               dashPattern  = Solid }


data Font = Font { 
      fontName    :: String,
      unitSize    :: Int
    }
  deriving (Eq,Show)
