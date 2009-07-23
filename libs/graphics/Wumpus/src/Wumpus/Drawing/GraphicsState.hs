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


module Wumpus.Drawing.GraphicsState where


data LineCap = CapButt | CapRound | CapSquare
  deriving (Enum,Eq,Show)

data JoinStyle = JoinMiter | JoinRound | JoinBevel
  deriving (Enum,Eq,Show)

data DashPattern = Solid | Dash Int [Int]
  deriving (Eq,Show)

data Pen = Pen { 
      lineWidth     :: Double,
      lineCap       :: LineCap,
      lineJoin      :: JoinStyle,
      dashPattern   :: DashPattern 
    }
  deriving (Eq,Show)



newPen :: Pen
newPen = Pen { lineWidth    = 1.0,          
               lineCap      = CapButt, 
               lineJoin     = JoinMiter,    
               dashPattern  = Solid }


data Font = Font { 
      fontName    :: String,
      unitSize    :: Double
    }
  deriving (Eq,Show)

helvetica :: Double -> Font
helvetica us = Font "Helvetica" us

timesRoman :: Double -> Font
timesRoman us = Font "Times-Roman" us
