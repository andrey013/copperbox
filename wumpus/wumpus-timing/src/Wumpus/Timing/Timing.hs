{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Timing.Timing
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC 
--
-- Timing diagram data type
--
--------------------------------------------------------------------------------

module Wumpus.Timing.Timing
  (

    SignalLevel(..)    
  , SignalGraphic
  , line
  , metastasis    
  , space
  , unknown
  , glitch

  , evalSignal

  ) where


import Wumpus.Core                      -- package: wumpus-core
import Wumpus.Basic.Utils.HList         -- package: wumpus-basic


type HalfCount = Int

-- Too much work to identify transitions on these data types
--

data SignalGraphic = Line        HalfCount SignalLevel LineTrans DRGB
                   | Data        HalfCount DataClass
                   | Metastasis  HalfCount
                   | Space       HalfCount
                   | Glitch
  deriving (Eq,Show)

data SignalLevel = Hi | Mid | Lo
  deriving (Eq,Show) 

data LineTrans = Clock | Slope
  deriving (Eq,Show)

data DataClass = Unknown DRGB | DataMsg (Maybe String)
  deriving (Eq,Show)

type Signal = (H SignalGraphic, Maybe (SignalGraphic))

type SignalF = Signal -> Signal

mbCons :: Maybe a -> H a -> H a
mbCons mb h = maybe h (`consH` h) mb


line :: SignalLevel -> DRGB -> HalfCount -> SignalF
line lvl rgb hc (h, Just (Line n sl lt lc))
        | lvl == sl && rgb == lc    = (h, Just $ Line (n+hc) sl lt lc)
line lvl rgb hc (h, tip)            = (mbCons tip h, Just $ Line hc lvl Slope rgb)

metastasis :: HalfCount -> SignalF
metastasis hc (h, Just (Metastasis n))  = (h, Just $ Metastasis (n+hc))
metastasis hc (h, tip)                  = (mbCons tip h, Just $ Metastasis hc) 

space :: HalfCount -> SignalF
space hc (h, Just (Space n))    = (h, Just $ Space (n+hc))
space hc (h, tip)               = (mbCons tip h, Just $ Space hc)

unknown :: DRGB -> HalfCount -> SignalF
unknown rgb hc (h, Just (Data n (Unknown uc)))
        | rgb == uc             = (h, Just $ Data (n+hc) (Unknown uc))
unknown rgb hc (h, tip)         = (mbCons tip h, Just $ Data hc (Unknown rgb))


glitch :: SignalF 
glitch (h, tip)                 = (mbCons tip h, Just Glitch)

signalZero :: Signal
signalZero = (id, Nothing)


evalSignal :: SignalF -> [SignalGraphic]
evalSignal signalF = let (h,tip) = signalF signalZero in mbCons tip h $ []


{-
drawSignals :: [SignalGraphic] -> DGraphic
drawSignals
-}

-- lineTransition :: SignalLevel -> LineTrans -> SignalLevel -> (DPoint2