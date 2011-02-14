{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Core.PtSize
-- Copyright   :  (c) Stephen Tetley 2010-2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Numeric type representing Point size (1/72 inch) which is 
-- PostScript and Wumpus-Core\'s internal unit size.
--
-- Other unit types (e.g. centimeter) should define an 
-- appropriate instance of FromPtSize.
-- 
-- Note - implicitly Wumpus-Core treats @Double@ as representing
-- PostScript point size / unit.
-- 
--------------------------------------------------------------------------------

module Wumpus.Core.PtSize
  ( 
  
  -- * Point size type
    PtSize
  , Pt
  
  -- * Extract (unscaled) PtSize as a Double 
  , ptSize

  -- * Conversion class
  , FromPtSize(..)

  ) where


import Wumpus.Core.Utils.Common

-- | Wrapped Double representing /Point size/ for font metrics 
-- etc.
-- 
newtype PtSize = PtSize 
          { ptSize :: Double  -- ^ Extract Point Size as a Double 
          } 
  deriving (Eq,Ord,Num,Floating,Fractional,Real,RealFrac,RealFloat)

instance Show PtSize where
  showsPrec p d = showsPrec p (ptSize d)


-- | Alias for PtSize.
-- 
-- The original name 'PtSize' is perhaps rather cumbersome, though
-- it does dis-ambiguate from geometrics Points. An alias is
-- convenient.
--
type Pt = PtSize


-- | Convert the value of PtSize scaling accordingly.
--
-- Note - the Double instance perfoms no scaling, this
-- is because internally Wumpus-Core works in points.
-- 
class Num u => FromPtSize u where
  fromPtSize :: PtSize -> u

instance FromPtSize Double where
  fromPtSize = ptSize


instance PSUnit PtSize where
  toDouble = ptSize



