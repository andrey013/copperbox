{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Core.PtSize
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Core PtSize
-- 
--------------------------------------------------------------------------------

module Wumpus.Core.PtSize
  ( 
  
  -- * Point size type
    PtSize
  
  -- * Extract (unscaled) PtSize as a Double 
  , ptSize

  -- * Conversion class
  , FromPtSize(..)

  ) where


-- | Wrapped Double representing /Point size/ for font metrics 
-- etc.
-- 
newtype PtSize = PtSize { ptSize :: Double } 
  deriving (Eq,Ord,Num,Floating,Fractional)

instance Show PtSize where
  showsPrec p d = showsPrec p (ptSize d)


-- | Convert the value of PtSize scaling accordingly.
--
-- Note - the Double instance perfoms no scaling, this
-- is because internally Wumpus-Core works in points.
-- 
class FromPtSize u where
  fromPtSize :: PtSize -> u

instance FromPtSize Double where
  fromPtSize = ptSize



