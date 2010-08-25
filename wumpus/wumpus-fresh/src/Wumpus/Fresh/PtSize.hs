{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Fresh.PtSize
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Fresh PtSize
-- 
--------------------------------------------------------------------------------

module Wumpus.Fresh.PtSize
  ( 
  
  -- * Point size type
    PtSize
  
  -- * Extract (unscaled) PtSize as a Double 
  , ptSize

  -- * Conversion class
  , FromPtSize(..)

  ) where


-- | Wumpus-Core 
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



