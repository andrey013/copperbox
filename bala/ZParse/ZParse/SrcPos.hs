{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZParse.SrcPos
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- SrcPos - ...
--
--------------------------------------------------------------------------------

module ZParse.SrcPos where



data SrcPos = SrcPos { _srcpos_line :: Int, _srcpos_column :: Int }
  deriving (Eq,Show)

initialPos :: SrcPos   
initialPos = SrcPos 1 1


class SourcePosition a where
  getSrcPos  :: a -> SrcPos
  setSrcPos  :: SrcPos -> a -> a



 
   