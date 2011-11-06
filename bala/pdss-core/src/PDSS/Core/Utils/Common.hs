{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Core.Utils.Common
-- Copyright   :  (c) Stephen Tetley 2009-2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Utility functions and a Hughes list.
--
--------------------------------------------------------------------------------


module PDSS.Core.Utils.Common
  ( 

  -- * Truncate / print a double
    dtruncFmt

  , truncateDouble

  ) where


import qualified PDSS.Core.Utils.FormatCombinators as Fmt



--------------------------------------------------------------------------------
-- PS Unit

-- 
dtruncFmt :: Double -> Fmt.Doc
dtruncFmt = Fmt.text . truncateDouble


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

